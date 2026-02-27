#![allow(dead_code)]

use std::collections::HashMap;

/// Represents any data type that can flow through the node editor's connections.
#[derive(Debug, Clone, PartialEq, Default)]
pub enum DynamicValue {
    #[default]
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Bytes(Vec<u8>),
    List(Vec<DynamicValue>),
    Object(HashMap<String, DynamicValue>),
    Error(String),
    Timestamp(u64),
}

/// The environment in which a flow executes.
/// This holds variables created by the user's nodes and tracks UI changes.
#[derive(Debug, Default, Clone)]
pub struct ExecutionContext {
    /// Variables stored in memory during or across flow executions.
    pub variables: HashMap<String, DynamicValue>,
    /// The ID or name of the currently active view.
    pub current_view: String,
    /// Tracks if a modal should be visible.
    pub active_modal: Option<String>,
}

/// Represents a single visual node in your flow editor.
#[derive(Debug, Clone)]
pub struct FlowNode {
    pub id: String,
    pub action: ActionType,
    /// The ID of the next node to execute in a linear flow.
    pub next_node: Option<String>,
}

/// The specific operation a node performs.
#[derive(Debug, Clone)]
pub enum ActionType {
    // === UI & View Control ===
    /// Swaps the current view.
    NavigateToView { target_view: String },
    /// Opens a modal overlay.
    ShowModal { modal_id: String },

    // === Logic & Branching ===
    /// A node with two possible outputs based on a boolean condition.
    Branch {
        condition_variable: String,
        true_next_node: Option<String>,
        false_next_node: Option<String>,
    },

    /// Evaluates a variable against multiple possible values.
    Match {
        match_variable: String,
        /// Ordered list of tuples: (Value to match against, Next Node ID).
        arms: Vec<(DynamicValue, String)>,
        /// Fallback node if no arm matches.
        default_next_node: Option<String>,
    },

    // === Variable Manipulation ===
    /// Hardcodes a value into the context.
    SetVariable { key: String, value: DynamicValue },
    /// A simple math operation (A + B).
    Add {
        var_a: String,
        var_b: String,
        result_key: String,
    },

    // === External Actions ===
    /// Placeholder for network requests.
    ApiRequest {
        url: String,
        method: String,
        result_key: String,
    },
}

/// Represents a complete user-defined flow.
#[derive(Debug, Clone)]
pub struct FlowGraph {
    pub id: String,
    pub nodes: HashMap<String, FlowNode>,
    pub start_node_id: Option<String>,
}

/// Executes a flow graph from start to finish, mutating the context along the way.
pub fn execute_flow(graph: &FlowGraph, context: &mut ExecutionContext) -> Result<(), String> {
    let mut current_node_id = graph.start_node_id.clone();

    // Prevent infinite loops in poorly designed user flows.
    let mut execution_limit = 1000_u32;

    while let Some(node_id) = current_node_id {
        if execution_limit == 0 {
            return Err("Execution limit reached. Possible infinite loop in flow.".to_owned());
        }
        execution_limit -= 1;

        let node = graph
            .nodes
            .get(&node_id)
            .ok_or_else(|| format!("Node {node_id} not found"))?;

        // Default to the standard linear `next_node`.
        let mut next = node.next_node.clone();

        match &node.action {
            ActionType::NavigateToView { target_view } => {
                context.current_view = target_view.clone();
            }
            ActionType::ShowModal { modal_id } => {
                context.active_modal = Some(modal_id.clone());
            }
            ActionType::SetVariable { key, value } => {
                context.variables.insert(key.clone(), value.clone());
            }
            ActionType::Branch {
                condition_variable,
                true_next_node,
                false_next_node,
            } => {
                let condition = match context.variables.get(condition_variable) {
                    Some(DynamicValue::Bool(value)) => *value,
                    _ => false,
                };

                next = if condition {
                    true_next_node.clone()
                } else {
                    false_next_node.clone()
                };
            }
            ActionType::Match {
                match_variable,
                arms,
                default_next_node,
            } => {
                let target_value = context
                    .variables
                    .get(match_variable)
                    .unwrap_or(&DynamicValue::Null);
                let matched_arm = arms.iter().find(|(value, _)| value == target_value);

                next = match matched_arm {
                    Some((_, next_node_id)) => Some(next_node_id.clone()),
                    None => default_next_node.clone(),
                };
            }
            ActionType::Add {
                var_a,
                var_b,
                result_key,
            } => {
                let a = context
                    .variables
                    .get(var_a)
                    .unwrap_or(&DynamicValue::Integer(0));
                let b = context
                    .variables
                    .get(var_b)
                    .unwrap_or(&DynamicValue::Integer(0));

                if let (DynamicValue::Integer(int_a), DynamicValue::Integer(int_b)) = (a, b) {
                    context
                        .variables
                        .insert(result_key.clone(), DynamicValue::Integer(int_a + int_b));
                } else {
                    return Err("Cannot add non-integer types".to_owned());
                }
            }
            ActionType::ApiRequest { .. } => {
                // In an interactive app, this would be an async command/task.
                println!("Triggered API request node");
            }
        }

        current_node_id = next;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node(id: &str, action: ActionType, next_node: Option<&str>) -> (String, FlowNode) {
        (
            id.to_owned(),
            FlowNode {
                id: id.to_owned(),
                action,
                next_node: next_node.map(ToOwned::to_owned),
            },
        )
    }

    #[test]
    fn executes_linear_nodes() {
        let mut graph = FlowGraph {
            id: "flow".to_owned(),
            nodes: HashMap::new(),
            start_node_id: Some("set".to_owned()),
        };
        graph.nodes.extend([
            node(
                "set",
                ActionType::SetVariable {
                    key: "x".to_owned(),
                    value: DynamicValue::Integer(12),
                },
                Some("view"),
            ),
            node(
                "view",
                ActionType::NavigateToView {
                    target_view: "dashboard".to_owned(),
                },
                None,
            ),
        ]);

        let mut context = ExecutionContext::default();
        execute_flow(&graph, &mut context).expect("flow should execute");

        assert_eq!(context.current_view, "dashboard");
        assert_eq!(context.variables.get("x"), Some(&DynamicValue::Integer(12)));
    }

    #[test]
    fn executes_branch_logic() {
        let mut graph = FlowGraph {
            id: "branch".to_owned(),
            nodes: HashMap::new(),
            start_node_id: Some("branch".to_owned()),
        };
        graph.nodes.extend([
            node(
                "branch",
                ActionType::Branch {
                    condition_variable: "is_admin".to_owned(),
                    true_next_node: Some("admin".to_owned()),
                    false_next_node: Some("user".to_owned()),
                },
                None,
            ),
            node(
                "admin",
                ActionType::NavigateToView {
                    target_view: "admin".to_owned(),
                },
                None,
            ),
            node(
                "user",
                ActionType::NavigateToView {
                    target_view: "home".to_owned(),
                },
                None,
            ),
        ]);

        let mut context = ExecutionContext::default();
        context
            .variables
            .insert("is_admin".to_owned(), DynamicValue::Bool(true));
        execute_flow(&graph, &mut context).expect("branch should execute");

        assert_eq!(context.current_view, "admin");
    }

    #[test]
    fn executes_match_logic() {
        let mut graph = FlowGraph {
            id: "match".to_owned(),
            nodes: HashMap::new(),
            start_node_id: Some("match".to_owned()),
        };
        graph.nodes.extend([
            node(
                "match",
                ActionType::Match {
                    match_variable: "role".to_owned(),
                    arms: vec![
                        (DynamicValue::String("admin".to_owned()), "admin".to_owned()),
                        (DynamicValue::String("staff".to_owned()), "staff".to_owned()),
                    ],
                    default_next_node: Some("guest".to_owned()),
                },
                None,
            ),
            node(
                "admin",
                ActionType::ShowModal {
                    modal_id: "admin_tools".to_owned(),
                },
                None,
            ),
            node(
                "staff",
                ActionType::ShowModal {
                    modal_id: "staff_tools".to_owned(),
                },
                None,
            ),
            node(
                "guest",
                ActionType::ShowModal {
                    modal_id: "guest_info".to_owned(),
                },
                None,
            ),
        ]);

        let mut context = ExecutionContext::default();
        context
            .variables
            .insert("role".to_owned(), DynamicValue::String("staff".to_owned()));
        execute_flow(&graph, &mut context).expect("match should execute");

        assert_eq!(context.active_modal.as_deref(), Some("staff_tools"));
    }

    #[test]
    fn add_fails_with_non_integer_values() {
        let mut graph = FlowGraph {
            id: "math".to_owned(),
            nodes: HashMap::new(),
            start_node_id: Some("add".to_owned()),
        };
        graph.nodes.extend([node(
            "add",
            ActionType::Add {
                var_a: "a".to_owned(),
                var_b: "b".to_owned(),
                result_key: "sum".to_owned(),
            },
            None,
        )]);

        let mut context = ExecutionContext::default();
        context
            .variables
            .insert("a".to_owned(), DynamicValue::String("oops".to_owned()));
        context
            .variables
            .insert("b".to_owned(), DynamicValue::Integer(2));

        let result = execute_flow(&graph, &mut context);
        assert_eq!(result, Err("Cannot add non-integer types".to_owned()));
    }

    #[test]
    fn fails_on_infinite_loop_guard() {
        let mut graph = FlowGraph {
            id: "loop".to_owned(),
            nodes: HashMap::new(),
            start_node_id: Some("loop".to_owned()),
        };
        graph.nodes.extend([node(
            "loop",
            ActionType::SetVariable {
                key: "tick".to_owned(),
                value: DynamicValue::Integer(1),
            },
            Some("loop"),
        )]);

        let mut context = ExecutionContext::default();
        let result = execute_flow(&graph, &mut context);
        assert_eq!(
            result,
            Err("Execution limit reached. Possible infinite loop in flow.".to_owned())
        );
    }
}
