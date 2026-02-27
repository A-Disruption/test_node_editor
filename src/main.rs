mod flow_engine;
mod node_editor;

use std::collections::{HashMap, HashSet};

use flow_engine::{ActionType, DynamicValue, ExecutionContext, FlowGraph, FlowNode, execute_flow};
use iced::widget::{button, column, container, pin, row, stack, text};
use iced::{Color, Element, Fill, Task, Theme, border};
use node_editor::{
    ACTION_KIND_OPTIONS, ActionKind, ConnectionKind, NodeId, NodeKind, PortKind, PortSide,
    START_TRIGGER_OPTIONS, StartInputId, StartTrigger, VariableValueType,
};

fn main() -> iced::Result {
    iced::application(
        NodeEditorDemo::new,
        NodeEditorDemo::update,
        NodeEditorDemo::view,
    )
    .title("Node Editor - Flows Style")
    .theme(Theme::Dark)
    .window_size((1440.0, 900.0))
    .run()
}

struct NodeEditorDemo {
    editor: node_editor::EditorState,
    last_run: Option<Result<ExecutionContext, String>>,
    last_trigger: Option<StartTrigger>,
}

#[derive(Debug, Clone)]
enum Message {
    Editor(node_editor::EditorEvent),
    ContextAction(node_editor::ContextAction),
    VariableDrop(node_editor::VariableDropOption),
    StartTriggerSelected(NodeId, StartTrigger),
    ActionKindSelected(NodeId, ActionKind),
    RunFlow,
}

impl NodeEditorDemo {
    fn new() -> Self {
        Self {
            editor: node_editor::EditorState::new_demo(),
            last_run: None,
            last_trigger: None,
        }
    }

    fn update(&mut self, message: Message) -> Task<Message> {
        match message {
            Message::Editor(event) => self.editor.on_event(event),
            Message::ContextAction(action) => self.editor.apply_context_action(action),
            Message::VariableDrop(option) => self.editor.apply_variable_drop_option(option),
            Message::StartTriggerSelected(node_id, trigger) => {
                self.editor.apply_start_trigger(node_id, trigger)
            }
            Message::ActionKindSelected(node_id, action_kind) => {
                self.editor.apply_action_kind(node_id, action_kind)
            }
            Message::RunFlow => self.run_flow(),
        }

        Task::none()
    }

    fn view(&self) -> Element<'_, Message> {
        let header = container(
            row![
                text(
                    "Drag nodes and ports. Click trigger/action chips for dropdown options, drag start-input rows to reorder, click output panel rows to cycle targets, right-click blank space for free nodes."
                )
                .size(14),
                text(self.editor.status()).size(14),
                button("Run Flow").on_press(Message::RunFlow),
                text(self.run_status()).size(13),
            ]
            .spacing(14),
        )
        .width(Fill)
        .padding([10, 14])
        .style(|_| container::Style {
            background: Some(Color::from_rgb8(0x11, 0x17, 0x20).into()),
            border: border::rounded(0)
                .width(0)
                .color(Color::from_rgb8(0x11, 0x17, 0x20)),
            text_color: Some(Color::from_rgb8(0xD2, 0xDA, 0xE8)),
            ..container::Style::default()
        });

        let mut layers: Vec<Element<'_, Message>> =
            vec![self.editor.widget(Message::Editor).into()];

        if let Some(menu) = &self.editor.context_menu {
            layers.push(
                pin(self.context_menu())
                    .x(menu.position.x)
                    .y(menu.position.y)
                    .into(),
            );
        }

        if let Some(menu) = &self.editor.variable_drop_menu {
            layers.push(
                pin(self.variable_drop_menu())
                    .x(menu.position.x)
                    .y(menu.position.y)
                    .into(),
            );
        }

        if let Some(menu) = &self.editor.start_trigger_menu {
            layers.push(
                pin(self.start_trigger_menu(menu.node))
                    .x(menu.position.x)
                    .y(menu.position.y)
                    .into(),
            );
        }

        if let Some(menu) = &self.editor.action_kind_menu {
            layers.push(
                pin(self.action_kind_menu(menu.node))
                    .x(menu.position.x)
                    .y(menu.position.y)
                    .into(),
            );
        }

        let canvas = stack(layers).width(Fill).height(Fill);
        column![header, canvas].width(Fill).height(Fill).into()
    }

    fn context_menu(&self) -> Element<'_, Message> {
        let mut content = column![].spacing(8);

        for action in node_editor::CONTEXT_ACTIONS {
            content = content.push(
                button(action.label())
                    .width(Fill)
                    .on_press(Message::ContextAction(action)),
            );
        }

        container(content)
            .padding(10)
            .width(188)
            .style(|_| container::Style {
                background: Some(Color::from_rgb8(0x1E, 0x26, 0x33).into()),
                border: border::rounded(10)
                    .width(1)
                    .color(Color::from_rgb8(0x3E, 0x4B, 0x61)),
                text_color: Some(Color::from_rgb8(0xE2, 0xE9, 0xF7)),
                ..container::Style::default()
            })
            .into()
    }

    fn variable_drop_menu(&self) -> Element<'_, Message> {
        let mut content = column![].spacing(8);

        for option in self.editor.variable_drop_options() {
            content = content.push(
                button(option.label())
                    .width(Fill)
                    .on_press(Message::VariableDrop(*option)),
            );
        }

        container(content)
            .padding(10)
            .width(220)
            .style(|_| container::Style {
                background: Some(Color::from_rgb8(0x22, 0x2A, 0x37).into()),
                border: border::rounded(10)
                    .width(1)
                    .color(Color::from_rgb8(0x4A, 0x59, 0x71)),
                text_color: Some(Color::from_rgb8(0xF0, 0xF6, 0xFF)),
                ..container::Style::default()
            })
            .into()
    }

    fn start_trigger_menu(&self, node_id: NodeId) -> Element<'_, Message> {
        let mut content = column![].spacing(6);

        for trigger in START_TRIGGER_OPTIONS {
            content = content.push(
                button(trigger.label())
                    .width(Fill)
                    .on_press(Message::StartTriggerSelected(node_id, trigger)),
            );
        }

        container(content)
            .padding(10)
            .width(200)
            .style(|_| container::Style {
                background: Some(Color::from_rgb8(0x1F, 0x27, 0x35).into()),
                border: border::rounded(10)
                    .width(1)
                    .color(Color::from_rgb8(0x47, 0x56, 0x6F)),
                text_color: Some(Color::from_rgb8(0xE7, 0xEE, 0xFA)),
                ..container::Style::default()
            })
            .into()
    }

    fn action_kind_menu(&self, node_id: NodeId) -> Element<'_, Message> {
        let mut content = column![].spacing(6);

        for action_kind in ACTION_KIND_OPTIONS {
            content = content.push(
                button(action_kind.label())
                    .width(Fill)
                    .on_press(Message::ActionKindSelected(node_id, action_kind)),
            );
        }

        container(content)
            .padding(10)
            .width(190)
            .style(|_| container::Style {
                background: Some(Color::from_rgb8(0x1F, 0x27, 0x35).into()),
                border: border::rounded(10)
                    .width(1)
                    .color(Color::from_rgb8(0x47, 0x56, 0x6F)),
                text_color: Some(Color::from_rgb8(0xE7, 0xEE, 0xFA)),
                ..container::Style::default()
            })
            .into()
    }

    fn run_status(&self) -> String {
        match &self.last_run {
            None => "Ready".to_owned(),
            Some(Ok(context)) => {
                let modal = context.active_modal.as_deref().unwrap_or("None");
                let trigger = self
                    .last_trigger
                    .map(StartTrigger::label)
                    .unwrap_or("Unknown");
                format!(
                    "Run OK | trigger={trigger} | view={} | modal={} | vars={}",
                    context.current_view,
                    modal,
                    context.variables.len()
                )
            }
            Some(Err(error)) => format!("Run Error: {error}"),
        }
    }

    fn run_flow(&mut self) {
        self.last_run = Some(match self.build_graph() {
            Ok((graph, trigger)) => {
                self.last_trigger = Some(trigger);
                let mut context = ExecutionContext::default();
                context.variables.insert(
                    "trigger".to_owned(),
                    DynamicValue::String(trigger.label().to_owned()),
                );
                execute_flow(&graph, &mut context).map(|_| context)
            }
            Err(error) => Err(error),
        });
    }

    fn build_graph(&self) -> Result<(FlowGraph, StartTrigger), String> {
        let start_node = self
            .editor
            .nodes
            .iter()
            .find(|node| node.kind == NodeKind::Start)
            .ok_or_else(|| "No Start node found. Add a Start block first.".to_owned())?;

        let start_target = self.start_target(start_node).ok_or_else(|| {
            "Start node needs an output connection from one of its input rows.".to_owned()
        })?;

        let mut nodes = HashMap::new();
        for node in self
            .editor
            .nodes
            .iter()
            .filter(|node| node.kind == NodeKind::Action)
        {
            let outgoing = self.outgoing_action_targets(node.id, PortKind::Primary);
            let action = self.action_from_node(node, &outgoing);
            let next_node = if matches!(node.action_kind, ActionKind::Branch | ActionKind::Match) {
                None
            } else {
                outgoing.first().map(|target| target.to_string())
            };

            let id = node.id.to_string();
            nodes.insert(
                id.clone(),
                FlowNode {
                    id,
                    action,
                    next_node,
                },
            );
        }

        if nodes.is_empty() {
            return Err("No Action nodes found. Add at least one Action block.".to_owned());
        }

        if !nodes.contains_key(&start_target.to_string()) {
            return Err("Start output must target an Action node.".to_owned());
        }

        let graph = FlowGraph {
            id: "action_builder_flow".to_owned(),
            nodes,
            start_node_id: Some(start_target.to_string()),
        };

        ensure_dag(&graph)?;
        Ok((graph, start_node.start_trigger))
    }

    fn start_target(&self, start_node: &node_editor::Node) -> Option<NodeId> {
        let preferred = start_node
            .active_start_input
            .or_else(|| start_node.start_inputs.first().map(|input| input.id));

        if let Some(input_id) = preferred
            && let Some(target) = self.start_target_for_input(start_node.id, input_id)
        {
            return Some(target);
        }

        start_node
            .start_inputs
            .iter()
            .find_map(|input| self.start_target_for_input(start_node.id, input.id))
    }

    fn start_target_for_input(&self, start_node: NodeId, input_id: StartInputId) -> Option<NodeId> {
        self.editor.connections.iter().find_map(|connection| {
            (connection.kind == ConnectionKind::Flow
                && connection.from.node == start_node
                && connection.from.side == PortSide::Output
                && connection.from.kind == PortKind::StartInput(input_id)
                && connection.to.side == PortSide::Input
                && connection.to.kind == PortKind::Primary
                && self.is_action_node(connection.to.node))
            .then_some(connection.to.node)
        })
    }

    fn outgoing_action_targets(&self, from_node: NodeId, from_kind: PortKind) -> Vec<NodeId> {
        let mut seen = HashSet::new();

        self.editor
            .connections
            .iter()
            .filter_map(|connection| {
                (connection.kind == ConnectionKind::Flow
                    && connection.from.node == from_node
                    && connection.from.side == PortSide::Output
                    && connection.from.kind == from_kind
                    && connection.to.side == PortSide::Input
                    && connection.to.kind == PortKind::Primary
                    && self.is_action_node(connection.to.node))
                .then_some(connection.to.node)
            })
            .filter(|target| seen.insert(*target))
            .collect()
    }

    fn action_from_node(&self, node: &node_editor::Node, outgoing: &[NodeId]) -> ActionType {
        match node.action_kind {
            ActionKind::NavigateToView => ActionType::NavigateToView {
                target_view: sanitize_identifier(&node.title, &format!("view_{}", node.id), false),
            },
            ActionKind::ShowModal => ActionType::ShowModal {
                modal_id: sanitize_identifier(
                    &format!("{}_modal", node.title),
                    &format!("modal_{}", node.id),
                    false,
                ),
            },
            ActionKind::SetVariable => {
                let key = node
                    .variables
                    .first()
                    .map(|item| sanitize_identifier(&item.label, &format!("var_{}", node.id), true))
                    .unwrap_or_else(|| format!("var_{}", node.id));
                let value = node
                    .variables
                    .first()
                    .map(dynamic_value_from_handle)
                    .unwrap_or(DynamicValue::Null);

                ActionType::SetVariable { key, value }
            }
            ActionKind::Add => {
                let var_a = node
                    .variables
                    .first()
                    .map(|item| sanitize_identifier(&item.label, "a", true))
                    .unwrap_or_else(|| "a".to_owned());
                let var_b = node
                    .variables
                    .get(1)
                    .map(|item| sanitize_identifier(&item.label, "b", true))
                    .unwrap_or_else(|| "b".to_owned());

                ActionType::Add {
                    var_a,
                    var_b,
                    result_key: format!("sum_{}", node.id),
                }
            }
            ActionKind::Branch => ActionType::Branch {
                condition_variable: node
                    .variables
                    .first()
                    .map(|item| sanitize_identifier(&item.label, "condition", true))
                    .unwrap_or_else(|| "condition".to_owned()),
                true_next_node: outgoing.first().map(|node_id| node_id.to_string()),
                false_next_node: outgoing.get(1).map(|node_id| node_id.to_string()),
            },
            ActionKind::Match => {
                let match_variable = node
                    .variables
                    .first()
                    .map(|item| sanitize_identifier(&item.label, "match_value", true))
                    .unwrap_or_else(|| "match_value".to_owned());

                let branch_targets = outgoing.len().saturating_sub(1);
                let mut arms = Vec::with_capacity(branch_targets);
                for index in 0..branch_targets {
                    let value = node
                        .variables
                        .get(index + 1)
                        .map(dynamic_value_from_handle)
                        .unwrap_or(DynamicValue::Integer(index as i64));
                    arms.push((value, outgoing[index].to_string()));
                }

                ActionType::Match {
                    match_variable,
                    arms,
                    default_next_node: outgoing.last().map(|node_id| node_id.to_string()),
                }
            }
            ActionKind::ApiRequest => ActionType::ApiRequest {
                url: format!(
                    "https://api.example.com/{}",
                    sanitize_identifier(&node.title, &format!("action_{}", node.id), false)
                ),
                method: "GET".to_owned(),
                result_key: format!("response_{}", node.id),
            },
        }
    }

    fn is_action_node(&self, node_id: NodeId) -> bool {
        self.editor
            .nodes
            .iter()
            .any(|node| node.id == node_id && node.kind == NodeKind::Action)
    }
}

fn sanitize_identifier(input: &str, fallback: &str, strict_identifier: bool) -> String {
    let mut value = String::new();

    for ch in input.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            value.push(ch.to_ascii_lowercase());
        } else if !strict_identifier && (ch == ' ' || ch == '-' || ch == '.') {
            if !value.ends_with('_') {
                value.push('_');
            }
        }
    }

    let cleaned = value.trim_matches('_');
    if cleaned.is_empty() {
        fallback.to_owned()
    } else {
        cleaned.to_owned()
    }
}

fn dynamic_value_from_handle(handle: &node_editor::NodeVariable) -> DynamicValue {
    let seed = handle.label.clone();
    match handle.value_type {
        VariableValueType::Null => DynamicValue::Null,
        VariableValueType::Bool => DynamicValue::Bool(true),
        VariableValueType::Integer => DynamicValue::Integer(1),
        VariableValueType::Float => DynamicValue::Float(1.0),
        VariableValueType::String => DynamicValue::String(seed),
        VariableValueType::Char => DynamicValue::Char(seed.chars().next().unwrap_or('x')),
        VariableValueType::Bytes => DynamicValue::Bytes(seed.into_bytes()),
        VariableValueType::List => DynamicValue::List(vec![DynamicValue::String(seed)]),
        VariableValueType::Object => DynamicValue::Object(HashMap::from([(
            "value".to_owned(),
            DynamicValue::String(seed),
        )])),
        VariableValueType::Error => DynamicValue::Error(seed),
        VariableValueType::Timestamp => DynamicValue::Timestamp(1),
    }
}

fn ensure_dag(graph: &FlowGraph) -> Result<(), String> {
    let mut adjacency: HashMap<String, Vec<String>> = HashMap::new();

    for (node_id, node) in &graph.nodes {
        let mut next = Vec::new();

        if let Some(linear) = &node.next_node {
            if graph.nodes.contains_key(linear) {
                next.push(linear.clone());
            }
        }

        match &node.action {
            ActionType::Branch {
                true_next_node,
                false_next_node,
                ..
            } => {
                if let Some(true_node) = true_next_node
                    && graph.nodes.contains_key(true_node)
                {
                    next.push(true_node.clone());
                }
                if let Some(false_node) = false_next_node
                    && graph.nodes.contains_key(false_node)
                {
                    next.push(false_node.clone());
                }
            }
            ActionType::Match {
                arms,
                default_next_node,
                ..
            } => {
                for (_, arm_node) in arms {
                    if graph.nodes.contains_key(arm_node) {
                        next.push(arm_node.clone());
                    }
                }
                if let Some(default_node) = default_next_node
                    && graph.nodes.contains_key(default_node)
                {
                    next.push(default_node.clone());
                }
            }
            ActionType::NavigateToView { .. }
            | ActionType::ShowModal { .. }
            | ActionType::SetVariable { .. }
            | ActionType::Add { .. }
            | ActionType::ApiRequest { .. } => {}
        }

        next.sort();
        next.dedup();
        adjacency.insert(node_id.clone(), next);
    }

    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();

    for node_id in graph.nodes.keys() {
        visit_for_cycle(node_id, &adjacency, &mut visiting, &mut visited)?;
    }

    Ok(())
}

fn visit_for_cycle(
    node_id: &str,
    adjacency: &HashMap<String, Vec<String>>,
    visiting: &mut HashSet<String>,
    visited: &mut HashSet<String>,
) -> Result<(), String> {
    if visited.contains(node_id) {
        return Ok(());
    }
    if !visiting.insert(node_id.to_owned()) {
        return Err(format!("Cycle detected in flow graph at node {node_id}."));
    }

    if let Some(children) = adjacency.get(node_id) {
        for next in children {
            visit_for_cycle(next, adjacency, visiting, visited)?;
        }
    }

    visiting.remove(node_id);
    visited.insert(node_id.to_owned());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dag_check_rejects_cycle() {
        let mut nodes = HashMap::new();
        nodes.insert(
            "1".to_owned(),
            FlowNode {
                id: "1".to_owned(),
                action: ActionType::NavigateToView {
                    target_view: "a".to_owned(),
                },
                next_node: Some("2".to_owned()),
            },
        );
        nodes.insert(
            "2".to_owned(),
            FlowNode {
                id: "2".to_owned(),
                action: ActionType::NavigateToView {
                    target_view: "b".to_owned(),
                },
                next_node: Some("1".to_owned()),
            },
        );

        let graph = FlowGraph {
            id: "loop".to_owned(),
            nodes,
            start_node_id: Some("1".to_owned()),
        };

        assert!(ensure_dag(&graph).is_err());
    }

    #[test]
    fn build_graph_uses_active_start_input_connection() {
        let mut demo = NodeEditorDemo::new();

        let start_id = demo
            .editor
            .nodes
            .iter()
            .find(|node| node.kind == NodeKind::Start)
            .map(|node| node.id)
            .expect("start node");
        let action_one = demo
            .editor
            .nodes
            .iter()
            .find(|node| node.kind == NodeKind::Action)
            .map(|node| node.id)
            .expect("action node");
        let promote_to_action = demo
            .editor
            .nodes
            .iter()
            .find(|node| node.kind == NodeKind::Variable)
            .map(|node| node.id)
            .expect("variable node");

        if let Some(node) = demo
            .editor
            .nodes
            .iter_mut()
            .find(|node| node.id == promote_to_action)
        {
            node.kind = NodeKind::Action;
            node.title = "Second Action".to_owned();
            node.action_kind = ActionKind::ShowModal;
        }

        let start_inputs = demo
            .editor
            .nodes
            .iter()
            .find(|node| node.id == start_id)
            .map(|node| node.start_inputs.clone())
            .expect("start inputs");
        assert!(start_inputs.len() >= 2);

        let input_one = start_inputs[0].id;
        let input_two = start_inputs[1].id;

        demo.editor.connections.retain(|connection| {
            !(connection.from.node == start_id && connection.kind == ConnectionKind::Flow)
        });
        demo.editor.connections.push(node_editor::Connection {
            from: node_editor::PortRef {
                node: start_id,
                side: PortSide::Output,
                kind: PortKind::StartInput(input_one),
            },
            to: node_editor::PortRef {
                node: action_one,
                side: PortSide::Input,
                kind: PortKind::Primary,
            },
            kind: ConnectionKind::Flow,
        });
        demo.editor.connections.push(node_editor::Connection {
            from: node_editor::PortRef {
                node: start_id,
                side: PortSide::Output,
                kind: PortKind::StartInput(input_two),
            },
            to: node_editor::PortRef {
                node: promote_to_action,
                side: PortSide::Input,
                kind: PortKind::Primary,
            },
            kind: ConnectionKind::Flow,
        });

        if let Some(start_node) = demo
            .editor
            .nodes
            .iter_mut()
            .find(|node| node.id == start_id)
        {
            start_node.active_start_input = Some(input_two);
        }

        let (graph, _) = demo.build_graph().expect("graph should build");
        assert_eq!(graph.start_node_id, Some(promote_to_action.to_string()));
    }

    #[test]
    fn set_variable_action_uses_selected_dynamic_value_type() {
        let mut demo = NodeEditorDemo::new();
        let action_id = demo
            .editor
            .nodes
            .iter()
            .find(|node| node.kind == NodeKind::Action)
            .map(|node| node.id)
            .expect("action node");

        if let Some(action_node) = demo
            .editor
            .nodes
            .iter_mut()
            .find(|node| node.id == action_id)
        {
            action_node.action_kind = ActionKind::SetVariable;
            action_node.variables.clear();
            action_node.variables.push(node_editor::NodeVariable {
                id: 900,
                label: "count".to_owned(),
                y_offset: 60.0,
                value_type: VariableValueType::Integer,
            });
        }

        let outgoing = demo.outgoing_action_targets(action_id, PortKind::Primary);
        let action = demo.action_from_node(
            demo.editor
                .nodes
                .iter()
                .find(|node| node.id == action_id)
                .expect("action node"),
            &outgoing,
        );

        match action {
            ActionType::SetVariable { key, value } => {
                assert_eq!(key, "count");
                assert_eq!(value, DynamicValue::Integer(1));
            }
            other => panic!("unexpected action: {other:?}"),
        }
    }

    #[test]
    fn match_action_kind_builds_match_action_type() {
        let mut demo = NodeEditorDemo::new();
        let action_id = demo
            .editor
            .nodes
            .iter()
            .find(|node| node.kind == NodeKind::Action)
            .map(|node| node.id)
            .expect("action node");
        let second_action = demo
            .editor
            .nodes
            .iter()
            .find(|node| node.kind == NodeKind::Variable)
            .map(|node| node.id)
            .expect("second node");

        if let Some(action_node) = demo
            .editor
            .nodes
            .iter_mut()
            .find(|node| node.id == action_id)
        {
            action_node.action_kind = ActionKind::Match;
            action_node.variables.push(node_editor::NodeVariable {
                id: 901,
                label: "mode".to_owned(),
                y_offset: 60.0,
                value_type: VariableValueType::String,
            });
            action_node.variables.push(node_editor::NodeVariable {
                id: 902,
                label: "admin".to_owned(),
                y_offset: 86.0,
                value_type: VariableValueType::String,
            });
        }

        if let Some(second) = demo
            .editor
            .nodes
            .iter_mut()
            .find(|node| node.id == second_action)
        {
            second.kind = NodeKind::Action;
            second.title = "Fallback".to_owned();
        }

        demo.editor.connections.push(node_editor::Connection {
            from: node_editor::PortRef {
                node: action_id,
                side: PortSide::Output,
                kind: PortKind::Primary,
            },
            to: node_editor::PortRef {
                node: second_action,
                side: PortSide::Input,
                kind: PortKind::Primary,
            },
            kind: ConnectionKind::Flow,
        });

        let outgoing = demo.outgoing_action_targets(action_id, PortKind::Primary);
        let action = demo.action_from_node(
            demo.editor
                .nodes
                .iter()
                .find(|node| node.id == action_id)
                .expect("action node"),
            &outgoing,
        );

        match action {
            ActionType::Match { .. } => {}
            other => panic!("unexpected action: {other:?}"),
        }
    }
}
