use iced::alignment;
use iced::border;
use iced::mouse;
use iced::widget::canvas::{self, Canvas};
use iced::widget::text;
use iced::{Color, Element, Fill, Point, Rectangle, Renderer, Size, Theme, Vector};

pub type NodeId = u64;
pub type VariableHandleId = u64;
pub type StartInputId = u64;

const NODE_WIDTH: f32 = 232.0;
const START_HEIGHT: f32 = 84.0;
const ACTION_HEIGHT: f32 = 94.0;
const VARIABLE_HEIGHT: f32 = 90.0;
const PORT_HIT_RADIUS: f32 = 14.0;
const PORT_PROXIMITY_RADIUS: f32 = 96.0;
const VARIABLE_SLOT_WIDTH: f32 = 150.0;
const VARIABLE_SLOT_HEIGHT: f32 = 22.0;
const VARIABLE_SLOT_MIN_Y: f32 = 58.0;
const VARIABLE_SLOT_SPACING: f32 = 26.0;
const START_HEADER_HEIGHT: f32 = 38.0;
const START_INPUT_HEIGHT: f32 = 32.0;
const START_ADD_INPUT_HEIGHT: f32 = 30.0;
const START_OUTPUT_PANEL_GAP: f32 = 12.0;
const START_OUTPUT_PANEL_ROW_HEIGHT: f32 = 28.0;
const START_OUTPUT_PANEL_HEADER_HEIGHT: f32 = 24.0;
const START_OUTPUT_PANEL_EXTRA_WIDTH: f32 = 58.0;
const MIN_ZOOM: f32 = 0.35;
const MAX_ZOOM: f32 = 2.6;
const MINIMAP_WIDTH: f32 = 160.0;
const MINIMAP_HEIGHT: f32 = 110.0;
const MINIMAP_MARGIN: f32 = 16.0;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum StartTrigger {
    #[default]
    Manual,
    OnAppLoad,
    OnViewEnter,
    OnButtonPress,
    OnInputChange,
    OnSubmit,
    OnTimer,
    OnWebhook,
}

impl StartTrigger {
    pub fn label(self) -> &'static str {
        match self {
            Self::Manual => "Manual",
            Self::OnAppLoad => "On App Load",
            Self::OnViewEnter => "On View Enter",
            Self::OnButtonPress => "On Button Press",
            Self::OnInputChange => "On Input Change",
            Self::OnSubmit => "On Submit",
            Self::OnTimer => "On Timer",
            Self::OnWebhook => "On Webhook",
        }
    }
}

pub const START_TRIGGER_OPTIONS: [StartTrigger; 8] = [
    StartTrigger::Manual,
    StartTrigger::OnAppLoad,
    StartTrigger::OnViewEnter,
    StartTrigger::OnButtonPress,
    StartTrigger::OnInputChange,
    StartTrigger::OnSubmit,
    StartTrigger::OnTimer,
    StartTrigger::OnWebhook,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ActionKind {
    #[default]
    NavigateToView,
    ShowModal,
    SetVariable,
    Add,
    Branch,
    Match,
    ApiRequest,
}

impl ActionKind {
    pub fn label(self) -> &'static str {
        match self {
            Self::NavigateToView => "Navigate",
            Self::ShowModal => "Show Modal",
            Self::SetVariable => "Set Variable",
            Self::Add => "Add",
            Self::Branch => "Branch",
            Self::Match => "Match",
            Self::ApiRequest => "API Request",
        }
    }
}

pub const ACTION_KIND_OPTIONS: [ActionKind; 7] = [
    ActionKind::NavigateToView,
    ActionKind::ShowModal,
    ActionKind::SetVariable,
    ActionKind::Add,
    ActionKind::Branch,
    ActionKind::Match,
    ActionKind::ApiRequest,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum VariableValueType {
    Null,
    Bool,
    Integer,
    Float,
    #[default]
    String,
    Char,
    Bytes,
    List,
    Object,
    Error,
    Timestamp,
}

impl VariableValueType {
    #[allow(dead_code)]
    pub fn label(self) -> &'static str {
        match self {
            Self::Null => "Null",
            Self::Bool => "Bool",
            Self::Integer => "Integer",
            Self::Float => "Float",
            Self::String => "String",
            Self::Char => "Char",
            Self::Bytes => "Bytes",
            Self::List => "List",
            Self::Object => "Object",
            Self::Error => "Error",
            Self::Timestamp => "Timestamp",
        }
    }

    fn short_label(self) -> &'static str {
        match self {
            Self::Null => "N",
            Self::Bool => "B",
            Self::Integer => "I64",
            Self::Float => "F64",
            Self::String => "Str",
            Self::Char => "Ch",
            Self::Bytes => "Bin",
            Self::List => "Vec",
            Self::Object => "Obj",
            Self::Error => "Err",
            Self::Timestamp => "Ts",
        }
    }

    fn next(self) -> Self {
        match self {
            Self::Null => Self::Bool,
            Self::Bool => Self::Integer,
            Self::Integer => Self::Float,
            Self::Float => Self::String,
            Self::String => Self::Char,
            Self::Char => Self::Bytes,
            Self::Bytes => Self::List,
            Self::List => Self::Object,
            Self::Object => Self::Error,
            Self::Error => Self::Timestamp,
            Self::Timestamp => Self::Null,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeKind {
    Start,
    Action,
    Variable,
}

impl NodeKind {
    fn default_title(self) -> &'static str {
        match self {
            Self::Start => "Start",
            Self::Action => "Action",
            Self::Variable => "Variable",
        }
    }

    fn accepts_input(self) -> bool {
        matches!(self, Self::Action)
    }

    fn has_output(self) -> bool {
        true
    }

    fn size(self) -> Size {
        let height = match self {
            Self::Start => START_HEIGHT,
            Self::Action => ACTION_HEIGHT,
            Self::Variable => VARIABLE_HEIGHT,
        };

        Size::new(NODE_WIDTH, height)
    }

    fn chip_color(self) -> Color {
        match self {
            Self::Start => Color::from_rgb8(0x39, 0xD9, 0x8A),
            Self::Action => Color::from_rgb8(0x5F, 0xA8, 0xFF),
            Self::Variable => Color::from_rgb8(0xE8, 0xB8, 0x48),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
    pub title: String,
    pub position: Point,
    pub start_trigger: StartTrigger,
    pub start_inputs: Vec<StartInput>,
    pub active_start_input: Option<StartInputId>,
    pub action_kind: ActionKind,
    pub variables: Vec<NodeVariable>,
}

#[derive(Debug, Clone)]
pub struct StartInput {
    pub id: StartInputId,
    pub label: String,
}

#[derive(Debug, Clone)]
pub struct NodeVariable {
    pub id: VariableHandleId,
    pub label: String,
    pub y_offset: f32,
    pub value_type: VariableValueType,
}

impl Node {
    fn new(id: NodeId, kind: NodeKind, position: Point) -> Self {
        Self {
            id,
            kind,
            title: kind.default_title().to_owned(),
            position,
            start_trigger: StartTrigger::default(),
            start_inputs: if kind == NodeKind::Start {
                vec![
                    StartInput {
                        id: 1,
                        label: "Input 1".to_owned(),
                    },
                    StartInput {
                        id: 2,
                        label: "Input 2".to_owned(),
                    },
                ]
            } else {
                Vec::new()
            },
            active_start_input: if kind == NodeKind::Start {
                Some(1)
            } else {
                None
            },
            action_kind: ActionKind::default(),
            variables: Vec::new(),
        }
    }

    fn with_title(id: NodeId, kind: NodeKind, position: Point, title: impl Into<String>) -> Self {
        Self {
            id,
            kind,
            title: title.into(),
            position,
            start_trigger: StartTrigger::default(),
            start_inputs: if kind == NodeKind::Start {
                vec![StartInput {
                    id: 1,
                    label: "Input 1".to_owned(),
                }]
            } else {
                Vec::new()
            },
            active_start_input: if kind == NodeKind::Start {
                Some(1)
            } else {
                None
            },
            action_kind: ActionKind::default(),
            variables: Vec::new(),
        }
    }

    fn size(&self) -> Size {
        let mut size = self.kind.size();
        match self.kind {
            NodeKind::Start => {
                let rows = self.start_inputs.len() as f32;
                let required_height =
                    START_HEADER_HEIGHT + rows * START_INPUT_HEIGHT + START_ADD_INPUT_HEIGHT + 12.0;
                size.height = size.height.max(required_height);
            }
            NodeKind::Action => {
                let slots = self.variables.len().saturating_sub(1) as f32;
                let required_height = VARIABLE_SLOT_MIN_Y
                    + VARIABLE_SLOT_HEIGHT
                    + slots * VARIABLE_SLOT_SPACING
                    + 16.0;
                size.height = size.height.max(required_height);
            }
            NodeKind::Variable => {}
        }
        size
    }

    fn bounds(&self) -> Rectangle {
        Rectangle::new(self.position, self.size())
    }

    fn input_anchor(&self) -> Option<Point> {
        if self.kind.accepts_input() {
            let bounds = self.bounds();
            Some(Point::new(bounds.x, bounds.center_y()))
        } else {
            None
        }
    }

    fn output_anchor(&self) -> Option<Point> {
        if self.kind.has_output() && self.kind != NodeKind::Start {
            let bounds = self.bounds();
            Some(Point::new(bounds.x + bounds.width, bounds.center_y()))
        } else {
            None
        }
    }

    fn start_trigger_bounds(&self) -> Option<Rectangle> {
        (self.kind == NodeKind::Start).then(|| {
            Rectangle::new(
                Point::new(
                    self.position.x + self.size().width - 138.0,
                    self.position.y + 8.0,
                ),
                Size::new(126.0, 22.0),
            )
        })
    }

    fn start_input_bounds(&self, input_id: StartInputId) -> Option<Rectangle> {
        let index = self
            .start_inputs
            .iter()
            .position(|input| input.id == input_id)?;
        Some(Rectangle::new(
            Point::new(
                self.position.x + 16.0,
                self.position.y + START_HEADER_HEIGHT + index as f32 * START_INPUT_HEIGHT,
            ),
            Size::new(self.size().width - 28.0, START_INPUT_HEIGHT),
        ))
    }

    fn start_add_input_bounds(&self) -> Option<Rectangle> {
        (self.kind == NodeKind::Start).then(|| {
            Rectangle::new(
                Point::new(
                    self.position.x + 16.0,
                    self.position.y
                        + START_HEADER_HEIGHT
                        + self.start_inputs.len() as f32 * START_INPUT_HEIGHT,
                ),
                Size::new(self.size().width - 28.0, START_ADD_INPUT_HEIGHT),
            )
        })
    }

    fn start_input_output_anchor(&self, input_id: StartInputId) -> Option<Point> {
        let bounds = self.start_input_bounds(input_id)?;
        Some(Point::new(bounds.x + bounds.width + 8.0, bounds.center_y()))
    }

    fn action_kind_bounds(&self) -> Option<Rectangle> {
        (self.kind == NodeKind::Action).then(|| {
            Rectangle::new(
                Point::new(
                    self.position.x + self.size().width - 126.0,
                    self.position.y + 8.0,
                ),
                Size::new(112.0, 22.0),
            )
        })
    }

    fn variable_bounds(&self, variable: &NodeVariable) -> Rectangle {
        Rectangle::new(
            Point::new(self.position.x + 16.0, self.position.y + variable.y_offset),
            Size::new(VARIABLE_SLOT_WIDTH, VARIABLE_SLOT_HEIGHT),
        )
    }

    fn variable_type_bounds(&self, variable: &NodeVariable) -> Rectangle {
        let bounds = self.variable_bounds(variable);
        Rectangle::new(
            Point::new(bounds.x + bounds.width - 40.0, bounds.y + 2.0),
            Size::new(36.0, bounds.height - 4.0),
        )
    }

    fn variable_output_anchor(&self, variable: &NodeVariable) -> Point {
        let bounds = self.variable_bounds(variable);
        Point::new(bounds.x + bounds.width + 8.0, bounds.center_y())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PortSide {
    Input,
    Output,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PortRef {
    pub node: NodeId,
    pub side: PortSide,
    pub kind: PortKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PortKind {
    Primary,
    Variable(VariableHandleId),
    StartInput(StartInputId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectionKind {
    Flow,
    VariableSet,
    VariableAppend,
    VariableGuard,
}

impl ConnectionKind {
    fn color(self) -> Color {
        match self {
            Self::Flow => Color::from_rgb8(0x7A, 0xA8, 0xF6),
            Self::VariableSet => Color::from_rgb8(0xF2, 0xC0, 0x5C),
            Self::VariableAppend => Color::from_rgb8(0xE1, 0x88, 0x50),
            Self::VariableGuard => Color::from_rgb8(0x82, 0xD9, 0x9B),
        }
    }

    fn label(self) -> Option<&'static str> {
        match self {
            Self::Flow => None,
            Self::VariableSet => Some("set"),
            Self::VariableAppend => Some("append"),
            Self::VariableGuard => Some("guard"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Connection {
    pub from: PortRef,
    pub to: PortRef,
    pub kind: ConnectionKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContextAction {
    AddStart,
    AddAction,
    AddVariable,
}

impl ContextAction {
    pub fn label(self) -> &'static str {
        match self {
            Self::AddStart => "Start Block",
            Self::AddAction => "Action Block",
            Self::AddVariable => "Variable Block",
        }
    }
}

pub const CONTEXT_ACTIONS: [ContextAction; 3] = [
    ContextAction::AddStart,
    ContextAction::AddAction,
    ContextAction::AddVariable,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableDropContext {
    Blank,
    Existing(NodeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableDropOption {
    SetInput,
    AppendInput,
    GateInput,
    CreateSetAction,
    CreateConditionAction,
}

impl VariableDropOption {
    pub fn label(self) -> &'static str {
        match self {
            Self::SetInput => "Set Input",
            Self::AppendInput => "Append Input",
            Self::GateInput => "Gate Input",
            Self::CreateSetAction => "Create Set Action",
            Self::CreateConditionAction => "Create Condition Action",
        }
    }

    fn connection_kind(self) -> ConnectionKind {
        match self {
            Self::SetInput | Self::CreateSetAction => ConnectionKind::VariableSet,
            Self::AppendInput => ConnectionKind::VariableAppend,
            Self::GateInput | Self::CreateConditionAction => ConnectionKind::VariableGuard,
        }
    }
}

const BLANK_DROP_OPTIONS: [VariableDropOption; 2] = [
    VariableDropOption::CreateSetAction,
    VariableDropOption::CreateConditionAction,
];

const EXISTING_DROP_OPTIONS: [VariableDropOption; 3] = [
    VariableDropOption::SetInput,
    VariableDropOption::AppendInput,
    VariableDropOption::GateInput,
];

#[derive(Debug, Clone)]
pub struct ContextMenu {
    pub position: Point,
}

#[derive(Debug, Clone)]
pub struct VariableDropMenu {
    pub position: Point,
    pub source: PortRef,
    pub context: VariableDropContext,
}

#[derive(Debug, Clone)]
pub struct StartTriggerMenu {
    pub position: Point,
    pub node: NodeId,
}

#[derive(Debug, Clone)]
pub struct ActionKindMenu {
    pub position: Point,
    pub node: NodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HitTarget {
    NodeBody(NodeId),
    VariableChip {
        node: NodeId,
        variable: VariableHandleId,
    },
    VariableType {
        node: NodeId,
        variable: VariableHandleId,
    },
    StartTrigger(NodeId),
    StartAddInput(NodeId),
    StartInputRow {
        node: NodeId,
        input: StartInputId,
    },
    StartOutputRow {
        node: NodeId,
        input: StartInputId,
    },
    ActionKind(NodeId),
    InputPort(PortRef),
    OutputPort(PortRef),
    Blank,
}

#[derive(Debug, Clone, Copy)]
pub enum EditorEvent {
    PointerMoved {
        position: Point,
        viewport: Size,
    },
    LeftPressed {
        position: Point,
        viewport: Size,
    },
    LeftReleased {
        position: Point,
        viewport: Size,
    },
    RightPressed {
        position: Point,
        viewport: Size,
    },
    WheelScrolled {
        position: Point,
        delta_y: f32,
        viewport: Size,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum DragState {
    None,
    Node {
        id: NodeId,
        grab_offset: Vector,
    },
    VariableChip {
        node: NodeId,
        variable: VariableHandleId,
        grab_offset_y: f32,
    },
    StartInputRow {
        node: NodeId,
        input: StartInputId,
        grab_offset_y: f32,
    },
    Connection {
        from: PortRef,
        current: Point,
    },
    Pan {
        start: Point,
        pan_start: Vector,
    },
    Minimap {
        world_bounds: Rectangle,
    },
}

pub struct EditorState {
    pub nodes: Vec<Node>,
    pub connections: Vec<Connection>,
    pub selected_node: Option<NodeId>,
    pub cursor_world: Point,
    pub cursor_screen: Point,
    pub drag: DragState,
    pub context_menu: Option<ContextMenu>,
    pub variable_drop_menu: Option<VariableDropMenu>,
    pub start_trigger_menu: Option<StartTriggerMenu>,
    pub action_kind_menu: Option<ActionKindMenu>,
    pub zoom: f32,
    pub pan: Vector,
    pub viewport: Size,
    next_node_id: NodeId,
    next_variable_id: VariableHandleId,
    next_start_input_id: StartInputId,
}

pub struct NodeEditorWidget<'a, Message> {
    content: Element<'a, Message>,
}

impl<'a, Message> NodeEditorWidget<'a, Message> {
    fn new(content: Element<'a, Message>) -> Self {
        Self { content }
    }
}

impl<'a, Message> From<NodeEditorWidget<'a, Message>> for Element<'a, Message> {
    fn from(widget: NodeEditorWidget<'a, Message>) -> Self {
        widget.content
    }
}

impl Default for EditorState {
    fn default() -> Self {
        Self::new_demo()
    }
}

impl EditorState {
    pub fn new_demo() -> Self {
        let mut next_id = 1_u64;
        let start = Node::new(next_id, NodeKind::Start, Point::new(90.0, 120.0));
        next_id += 1;
        let next_start_input_id = start
            .start_inputs
            .iter()
            .map(|input| input.id)
            .max()
            .unwrap_or(0)
            + 1;
        let action = Node::new(next_id, NodeKind::Action, Point::new(420.0, 120.0));
        next_id += 1;
        let variable = Node::new(next_id, NodeKind::Variable, Point::new(420.0, 290.0));
        next_id += 1;

        let mut state = Self {
            nodes: vec![start, action, variable],
            connections: Vec::new(),
            selected_node: None,
            cursor_world: Point::ORIGIN,
            cursor_screen: Point::ORIGIN,
            drag: DragState::None,
            context_menu: None,
            variable_drop_menu: None,
            start_trigger_menu: None,
            action_kind_menu: None,
            zoom: 1.0,
            pan: Vector::new(0.0, 0.0),
            viewport: Size::new(1280.0, 800.0),
            next_node_id: next_id,
            next_variable_id: 1,
            next_start_input_id,
        };

        state.connections.push(Connection {
            from: PortRef {
                node: 1,
                side: PortSide::Output,
                kind: PortKind::StartInput(1),
            },
            to: PortRef {
                node: 2,
                side: PortSide::Input,
                kind: PortKind::Primary,
            },
            kind: ConnectionKind::Flow,
        });

        state
    }

    pub fn on_event(&mut self, event: EditorEvent) {
        match event {
            EditorEvent::PointerMoved { position, viewport } => {
                self.viewport = viewport;
                self.on_pointer_moved(position);
            }
            EditorEvent::LeftPressed { position, viewport } => {
                self.viewport = viewport;
                self.on_left_pressed(position);
            }
            EditorEvent::LeftReleased { position, viewport } => {
                self.viewport = viewport;
                self.on_left_released(position);
            }
            EditorEvent::RightPressed { position, viewport } => {
                self.viewport = viewport;
                self.on_right_pressed(position);
            }
            EditorEvent::WheelScrolled {
                position,
                delta_y,
                viewport,
            } => {
                self.viewport = viewport;
                self.on_wheel_scrolled(position, delta_y);
            }
        }
    }

    pub fn hit_test(&self, point: Point) -> HitTarget {
        if let Some(start_node) = self.selected_start_node() {
            for input in &start_node.start_inputs {
                if let Some(row_bounds) = self.start_output_row_bounds(start_node.id, input.id)
                    && row_bounds.contains(point)
                {
                    return HitTarget::StartOutputRow {
                        node: start_node.id,
                        input: input.id,
                    };
                }
            }
        }

        for node in self.nodes.iter().rev() {
            if node.kind == NodeKind::Start {
                if let Some(trigger_bounds) = node.start_trigger_bounds()
                    && trigger_bounds.contains(point)
                {
                    return HitTarget::StartTrigger(node.id);
                }

                if let Some(add_input_bounds) = node.start_add_input_bounds()
                    && add_input_bounds.contains(point)
                {
                    return HitTarget::StartAddInput(node.id);
                }

                for input in node.start_inputs.iter().rev() {
                    if let Some(row_bounds) = node.start_input_bounds(input.id)
                        && row_bounds.contains(point)
                    {
                        return HitTarget::StartInputRow {
                            node: node.id,
                            input: input.id,
                        };
                    }

                    if let Some(output_anchor) = node.start_input_output_anchor(input.id) {
                        let port = PortRef {
                            node: node.id,
                            side: PortSide::Output,
                            kind: PortKind::StartInput(input.id),
                        };
                        let radius = self.output_port_radius(port) + 4.0 / self.zoom.max(0.01);
                        if point.distance(output_anchor) <= radius {
                            return HitTarget::OutputPort(port);
                        }
                    }
                }
            }

            if node.kind == NodeKind::Action
                && let Some(action_bounds) = node.action_kind_bounds()
                && action_bounds.contains(point)
            {
                return HitTarget::ActionKind(node.id);
            }

            for variable in node.variables.iter().rev() {
                let variable_port = PortRef {
                    node: node.id,
                    side: PortSide::Output,
                    kind: PortKind::Variable(variable.id),
                };
                let output_anchor = node.variable_output_anchor(variable);
                let radius = self.output_port_radius(variable_port) + 4.0 / self.zoom.max(0.01);
                if point.distance(output_anchor) <= radius {
                    return HitTarget::OutputPort(variable_port);
                }

                if node.variable_type_bounds(variable).contains(point) {
                    return HitTarget::VariableType {
                        node: node.id,
                        variable: variable.id,
                    };
                }

                if node.variable_bounds(variable).contains(point) {
                    return HitTarget::VariableChip {
                        node: node.id,
                        variable: variable.id,
                    };
                }
            }

            if let Some(output_anchor) = node.output_anchor() {
                let primary_port = PortRef {
                    node: node.id,
                    side: PortSide::Output,
                    kind: PortKind::Primary,
                };
                let radius = self.output_port_radius(primary_port) + 4.0 / self.zoom.max(0.01);
                if point.distance(output_anchor) <= radius {
                    return HitTarget::OutputPort(primary_port);
                }
            }

            if let Some(input_anchor) = node.input_anchor()
                && point.distance(input_anchor) <= PORT_HIT_RADIUS / self.zoom.max(0.01)
            {
                return HitTarget::InputPort(PortRef {
                    node: node.id,
                    side: PortSide::Input,
                    kind: PortKind::Primary,
                });
            }

            if node.bounds().contains(point) {
                return HitTarget::NodeBody(node.id);
            }
        }

        HitTarget::Blank
    }

    pub fn apply_context_action(&mut self, action: ContextAction) {
        let Some(menu) = self.context_menu.take() else {
            return;
        };
        self.start_trigger_menu = None;
        self.action_kind_menu = None;

        let position =
            self.screen_to_world(menu.position) + Vector::new(16.0 / self.zoom, -12.0 / self.zoom);
        let new_id = match action {
            ContextAction::AddStart => self.add_node(NodeKind::Start, position),
            ContextAction::AddAction => self.add_node(NodeKind::Action, position),
            ContextAction::AddVariable => self.add_node(NodeKind::Variable, position),
        };

        self.selected_node = Some(new_id);
    }

    pub fn apply_start_trigger(&mut self, node_id: NodeId, trigger: StartTrigger) {
        self.start_trigger_menu = None;
        if let Some(node) = self.node_mut(node_id)
            && node.kind == NodeKind::Start
        {
            node.start_trigger = trigger;
            self.selected_node = Some(node_id);
        }
    }

    pub fn apply_action_kind(&mut self, node_id: NodeId, action_kind: ActionKind) {
        self.action_kind_menu = None;
        if let Some(node) = self.node_mut(node_id)
            && node.kind == NodeKind::Action
        {
            node.action_kind = action_kind;
            self.selected_node = Some(node_id);
        }
    }

    pub fn variable_drop_options(&self) -> &'static [VariableDropOption] {
        match self.variable_drop_menu.as_ref().map(|menu| menu.context) {
            Some(VariableDropContext::Blank) => &BLANK_DROP_OPTIONS,
            Some(VariableDropContext::Existing(_)) => &EXISTING_DROP_OPTIONS,
            None => &[],
        }
    }

    pub fn apply_variable_drop_option(&mut self, option: VariableDropOption) {
        let Some(menu) = self.variable_drop_menu.take() else {
            return;
        };
        self.start_trigger_menu = None;
        self.action_kind_menu = None;

        match menu.context {
            VariableDropContext::Existing(target_node) => {
                if self
                    .add_variable_handle(target_node, menu.source, option)
                    .is_some()
                    && let Some(input) = self.input_port(target_node)
                {
                    self.connect(menu.source, input, option.connection_kind());
                    self.selected_node = Some(target_node);
                }
            }
            VariableDropContext::Blank => {
                let menu_world = self.screen_to_world(menu.position);
                let title = match option {
                    VariableDropOption::CreateSetAction => "Set Variable",
                    VariableDropOption::CreateConditionAction => "Variable Guard",
                    _ => "Action",
                };
                let created_id = self.add_named_action(
                    Point::new(
                        menu_world.x + 26.0 / self.zoom,
                        menu_world.y - 20.0 / self.zoom,
                    ),
                    title,
                );

                let _ = self.add_variable_handle(created_id, menu.source, option);

                if let Some(input) = self.input_port(created_id) {
                    self.connect(menu.source, input, option.connection_kind());
                }

                self.selected_node = Some(created_id);
            }
        }
    }

    pub fn status(&self) -> String {
        let selected = self
            .selected_node
            .and_then(|id| self.node(id))
            .map(|node| node.title.clone())
            .unwrap_or_else(|| "None".to_owned());
        let variable_handles: usize = self.nodes.iter().map(|node| node.variables.len()).sum();

        format!(
            "Selected: {selected} | Nodes: {} | Connections: {} | Variable Handles: {variable_handles} | Zoom: {:.0}%",
            self.nodes.len(),
            self.connections.len(),
            self.zoom * 100.0
        )
    }

    pub fn canvas<'a, Message: 'a>(
        &'a self,
        on_event: impl Fn(EditorEvent) -> Message + 'a,
    ) -> Element<'a, Message> {
        Canvas::new(EditorProgram {
            state: self,
            on_event: Box::new(on_event),
        })
        .width(Fill)
        .height(Fill)
        .into()
    }

    pub fn widget<'a, Message: 'a>(
        &'a self,
        on_event: impl Fn(EditorEvent) -> Message + 'a,
    ) -> NodeEditorWidget<'a, Message> {
        NodeEditorWidget::new(self.canvas(on_event))
    }

    fn on_pointer_moved(&mut self, position_screen: Point) {
        self.cursor_screen = position_screen;
        self.cursor_world = self.screen_to_world(position_screen);

        match self.drag {
            DragState::Node { id, grab_offset } => {
                let position_world = self.screen_to_world(position_screen);
                if let Some(node) = self.node_mut(id) {
                    node.position = Point::new(
                        position_world.x - grab_offset.x,
                        position_world.y - grab_offset.y,
                    );
                }
            }
            DragState::VariableChip {
                node,
                variable,
                grab_offset_y,
            } => {
                let Some(node_position) = self.node(node).map(|n| n.position) else {
                    return;
                };
                let position_world = self.screen_to_world(position_screen);
                let local_y = position_world.y - node_position.y - grab_offset_y;
                if let Some(variable_ref) = self.variable_mut(node, variable) {
                    variable_ref.y_offset = local_y;
                }
                self.normalize_variable_positions(node);
            }
            DragState::StartInputRow {
                node,
                input,
                grab_offset_y,
            } => {
                let Some(node_position) = self.node(node).map(|current| current.position) else {
                    return;
                };
                let position_world = self.screen_to_world(position_screen);
                let local_top =
                    position_world.y - node_position.y - START_HEADER_HEIGHT - grab_offset_y;
                let target_index =
                    ((local_top + START_INPUT_HEIGHT * 0.5) / START_INPUT_HEIGHT).floor() as isize;
                self.reorder_start_input_to_index(node, input, target_index);
            }
            DragState::Connection { from, .. } => {
                let position_world = self.screen_to_world(position_screen);
                self.drag = DragState::Connection {
                    from,
                    current: position_world,
                };
            }
            DragState::Pan { start, pan_start } => {
                self.pan = pan_start + (position_screen - start);
            }
            DragState::Minimap { world_bounds } => {
                self.pan_to_minimap(position_screen, world_bounds);
            }
            DragState::None => {}
        }
    }

    fn on_left_pressed(&mut self, position_screen: Point) {
        self.cursor_screen = position_screen;
        self.cursor_world = self.screen_to_world(position_screen);
        self.context_menu = None;
        self.variable_drop_menu = None;
        self.start_trigger_menu = None;
        self.action_kind_menu = None;

        if let Some(minimap_bounds) = self.minimap_bounds()
            && minimap_bounds.contains(position_screen)
        {
            let world_bounds = self.world_bounds();
            self.drag = DragState::Minimap { world_bounds };
            self.pan_to_minimap(position_screen, world_bounds);
            return;
        }

        let position_world = self.screen_to_world(position_screen);

        match self.hit_test(position_world) {
            HitTarget::StartTrigger(node_id) => {
                let menu_position = self
                    .node(node_id)
                    .and_then(Node::start_trigger_bounds)
                    .map(|bounds| {
                        self.world_to_screen(Point::new(
                            bounds.x,
                            bounds.y + bounds.height + 4.0 / self.zoom,
                        ))
                    })
                    .unwrap_or(position_screen);
                self.start_trigger_menu = Some(StartTriggerMenu {
                    position: menu_position,
                    node: node_id,
                });
                self.selected_node = Some(node_id);
                self.drag = DragState::None;
            }
            HitTarget::StartAddInput(node_id) => {
                self.add_start_input(node_id);
                self.selected_node = Some(node_id);
                self.drag = DragState::None;
            }
            HitTarget::StartInputRow { node, input } => {
                if let Some(target_node) = self.node_mut(node)
                    && let Some(bounds) = target_node.start_input_bounds(input)
                {
                    target_node.active_start_input = Some(input);
                    self.drag = DragState::StartInputRow {
                        node,
                        input,
                        grab_offset_y: position_world.y - bounds.y,
                    };
                } else {
                    self.drag = DragState::None;
                }
                self.selected_node = Some(node);
            }
            HitTarget::StartOutputRow { node, input } => {
                self.cycle_start_output_target(node, input);
                self.selected_node = Some(node);
                self.drag = DragState::None;
            }
            HitTarget::ActionKind(node_id) => {
                let menu_position = self
                    .node(node_id)
                    .and_then(Node::action_kind_bounds)
                    .map(|bounds| {
                        self.world_to_screen(Point::new(
                            bounds.x,
                            bounds.y + bounds.height + 4.0 / self.zoom,
                        ))
                    })
                    .unwrap_or(position_screen);
                self.action_kind_menu = Some(ActionKindMenu {
                    position: menu_position,
                    node: node_id,
                });
                self.selected_node = Some(node_id);
                self.drag = DragState::None;
            }
            HitTarget::VariableType { node, variable } => {
                if let Some(variable_ref) = self.variable_mut(node, variable) {
                    variable_ref.value_type = variable_ref.value_type.next();
                }
                self.selected_node = Some(node);
                self.drag = DragState::None;
            }
            HitTarget::OutputPort(port) => {
                self.selected_node = Some(port.node);
                self.drag = DragState::Connection {
                    from: port,
                    current: position_world,
                };
                self.bring_to_front(port.node);
            }
            HitTarget::NodeBody(node_id) => {
                self.selected_node = Some(node_id);
                self.bring_to_front(node_id);

                if let Some(node) = self.node(node_id) {
                    self.drag = DragState::Node {
                        id: node_id,
                        grab_offset: position_world - node.position,
                    };
                }
            }
            HitTarget::VariableChip { node, variable } => {
                self.selected_node = Some(node);
                self.bring_to_front(node);

                if let Some(bounds) = self.variable_bounds(node, variable) {
                    self.drag = DragState::VariableChip {
                        node,
                        variable,
                        grab_offset_y: position_world.y - bounds.y,
                    };
                }
            }
            HitTarget::InputPort(port) => {
                self.selected_node = Some(port.node);
                self.drag = DragState::None;
            }
            HitTarget::Blank => {
                self.selected_node = None;
                self.drag = DragState::Pan {
                    start: position_screen,
                    pan_start: self.pan,
                };
            }
        }
    }

    fn on_left_released(&mut self, position_screen: Point) {
        self.cursor_screen = position_screen;
        self.cursor_world = self.screen_to_world(position_screen);
        let drag = self.drag;
        self.drag = DragState::None;

        match drag {
            DragState::Connection { from, .. } => {
                self.on_connection_drop(from, self.screen_to_world(position_screen))
            }
            DragState::VariableChip { node, .. } => self.normalize_variable_positions(node),
            DragState::StartInputRow { .. } => {}
            DragState::None
            | DragState::Node { .. }
            | DragState::Pan { .. }
            | DragState::Minimap { .. } => {}
        }
    }

    fn on_right_pressed(&mut self, position_screen: Point) {
        self.cursor_screen = position_screen;
        self.cursor_world = self.screen_to_world(position_screen);
        self.drag = DragState::None;
        self.variable_drop_menu = None;
        self.start_trigger_menu = None;
        self.action_kind_menu = None;

        match self.hit_test(self.screen_to_world(position_screen)) {
            HitTarget::Blank => {
                self.selected_node = None;
                self.context_menu = Some(ContextMenu {
                    position: position_screen,
                });
            }
            HitTarget::NodeBody(node_id)
            | HitTarget::VariableChip { node: node_id, .. }
            | HitTarget::VariableType { node: node_id, .. }
            | HitTarget::StartTrigger(node_id)
            | HitTarget::StartAddInput(node_id)
            | HitTarget::StartInputRow { node: node_id, .. }
            | HitTarget::StartOutputRow { node: node_id, .. }
            | HitTarget::ActionKind(node_id)
            | HitTarget::InputPort(PortRef { node: node_id, .. })
            | HitTarget::OutputPort(PortRef { node: node_id, .. }) => {
                self.selected_node = Some(node_id);
                self.context_menu = None;
                self.bring_to_front(node_id);
            }
        }
    }

    fn on_wheel_scrolled(&mut self, position_screen: Point, delta_y: f32) {
        let old_zoom = self.zoom;
        let old_world = self.screen_to_world_with(position_screen, old_zoom, self.pan);

        let factor = (1.0 + delta_y / 30.0).clamp(0.82, 1.18);
        self.zoom = (self.zoom * factor).clamp(MIN_ZOOM, MAX_ZOOM);

        self.pan = Vector::new(
            position_screen.x - old_world.x * self.zoom,
            position_screen.y - old_world.y * self.zoom,
        );
    }

    fn on_connection_drop(&mut self, from: PortRef, position: Point) {
        let target_input = match self.hit_test(position) {
            HitTarget::InputPort(port) => Some(port),
            HitTarget::NodeBody(node_id) => self.input_port(node_id),
            _ => None,
        };

        let is_variable_source = match from.kind {
            PortKind::Variable(_) => true,
            PortKind::Primary => self
                .node(from.node)
                .is_some_and(|node| node.kind == NodeKind::Variable),
            PortKind::StartInput(_) => false,
        };

        if is_variable_source {
            self.variable_drop_menu = Some(VariableDropMenu {
                position: self.world_to_screen(position),
                source: from,
                context: target_input
                    .map(|target| VariableDropContext::Existing(target.node))
                    .unwrap_or(VariableDropContext::Blank),
            });
            return;
        }

        if let Some(to) = target_input {
            let kind = if matches!(from.kind, PortKind::Variable(_)) {
                ConnectionKind::VariableSet
            } else {
                ConnectionKind::Flow
            };
            self.connect(from, to, kind);
        }
    }

    fn connect(&mut self, from: PortRef, to: PortRef, kind: ConnectionKind) {
        if from.node == to.node {
            return;
        }

        if kind == ConnectionKind::Flow && matches!(from.kind, PortKind::StartInput(_)) {
            self.connections.retain(|existing| {
                !(existing.kind == ConnectionKind::Flow && existing.from == from)
            });
        }

        if !self
            .connections
            .iter()
            .any(|existing| existing.from == from && existing.to == to && existing.kind == kind)
        {
            self.connections.push(Connection { from, to, kind });
        }
    }

    fn add_node(&mut self, kind: NodeKind, position: Point) -> NodeId {
        let node_id = self.next_node_id;
        self.next_node_id += 1;
        let mut node = Node::new(node_id, kind, position);
        if kind == NodeKind::Start {
            for input in &mut node.start_inputs {
                input.id = self.next_start_input_id;
                self.next_start_input_id += 1;
            }
            node.active_start_input = node.start_inputs.first().map(|input| input.id);
        }
        self.nodes.push(node);
        node_id
    }

    fn add_named_action(&mut self, position: Point, title: &str) -> NodeId {
        let node_id = self.next_node_id;
        self.next_node_id += 1;
        self.nodes
            .push(Node::with_title(node_id, NodeKind::Action, position, title));
        node_id
    }

    fn add_start_input(&mut self, node_id: NodeId) {
        let input_id = self.next_start_input_id;
        self.next_start_input_id += 1;

        if let Some(node) = self.node_mut(node_id) {
            if node.kind != NodeKind::Start {
                return;
            }

            let next_label = node.start_inputs.len() + 1;
            node.start_inputs.push(StartInput {
                id: input_id,
                label: format!("Input {next_label}"),
            });
            node.active_start_input = Some(input_id);
        }
    }

    fn add_variable_handle(
        &mut self,
        target_node: NodeId,
        source_port: PortRef,
        option: VariableDropOption,
    ) -> Option<VariableHandleId> {
        let source_title = self.node(source_port.node).map(|node| node.title.clone())?;
        if !self
            .node(target_node)
            .is_some_and(|node| node.kind == NodeKind::Action)
        {
            return None;
        }

        let handle_id = self.next_variable_id;
        self.next_variable_id += 1;

        let node = self.node_mut(target_node)?;

        let suffix = match option {
            VariableDropOption::SetInput | VariableDropOption::CreateSetAction => "set",
            VariableDropOption::AppendInput => "append",
            VariableDropOption::GateInput | VariableDropOption::CreateConditionAction => "guard",
        };
        let y_offset = VARIABLE_SLOT_MIN_Y + (node.variables.len() as f32 * VARIABLE_SLOT_SPACING);

        node.variables.push(NodeVariable {
            id: handle_id,
            label: format!("{source_title}.{suffix}"),
            y_offset,
            value_type: VariableValueType::default(),
        });
        self.normalize_variable_positions(target_node);
        Some(handle_id)
    }

    fn node(&self, node_id: NodeId) -> Option<&Node> {
        self.nodes.iter().find(|node| node.id == node_id)
    }

    fn node_mut(&mut self, node_id: NodeId) -> Option<&mut Node> {
        self.nodes.iter_mut().find(|node| node.id == node_id)
    }

    fn variable_mut(
        &mut self,
        node_id: NodeId,
        variable_id: VariableHandleId,
    ) -> Option<&mut NodeVariable> {
        self.node_mut(node_id)?
            .variables
            .iter_mut()
            .find(|variable| variable.id == variable_id)
    }

    fn variable_bounds(&self, node_id: NodeId, variable_id: VariableHandleId) -> Option<Rectangle> {
        let node = self.node(node_id)?;
        let variable = node
            .variables
            .iter()
            .find(|variable| variable.id == variable_id)?;
        Some(node.variable_bounds(variable))
    }

    #[allow(dead_code)]
    fn variable_type_bounds(
        &self,
        node_id: NodeId,
        variable_id: VariableHandleId,
    ) -> Option<Rectangle> {
        let node = self.node(node_id)?;
        let variable = node
            .variables
            .iter()
            .find(|variable| variable.id == variable_id)?;
        Some(node.variable_type_bounds(variable))
    }

    fn selected_start_node(&self) -> Option<&Node> {
        let selected = self.selected_node?;
        self.node(selected)
            .filter(|node| node.kind == NodeKind::Start)
    }

    fn start_output_panel_bounds(&self, node: &Node) -> Rectangle {
        let width = node.size().width + START_OUTPUT_PANEL_EXTRA_WIDTH;
        let height = START_OUTPUT_PANEL_HEADER_HEIGHT
            + node.start_inputs.len() as f32 * START_OUTPUT_PANEL_ROW_HEIGHT
            + 10.0;

        Rectangle::new(
            Point::new(
                node.position.x,
                node.position.y + node.size().height + START_OUTPUT_PANEL_GAP,
            ),
            Size::new(width, height),
        )
    }

    fn start_output_row_bounds(
        &self,
        node_id: NodeId,
        input_id: StartInputId,
    ) -> Option<Rectangle> {
        let node = self.node(node_id)?;
        if node.kind != NodeKind::Start {
            return None;
        }
        let index = node
            .start_inputs
            .iter()
            .position(|input| input.id == input_id)?;
        let panel = self.start_output_panel_bounds(node);

        Some(Rectangle::new(
            Point::new(
                panel.x + 8.0,
                panel.y
                    + START_OUTPUT_PANEL_HEADER_HEIGHT
                    + index as f32 * START_OUTPUT_PANEL_ROW_HEIGHT,
            ),
            Size::new(panel.width - 16.0, START_OUTPUT_PANEL_ROW_HEIGHT - 2.0),
        ))
    }

    fn start_output_target(&self, node_id: NodeId, input_id: StartInputId) -> Option<NodeId> {
        self.connections.iter().find_map(|connection| {
            (connection.kind == ConnectionKind::Flow
                && connection.from.node == node_id
                && connection.from.side == PortSide::Output
                && connection.from.kind == PortKind::StartInput(input_id)
                && connection.to.side == PortSide::Input
                && connection.to.kind == PortKind::Primary
                && self
                    .node(connection.to.node)
                    .is_some_and(|node| node.kind == NodeKind::Action))
            .then_some(connection.to.node)
        })
    }

    fn set_start_output_target(
        &mut self,
        node_id: NodeId,
        input_id: StartInputId,
        target: Option<NodeId>,
    ) {
        self.connections.retain(|connection| {
            !(connection.kind == ConnectionKind::Flow
                && connection.from.node == node_id
                && connection.from.side == PortSide::Output
                && connection.from.kind == PortKind::StartInput(input_id))
        });

        let Some(target_node) = target else {
            return;
        };
        if self
            .node(target_node)
            .is_none_or(|node| node.kind != NodeKind::Action)
        {
            return;
        }

        if let Some(input) = self.input_port(target_node) {
            self.connect(
                PortRef {
                    node: node_id,
                    side: PortSide::Output,
                    kind: PortKind::StartInput(input_id),
                },
                input,
                ConnectionKind::Flow,
            );
        }
    }

    fn cycle_start_output_target(&mut self, node_id: NodeId, input_id: StartInputId) {
        let mut options = vec![None];
        options.extend(
            self.nodes
                .iter()
                .filter(|node| node.kind == NodeKind::Action)
                .map(|node| Some(node.id)),
        );
        if options.is_empty() {
            return;
        }

        let current = self.start_output_target(node_id, input_id);
        let current_index = options
            .iter()
            .position(|candidate| *candidate == current)
            .unwrap_or(0);
        let next = options[(current_index + 1) % options.len()];
        self.set_start_output_target(node_id, input_id, next);
    }

    fn reorder_start_input_to_index(
        &mut self,
        node_id: NodeId,
        input_id: StartInputId,
        target_index: isize,
    ) {
        let Some(node) = self.node_mut(node_id) else {
            return;
        };
        let len = node.start_inputs.len();
        if len < 2 {
            return;
        }

        let Some(current_index) = node
            .start_inputs
            .iter()
            .position(|input| input.id == input_id)
        else {
            return;
        };
        let target_index = target_index.clamp(0, (len - 1) as isize) as usize;
        if current_index == target_index {
            return;
        }

        let input = node.start_inputs.remove(current_index);
        node.start_inputs.insert(target_index, input);
        node.active_start_input = Some(input_id);
    }

    fn input_port(&self, node_id: NodeId) -> Option<PortRef> {
        self.node(node_id)
            .filter(|node| node.kind.accepts_input())
            .map(|_| PortRef {
                node: node_id,
                side: PortSide::Input,
                kind: PortKind::Primary,
            })
    }

    #[allow(dead_code)]
    fn output_port(&self, node_id: NodeId) -> Option<PortRef> {
        self.node(node_id).and_then(|node| {
            if node.kind == NodeKind::Start {
                let selected = node
                    .active_start_input
                    .or_else(|| node.start_inputs.first().map(|input| input.id))?;
                Some(PortRef {
                    node: node_id,
                    side: PortSide::Output,
                    kind: PortKind::StartInput(selected),
                })
            } else if node.kind.has_output() {
                Some(PortRef {
                    node: node_id,
                    side: PortSide::Output,
                    kind: PortKind::Primary,
                })
            } else {
                None
            }
        })
    }

    fn anchor(&self, port: PortRef) -> Option<Point> {
        let node = self.node(port.node)?;
        match port.side {
            PortSide::Input => {
                if port.kind == PortKind::Primary {
                    node.input_anchor()
                } else {
                    None
                }
            }
            PortSide::Output => match port.kind {
                PortKind::Primary => node.output_anchor(),
                PortKind::Variable(variable_id) => node
                    .variables
                    .iter()
                    .find(|variable| variable.id == variable_id)
                    .map(|variable| node.variable_output_anchor(variable)),
                PortKind::StartInput(input_id) => node.start_input_output_anchor(input_id),
            },
        }
    }

    fn output_port_radius(&self, port: PortRef) -> f32 {
        let Some(anchor) = self.anchor(port) else {
            return PORT_HIT_RADIUS;
        };

        let is_drag_source =
            matches!(self.drag, DragState::Connection { from, .. } if from == port);
        let expanded = if port.kind == PortKind::Primary {
            13.5 / self.zoom.max(0.01)
        } else {
            11.5 / self.zoom.max(0.01)
        };
        let normal = if port.kind == PortKind::Primary {
            8.5 / self.zoom.max(0.01)
        } else {
            6.8 / self.zoom.max(0.01)
        };

        if is_drag_source
            || self.cursor_world.distance(anchor) < PORT_PROXIMITY_RADIUS / self.zoom.max(0.01)
        {
            expanded
        } else {
            normal
        }
    }

    fn normalize_variable_positions(&mut self, node_id: NodeId) {
        let Some(node) = self.node_mut(node_id) else {
            return;
        };

        let min_y = VARIABLE_SLOT_MIN_Y;
        let max_y = (node.size().height - VARIABLE_SLOT_HEIGHT - 8.0).max(min_y);

        node.variables
            .iter_mut()
            .for_each(|variable| variable.y_offset = variable.y_offset.clamp(min_y, max_y));
        node.variables
            .sort_by(|a, b| a.y_offset.total_cmp(&b.y_offset));
    }

    fn bring_to_front(&mut self, node_id: NodeId) {
        let Some(index) = self.nodes.iter().position(|node| node.id == node_id) else {
            return;
        };

        if index + 1 == self.nodes.len() {
            return;
        }

        let node = self.nodes.remove(index);
        self.nodes.push(node);
    }

    fn screen_to_world(&self, screen: Point) -> Point {
        self.screen_to_world_with(screen, self.zoom, self.pan)
    }

    fn screen_to_world_with(&self, screen: Point, zoom: f32, pan: Vector) -> Point {
        Point::new((screen.x - pan.x) / zoom, (screen.y - pan.y) / zoom)
    }

    fn world_to_screen(&self, world: Point) -> Point {
        Point::new(
            world.x * self.zoom + self.pan.x,
            world.y * self.zoom + self.pan.y,
        )
    }

    fn minimap_bounds(&self) -> Option<Rectangle> {
        if self.viewport.width < 1.0 || self.viewport.height < 1.0 {
            return None;
        }

        Some(Rectangle::new(
            Point::new(
                MINIMAP_MARGIN,
                self.viewport.height - MINIMAP_MARGIN - MINIMAP_HEIGHT,
            ),
            Size::new(MINIMAP_WIDTH, MINIMAP_HEIGHT),
        ))
    }

    fn world_bounds(&self) -> Rectangle {
        let mut bounds = self
            .nodes
            .iter()
            .map(Node::bounds)
            .reduce(|acc, next| acc.union(&next))
            .unwrap_or(Rectangle::new(
                Point::new(-300.0, -200.0),
                Size::new(800.0, 600.0),
            ));

        bounds = bounds.expand(120.0);
        if bounds.width < 400.0 {
            bounds.width = 400.0;
        }
        if bounds.height < 300.0 {
            bounds.height = 300.0;
        }
        bounds
    }

    fn pan_to_minimap(&mut self, cursor_screen: Point, world_bounds: Rectangle) {
        let Some(minimap) = self.minimap_bounds() else {
            return;
        };

        let nx = ((cursor_screen.x - minimap.x) / minimap.width).clamp(0.0, 1.0);
        let ny = ((cursor_screen.y - minimap.y) / minimap.height).clamp(0.0, 1.0);
        let world_target = Point::new(
            world_bounds.x + nx * world_bounds.width,
            world_bounds.y + ny * world_bounds.height,
        );
        let viewport_center = Point::new(self.viewport.width * 0.5, self.viewport.height * 0.5);
        self.pan = Vector::new(
            viewport_center.x - world_target.x * self.zoom,
            viewport_center.y - world_target.y * self.zoom,
        );
    }
}

struct EditorProgram<'a, Message> {
    state: &'a EditorState,
    on_event: Box<dyn Fn(EditorEvent) -> Message + 'a>,
}

impl<Message> canvas::Program<Message> for EditorProgram<'_, Message> {
    type State = ();

    fn update(
        &self,
        _state: &mut Self::State,
        event: &canvas::Event,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> Option<canvas::Action<Message>> {
        let local = cursor.position_from(bounds.position())?;
        let viewport = bounds.size();

        let event = match event {
            canvas::Event::Mouse(mouse::Event::CursorMoved { .. }) => {
                Some(EditorEvent::PointerMoved {
                    position: local,
                    viewport,
                })
            }
            canvas::Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                Some(EditorEvent::LeftPressed {
                    position: local,
                    viewport,
                })
            }
            canvas::Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) => {
                Some(EditorEvent::LeftReleased {
                    position: local,
                    viewport,
                })
            }
            canvas::Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Right)) => {
                Some(EditorEvent::RightPressed {
                    position: local,
                    viewport,
                })
            }
            canvas::Event::Mouse(mouse::Event::WheelScrolled { delta }) => {
                let delta_y = match *delta {
                    mouse::ScrollDelta::Lines { y, .. } => y,
                    mouse::ScrollDelta::Pixels { y, .. } => y / 20.0,
                };

                Some(EditorEvent::WheelScrolled {
                    position: local,
                    delta_y,
                    viewport,
                })
            }
            _ => None,
        }?;

        Some(canvas::Action::publish((self.on_event)(event)).and_capture())
    }

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &Theme,
        bounds: Rectangle,
        _cursor: mouse::Cursor,
    ) -> Vec<canvas::Geometry> {
        let mut frame = canvas::Frame::new(renderer, bounds.size());

        draw_background(&mut frame, self.state);
        frame.with_save(|frame| {
            frame.translate(self.state.pan);
            frame.scale(self.state.zoom);

            draw_connections(self.state, frame);
            draw_drag_preview(self.state, frame);

            for node in &self.state.nodes {
                draw_node(self.state, node, frame);
            }

            draw_start_output_panel(self.state, frame);
        });
        draw_minimap(&mut frame, self.state);

        vec![frame.into_geometry()]
    }

    fn mouse_interaction(
        &self,
        _state: &Self::State,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> mouse::Interaction {
        let Some(local_screen) = cursor.position_in(bounds) else {
            return mouse::Interaction::default();
        };
        let local_world = self.state.screen_to_world(local_screen);

        if let Some(minimap_bounds) = self.state.minimap_bounds() {
            if minimap_bounds.contains(local_screen) {
                return mouse::Interaction::Pointer;
            }
        }

        match self.state.drag {
            DragState::Node { .. } => mouse::Interaction::Grabbing,
            DragState::VariableChip { .. } => mouse::Interaction::Grabbing,
            DragState::StartInputRow { .. } => mouse::Interaction::Grabbing,
            DragState::Connection { .. } => mouse::Interaction::Crosshair,
            DragState::Pan { .. } => mouse::Interaction::Grabbing,
            DragState::Minimap { .. } => mouse::Interaction::Grabbing,
            DragState::None => match self.state.hit_test(local_world) {
                HitTarget::NodeBody(_) => mouse::Interaction::Grab,
                HitTarget::VariableChip { .. } => mouse::Interaction::Grab,
                HitTarget::StartInputRow { .. } => mouse::Interaction::Grab,
                HitTarget::StartTrigger(_)
                | HitTarget::StartAddInput(_)
                | HitTarget::StartOutputRow { .. }
                | HitTarget::ActionKind(_)
                | HitTarget::VariableType { .. } => mouse::Interaction::Pointer,
                HitTarget::InputPort(_) | HitTarget::OutputPort(_) => mouse::Interaction::Pointer,
                HitTarget::Blank => mouse::Interaction::default(),
            },
        }
    }
}

fn draw_background(frame: &mut canvas::Frame, state: &EditorState) {
    let backdrop = canvas::Path::rectangle(Point::ORIGIN, frame.size());
    frame.fill(&backdrop, Color::from_rgb8(0x10, 0x14, 0x1C));

    let dot_color = Color::from_rgba8(0x3A, 0x44, 0x54, 0.35);
    let spacing = 24.0 * state.zoom.max(0.5);
    let radius = 1.2;
    let width = frame.width();
    let height = frame.height();
    let offset_x = state.pan.x.rem_euclid(spacing);
    let offset_y = state.pan.y.rem_euclid(spacing);

    let mut y = -spacing + offset_y;
    while y <= height {
        let mut x = -spacing + offset_x;
        while x <= width {
            let dot = canvas::Path::circle(Point::new(x, y), radius);
            frame.fill(&dot, dot_color);
            x += spacing;
        }
        y += spacing;
    }
}

fn draw_minimap(frame: &mut canvas::Frame, state: &EditorState) {
    let Some(minimap) = state.minimap_bounds() else {
        return;
    };

    let world_bounds = state.world_bounds();
    let border = canvas::Path::rounded_rectangle(
        minimap.position(),
        minimap.size(),
        border::Radius::from(8.0),
    );
    frame.fill(&border, Color::from_rgba8(0x17, 0x1E, 0x2A, 0.92));
    frame.stroke(
        &border,
        canvas::Stroke {
            width: 1.0,
            style: canvas::Style::Solid(Color::from_rgb8(0x3A, 0x46, 0x57)),
            ..canvas::Stroke::default()
        },
    );

    for node in &state.nodes {
        let node_bounds = node.bounds();
        let nx = (node_bounds.x - world_bounds.x) / world_bounds.width;
        let ny = (node_bounds.y - world_bounds.y) / world_bounds.height;
        let nw = node_bounds.width / world_bounds.width;
        let nh = node_bounds.height / world_bounds.height;

        let preview = canvas::Path::rounded_rectangle(
            Point::new(
                minimap.x + nx * minimap.width,
                minimap.y + ny * minimap.height,
            ),
            Size::new(
                (nw * minimap.width).max(3.0),
                (nh * minimap.height).max(3.0),
            ),
            border::Radius::from(2.0),
        );

        frame.fill(
            &preview,
            if state.selected_node == Some(node.id) {
                Color::from_rgb8(0x5A, 0x9B, 0xFF)
            } else {
                Color::from_rgb8(0x6A, 0x75, 0x8A)
            },
        );
    }

    let view_top_left = state.screen_to_world(Point::ORIGIN);
    let view_bottom_right =
        state.screen_to_world(Point::new(state.viewport.width, state.viewport.height));
    let viewport_world = Rectangle::new(
        Point::new(view_top_left.x, view_top_left.y),
        Size::new(
            view_bottom_right.x - view_top_left.x,
            view_bottom_right.y - view_top_left.y,
        ),
    );
    let vx = (viewport_world.x - world_bounds.x) / world_bounds.width;
    let vy = (viewport_world.y - world_bounds.y) / world_bounds.height;
    let vw = viewport_world.width / world_bounds.width;
    let vh = viewport_world.height / world_bounds.height;
    let view_rect = canvas::Path::rounded_rectangle(
        Point::new(
            minimap.x + vx * minimap.width,
            minimap.y + vy * minimap.height,
        ),
        Size::new(
            (vw * minimap.width).max(8.0),
            (vh * minimap.height).max(8.0),
        ),
        border::Radius::from(3.0),
    );
    frame.stroke(
        &view_rect,
        canvas::Stroke {
            width: 1.4,
            style: canvas::Style::Solid(Color::from_rgb8(0x9B, 0xC0, 0xFF)),
            ..canvas::Stroke::default()
        },
    );
}

fn draw_start_output_panel(state: &EditorState, frame: &mut canvas::Frame) {
    let Some(node) = state.selected_start_node() else {
        return;
    };
    let panel_bounds = state.start_output_panel_bounds(node);
    let panel = canvas::Path::rounded_rectangle(
        panel_bounds.position(),
        panel_bounds.size(),
        border::Radius::from(10.0),
    );

    frame.fill(&panel, Color::from_rgba8(0x1B, 0x23, 0x31, 0.96));
    frame.stroke(
        &panel,
        canvas::Stroke {
            width: 1.2,
            style: canvas::Style::Solid(Color::from_rgb8(0x4A, 0x58, 0x6F)),
            ..canvas::Stroke::default()
        },
    );

    frame.fill_text(canvas::Text {
        content: "Outputs".to_owned(),
        position: Point::new(panel_bounds.x + 12.0, panel_bounds.y + 16.0),
        color: Color::from_rgb8(0xD9, 0xE2, 0xF1),
        size: 12.0.into(),
        ..canvas::Text::default()
    });

    for input in &node.start_inputs {
        let Some(row_bounds) = state.start_output_row_bounds(node.id, input.id) else {
            continue;
        };
        let current_target = state
            .start_output_target(node.id, input.id)
            .and_then(|target_id| state.node(target_id))
            .map(|target| target.title.clone())
            .unwrap_or_else(|| "(None)".to_owned());
        let active = node.active_start_input == Some(input.id);

        let row = canvas::Path::rounded_rectangle(
            row_bounds.position(),
            row_bounds.size(),
            border::Radius::from(6.0),
        );
        frame.fill(
            &row,
            if active {
                Color::from_rgb8(0x2B, 0x38, 0x4A)
            } else {
                Color::from_rgb8(0x24, 0x2E, 0x3D)
            },
        );
        frame.stroke(
            &row,
            canvas::Stroke {
                width: 1.0,
                style: canvas::Style::Solid(if active {
                    Color::from_rgb8(0x62, 0xA8, 0xFF)
                } else {
                    Color::from_rgb8(0x3C, 0x48, 0x5B)
                }),
                ..canvas::Stroke::default()
            },
        );

        frame.fill_text(canvas::Text {
            content: format!("{} -> {}", input.label, current_target),
            position: Point::new(row_bounds.x + 10.0, row_bounds.center_y()),
            align_y: alignment::Vertical::Center,
            color: Color::from_rgb8(0xD7, 0xE0, 0xEF),
            size: 11.5.into(),
            ..canvas::Text::default()
        });
    }
}

fn draw_connections(state: &EditorState, frame: &mut canvas::Frame) {
    for connection in &state.connections {
        let Some(from) = state.anchor(connection.from) else {
            continue;
        };
        let Some(to) = state.anchor(connection.to) else {
            continue;
        };

        let path = connection_path(from, to);
        let selected = state.selected_node.is_some_and(|selected| {
            selected == connection.from.node || selected == connection.to.node
        });
        let stroke_width = if selected { 3.1 } else { 2.2 };

        frame.stroke(
            &path,
            canvas::Stroke {
                width: stroke_width,
                style: canvas::Style::Solid(connection.kind.color()),
                line_cap: canvas::LineCap::Round,
                ..canvas::Stroke::default()
            },
        );

        let target_dot = canvas::Path::circle(to, 3.0);
        frame.fill(&target_dot, connection.kind.color());

        if let Some(label) = connection.kind.label() {
            let label_point = Point::new((from.x + to.x) * 0.5, (from.y + to.y) * 0.5 - 10.0);
            let pill = canvas::Path::rounded_rectangle(
                Point::new(label_point.x - 22.0, label_point.y - 9.0),
                Size::new(44.0, 18.0),
                border::Radius::from(7.0),
            );
            frame.fill(&pill, Color::from_rgb8(0x23, 0x2C, 0x39));
            frame.stroke(
                &pill,
                canvas::Stroke {
                    width: 1.0,
                    style: canvas::Style::Solid(Color::from_rgb8(0x4D, 0x58, 0x69)),
                    ..canvas::Stroke::default()
                },
            );
            frame.fill_text(canvas::Text {
                content: label.to_owned(),
                position: label_point,
                color: Color::from_rgb8(0xCC, 0xD6, 0xE7),
                align_x: text::Alignment::Center,
                align_y: alignment::Vertical::Center,
                size: 12.0.into(),
                ..canvas::Text::default()
            });
        }
    }
}

fn draw_drag_preview(state: &EditorState, frame: &mut canvas::Frame) {
    let DragState::Connection { from, current } = state.drag else {
        return;
    };

    let Some(anchor) = state.anchor(from) else {
        return;
    };

    let color = state
        .node(from.node)
        .map(|node| match node.kind {
            NodeKind::Variable => Color::from_rgb8(0xF2, 0xC0, 0x5C),
            _ => Color::from_rgb8(0x7A, 0xA8, 0xF6),
        })
        .unwrap_or(Color::WHITE);
    let color = if matches!(from.kind, PortKind::Variable(_)) {
        Color::from_rgb8(0xF2, 0xC0, 0x5C)
    } else {
        color
    };

    let path = connection_path(anchor, current);
    frame.stroke(
        &path,
        canvas::Stroke {
            width: 2.6,
            style: canvas::Style::Solid(color),
            line_cap: canvas::LineCap::Round,
            ..canvas::Stroke::default()
        },
    );

    let pointer = canvas::Path::circle(current, 4.0);
    frame.fill(&pointer, color);
}

fn draw_output_port(
    frame: &mut canvas::Frame,
    state: &EditorState,
    selected: bool,
    anchor: Point,
    port: PortRef,
    tint: Color,
) {
    let outdent_radius = state.output_port_radius(port);
    let outdent_width = if outdent_radius > 11.0 { 18.0 } else { 11.0 };
    let outdent_height = if outdent_radius > 11.0 { 18.0 } else { 12.0 };
    let outdent = canvas::Path::rounded_rectangle(
        Point::new(
            anchor.x - outdent_width * 0.35,
            anchor.y - outdent_height * 0.5,
        ),
        Size::new(outdent_width, outdent_height),
        border::Radius::from(outdent_height * 0.5),
    );
    frame.fill(&outdent, Color::from_rgb8(0x2D, 0x3A, 0x4D));

    let socket = canvas::Path::circle(anchor, outdent_radius);
    frame.fill(&socket, Color::from_rgb8(0x1A, 0x20, 0x2B));
    frame.stroke(
        &socket,
        canvas::Stroke {
            width: if outdent_radius > 11.0 { 1.8 } else { 1.1 },
            style: canvas::Style::Solid(if selected {
                Color::from_rgb8(0x88, 0xBC, 0xFF)
            } else {
                tint
            }),
            ..canvas::Stroke::default()
        },
    );

    let center_dot = canvas::Path::circle(anchor, (outdent_radius * 0.34).max(2.6));
    frame.fill(&center_dot, Color::from_rgb8(0xA8, 0xB4, 0xC8));
}

fn draw_node(state: &EditorState, node: &Node, frame: &mut canvas::Frame) {
    let bounds = node.bounds();
    let selected = state.selected_node == Some(node.id);
    let hovered = bounds.contains(state.cursor_world);
    let rounding = border::Radius::from(11.0);

    if selected {
        let glow = canvas::Path::rounded_rectangle(
            Point::new(bounds.x - 2.0, bounds.y - 2.0),
            Size::new(bounds.width + 4.0, bounds.height + 4.0),
            border::Radius::from(13.0),
        );
        frame.fill(&glow, Color::from_rgba8(0x4F, 0x95, 0xFF, 0.18));
    }

    let body = canvas::Path::rounded_rectangle(bounds.position(), bounds.size(), rounding);
    frame.fill(&body, Color::from_rgb8(0x1C, 0x22, 0x2E));
    frame.stroke(
        &body,
        canvas::Stroke {
            width: if selected { 2.1 } else { 1.0 },
            style: canvas::Style::Solid(if selected {
                Color::from_rgb8(0x4F, 0x95, 0xFF)
            } else if hovered {
                Color::from_rgb8(0x64, 0x71, 0x84)
            } else {
                Color::from_rgb8(0x4A, 0x55, 0x68)
            }),
            ..canvas::Stroke::default()
        },
    );

    if node.kind == NodeKind::Start {
        let accent = canvas::Path::rounded_rectangle(
            Point::new(bounds.x - 0.6, bounds.y - 0.6),
            Size::new(7.2, bounds.height + 1.2),
            border::Radius {
                top_left: 11.6,
                top_right: 0.0,
                bottom_right: 0.0,
                bottom_left: 11.6,
            },
        );
        frame.fill(&accent, Color::from_rgb8(0xF4, 0x76, 0x3A));
    }

    match node.kind {
        NodeKind::Start => {
            frame.fill_text(canvas::Text {
                content: "Start".to_owned(),
                position: Point::new(bounds.x + 26.0, bounds.y + 22.0),
                align_y: alignment::Vertical::Center,
                color: Color::from_rgb8(0xEF, 0xF3, 0xFA),
                size: 24.0.into(),
                ..canvas::Text::default()
            });

            if let Some(trigger_bounds) = node.start_trigger_bounds() {
                let trigger = canvas::Path::rounded_rectangle(
                    trigger_bounds.position(),
                    trigger_bounds.size(),
                    border::Radius::from(7.0),
                );
                frame.fill(&trigger, Color::from_rgb8(0x28, 0x30, 0x3D));
                frame.stroke(
                    &trigger,
                    canvas::Stroke {
                        width: 1.0,
                        style: canvas::Style::Solid(Color::from_rgb8(0x4B, 0x57, 0x6A)),
                        ..canvas::Stroke::default()
                    },
                );
                frame.fill_text(canvas::Text {
                    content: format!("{}  v", node.start_trigger.label()),
                    position: Point::new(trigger_bounds.center_x(), trigger_bounds.center_y()),
                    align_x: text::Alignment::Center,
                    align_y: alignment::Vertical::Center,
                    color: Color::from_rgb8(0xD7, 0xE0, 0xEE),
                    size: 11.0.into(),
                    ..canvas::Text::default()
                });
            }

            let divider = canvas::Path::line(
                Point::new(bounds.x + 10.0, bounds.y + START_HEADER_HEIGHT),
                Point::new(
                    bounds.x + bounds.width - 10.0,
                    bounds.y + START_HEADER_HEIGHT,
                ),
            );
            frame.stroke(
                &divider,
                canvas::Stroke {
                    width: 1.0,
                    style: canvas::Style::Solid(Color::from_rgb8(0x3E, 0x4A, 0x5C)),
                    ..canvas::Stroke::default()
                },
            );

            for input in &node.start_inputs {
                if let Some(row_bounds) = node.start_input_bounds(input.id) {
                    let active = node.active_start_input == Some(input.id);
                    let row = canvas::Path::rounded_rectangle(
                        row_bounds.position(),
                        row_bounds.size(),
                        border::Radius::from(6.0),
                    );
                    frame.fill(
                        &row,
                        if active {
                            Color::from_rgb8(0x2C, 0x39, 0x4B)
                        } else {
                            Color::from_rgb8(0x25, 0x2D, 0x3A)
                        },
                    );
                    frame.stroke(
                        &row,
                        canvas::Stroke {
                            width: if active { 1.2 } else { 1.0 },
                            style: canvas::Style::Solid(if active {
                                Color::from_rgb8(0x5F, 0xA8, 0xFF)
                            } else {
                                Color::from_rgb8(0x3F, 0x4A, 0x5A)
                            }),
                            ..canvas::Stroke::default()
                        },
                    );

                    for index in 0..3 {
                        let dot = canvas::Path::circle(
                            Point::new(row_bounds.x + 9.0, row_bounds.y + 8.0 + index as f32 * 7.0),
                            1.0,
                        );
                        frame.fill(&dot, Color::from_rgb8(0x7A, 0x86, 0x9A));
                    }

                    frame.fill_text(canvas::Text {
                        content: input.label.clone(),
                        position: Point::new(row_bounds.x + 20.0, row_bounds.center_y()),
                        align_y: alignment::Vertical::Center,
                        color: Color::from_rgb8(0xDE, 0xE6, 0xF4),
                        size: 13.0.into(),
                        ..canvas::Text::default()
                    });

                    if let Some(anchor) = node.start_input_output_anchor(input.id) {
                        draw_output_port(
                            frame,
                            state,
                            selected,
                            anchor,
                            PortRef {
                                node: node.id,
                                side: PortSide::Output,
                                kind: PortKind::StartInput(input.id),
                            },
                            Color::from_rgb8(0x8A, 0x99, 0xB2),
                        );
                    }
                }
            }

            if let Some(add_bounds) = node.start_add_input_bounds() {
                let add_row = canvas::Path::rounded_rectangle(
                    add_bounds.position(),
                    add_bounds.size(),
                    border::Radius::from(6.0),
                );
                frame.fill(&add_row, Color::from_rgb8(0x22, 0x2A, 0x36));
                frame.stroke(
                    &add_row,
                    canvas::Stroke {
                        width: 1.0,
                        style: canvas::Style::Solid(Color::from_rgb8(0x3B, 0x47, 0x58)),
                        ..canvas::Stroke::default()
                    },
                );
                frame.fill_text(canvas::Text {
                    content: "+ Add input".to_owned(),
                    position: Point::new(add_bounds.x + 10.0, add_bounds.center_y()),
                    align_y: alignment::Vertical::Center,
                    color: Color::from_rgb8(0xAF, 0xBE, 0xD2),
                    size: 13.0.into(),
                    ..canvas::Text::default()
                });
            }
        }
        NodeKind::Action => {
            let chip = canvas::Path::rounded_rectangle(
                Point::new(bounds.x + 14.0, bounds.y + 10.0),
                Size::new(64.0, 20.0),
                border::Radius::from(10.0),
            );
            frame.fill(&chip, node.kind.chip_color());
            frame.fill_text(canvas::Text {
                content: "ACTION".to_owned(),
                position: Point::new(bounds.x + 46.0, bounds.y + 20.5),
                align_x: text::Alignment::Center,
                align_y: alignment::Vertical::Center,
                color: Color::from_rgb8(0x08, 0x0D, 0x15),
                size: 10.5.into(),
                ..canvas::Text::default()
            });

            frame.fill_text(canvas::Text {
                content: node.title.clone(),
                position: Point::new(bounds.x + 14.0, bounds.y + 44.0),
                color: Color::from_rgb8(0xEE, 0xF2, 0xF9),
                size: 16.0.into(),
                ..canvas::Text::default()
            });

            if let Some(action_bounds) = node.action_kind_bounds() {
                let action_chip = canvas::Path::rounded_rectangle(
                    action_bounds.position(),
                    action_bounds.size(),
                    border::Radius::from(7.0),
                );
                frame.fill(&action_chip, Color::from_rgb8(0x28, 0x30, 0x3D));
                frame.stroke(
                    &action_chip,
                    canvas::Stroke {
                        width: 1.0,
                        style: canvas::Style::Solid(Color::from_rgb8(0x4B, 0x57, 0x6A)),
                        ..canvas::Stroke::default()
                    },
                );
                frame.fill_text(canvas::Text {
                    content: format!("{}  v", node.action_kind.label()),
                    position: Point::new(action_bounds.center_x(), action_bounds.center_y()),
                    align_x: text::Alignment::Center,
                    align_y: alignment::Vertical::Center,
                    color: Color::from_rgb8(0xD7, 0xE0, 0xEE),
                    size: 11.0.into(),
                    ..canvas::Text::default()
                });
            }

            let section = canvas::Path::line(
                Point::new(bounds.x + 12.0, bounds.y + 54.0),
                Point::new(bounds.x + bounds.width - 12.0, bounds.y + 54.0),
            );
            frame.stroke(
                &section,
                canvas::Stroke {
                    width: 1.0,
                    style: canvas::Style::Solid(Color::from_rgb8(0x3E, 0x4A, 0x5C)),
                    ..canvas::Stroke::default()
                },
            );

            if node.variables.is_empty() {
                frame.fill_text(canvas::Text {
                    content: "Drag variable output here".to_owned(),
                    position: Point::new(bounds.x + 16.0, bounds.y + 74.0),
                    color: Color::from_rgb8(0x8F, 0x9C, 0xB2),
                    size: 12.0.into(),
                    ..canvas::Text::default()
                });
            }
        }
        NodeKind::Variable => {
            let chip = canvas::Path::rounded_rectangle(
                Point::new(bounds.x + 16.0, bounds.y + 14.0),
                Size::new(48.0, 20.0),
                border::Radius::from(10.0),
            );
            frame.fill(&chip, node.kind.chip_color());
            frame.fill_text(canvas::Text {
                content: "VAR".to_owned(),
                position: Point::new(bounds.x + 40.0, bounds.y + 24.0),
                align_x: text::Alignment::Center,
                align_y: alignment::Vertical::Center,
                color: Color::from_rgb8(0x09, 0x0E, 0x14),
                size: 10.0.into(),
                ..canvas::Text::default()
            });
            frame.fill_text(canvas::Text {
                content: node.title.clone(),
                position: Point::new(bounds.x + 16.0, bounds.y + 52.0),
                color: Color::from_rgb8(0xEE, 0xF2, 0xF9),
                size: 16.0.into(),
                ..canvas::Text::default()
            });
            frame.fill_text(canvas::Text {
                content: "Drag from outdent to map variable".to_owned(),
                position: Point::new(bounds.x + 16.0, bounds.y + bounds.height - 18.0),
                color: Color::from_rgb8(0x97, 0xA4, 0xB9),
                size: 11.5.into(),
                ..canvas::Text::default()
            });
        }
    }

    if let Some(input_anchor) = node.input_anchor() {
        let socket = canvas::Path::circle(input_anchor, 6.0);
        frame.fill(&socket, Color::from_rgb8(0x18, 0x1D, 0x27));
        frame.stroke(
            &socket,
            canvas::Stroke {
                width: 1.2,
                style: canvas::Style::Solid(Color::from_rgb8(0x7B, 0x86, 0x98)),
                ..canvas::Stroke::default()
            },
        );
    }

    if let Some(output_anchor) = node.output_anchor() {
        draw_output_port(
            frame,
            state,
            selected,
            output_anchor,
            PortRef {
                node: node.id,
                side: PortSide::Output,
                kind: PortKind::Primary,
            },
            Color::from_rgb8(0x7B, 0x86, 0x98),
        );
    }

    for variable in &node.variables {
        let variable_bounds = node.variable_bounds(variable);
        let dragging_this = matches!(
            state.drag,
            DragState::VariableChip {
                node: drag_node,
                variable: drag_variable,
                ..
            } if drag_node == node.id && drag_variable == variable.id
        );

        let chip = canvas::Path::rounded_rectangle(
            variable_bounds.position(),
            variable_bounds.size(),
            border::Radius::from(8.0),
        );
        frame.fill(
            &chip,
            if dragging_this {
                Color::from_rgb8(0x2C, 0x3A, 0x4E)
            } else {
                Color::from_rgb8(0x2A, 0x35, 0x45)
            },
        );
        frame.stroke(
            &chip,
            canvas::Stroke {
                width: 1.0,
                style: canvas::Style::Solid(Color::from_rgb8(0x5A, 0x68, 0x81)),
                ..canvas::Stroke::default()
            },
        );
        frame.fill_text(canvas::Text {
            content: variable.label.clone(),
            position: Point::new(variable_bounds.x + 8.0, variable_bounds.center_y()),
            align_y: alignment::Vertical::Center,
            color: Color::from_rgb8(0xD7, 0xE0, 0xF0),
            size: 12.0.into(),
            ..canvas::Text::default()
        });

        let type_bounds = node.variable_type_bounds(variable);
        let type_chip = canvas::Path::rounded_rectangle(
            type_bounds.position(),
            type_bounds.size(),
            border::Radius::from(4.0),
        );
        frame.fill(&type_chip, Color::from_rgb8(0x39, 0x47, 0x5D));
        frame.stroke(
            &type_chip,
            canvas::Stroke {
                width: 1.0,
                style: canvas::Style::Solid(Color::from_rgb8(0x5C, 0x6C, 0x87)),
                ..canvas::Stroke::default()
            },
        );
        frame.fill_text(canvas::Text {
            content: variable.value_type.short_label().to_owned(),
            position: Point::new(type_bounds.center_x(), type_bounds.center_y()),
            align_x: text::Alignment::Center,
            align_y: alignment::Vertical::Center,
            color: Color::from_rgb8(0xE2, 0xE9, 0xF8),
            size: 10.0.into(),
            ..canvas::Text::default()
        });

        let output_anchor = node.variable_output_anchor(variable);
        draw_output_port(
            frame,
            state,
            selected,
            output_anchor,
            PortRef {
                node: node.id,
                side: PortSide::Output,
                kind: PortKind::Variable(variable.id),
            },
            Color::from_rgb8(0x98, 0xA7, 0xC0),
        );
    }
}

fn connection_path(from: Point, to: Point) -> canvas::Path {
    let curvature = ((to.x - from.x).abs() * 0.45).clamp(36.0, 180.0);

    canvas::Path::new(|builder| {
        builder.move_to(from);
        builder.bezier_curve_to(
            Point::new(from.x + curvature, from.y),
            Point::new(to.x - curvature, to.y),
            to,
        );
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn viewport() -> Size {
        Size::new(1280.0, 800.0)
    }

    fn pointer_moved(position: Point) -> EditorEvent {
        EditorEvent::PointerMoved {
            position,
            viewport: viewport(),
        }
    }

    fn left_pressed(position: Point) -> EditorEvent {
        EditorEvent::LeftPressed {
            position,
            viewport: viewport(),
        }
    }

    fn left_released(position: Point) -> EditorEvent {
        EditorEvent::LeftReleased {
            position,
            viewport: viewport(),
        }
    }

    fn right_pressed(position: Point) -> EditorEvent {
        EditorEvent::RightPressed {
            position,
            viewport: viewport(),
        }
    }

    fn variable_node(state: &EditorState) -> NodeId {
        state
            .nodes
            .iter()
            .find(|node| node.kind == NodeKind::Variable)
            .map(|node| node.id)
            .expect("variable node should exist")
    }

    fn action_node(state: &EditorState) -> NodeId {
        state
            .nodes
            .iter()
            .find(|node| node.kind == NodeKind::Action)
            .map(|node| node.id)
            .expect("action node should exist")
    }

    fn start_node(state: &EditorState) -> NodeId {
        state
            .nodes
            .iter()
            .find(|node| node.kind == NodeKind::Start)
            .map(|node| node.id)
            .expect("start node should exist")
    }

    #[test]
    fn right_click_blank_opens_context_menu() {
        let mut state = EditorState::new_demo();
        let blank = Point::new(24.0, 24.0);
        assert_eq!(state.hit_test(blank), HitTarget::Blank);

        state.on_event(right_pressed(blank));

        assert!(state.context_menu.is_some());
        assert_eq!(
            state.context_menu.as_ref().map(|menu| menu.position),
            Some(blank)
        );
    }

    #[test]
    fn context_action_adds_node_at_menu_position() {
        let mut state = EditorState::new_demo();
        let before = state.nodes.len();
        state.on_event(right_pressed(Point::new(50.0, 60.0)));

        state.apply_context_action(ContextAction::AddAction);

        assert_eq!(state.nodes.len(), before + 1);
        let created = state
            .nodes
            .last()
            .expect("newly created node should be present");
        assert_eq!(created.kind, NodeKind::Action);
    }

    #[test]
    fn start_trigger_click_opens_menu_and_apply_changes_value() {
        let mut state = EditorState::new_demo();
        let start_id = start_node(&state);
        let trigger_center = state
            .node(start_id)
            .and_then(Node::start_trigger_bounds)
            .map(|bounds| Point::new(bounds.center_x(), bounds.center_y()))
            .expect("trigger bounds should exist");

        state.on_event(left_pressed(trigger_center));

        assert_eq!(
            state.start_trigger_menu.as_ref().map(|menu| menu.node),
            Some(start_id)
        );
        state.apply_start_trigger(start_id, StartTrigger::OnSubmit);
        assert_eq!(
            state.node(start_id).map(|node| node.start_trigger),
            Some(StartTrigger::OnSubmit)
        );
        assert!(state.start_trigger_menu.is_none());
    }

    #[test]
    fn start_input_rows_can_be_reordered_by_drag() {
        let mut state = EditorState::new_demo();
        let start_id = start_node(&state);
        let initial = state
            .node(start_id)
            .map(|node| {
                node.start_inputs
                    .iter()
                    .map(|input| input.id)
                    .collect::<Vec<_>>()
            })
            .expect("start node");
        assert!(initial.len() >= 2);

        let first_id = initial[0];
        let second_id = initial[1];
        let first_row = state
            .node(start_id)
            .and_then(|node| node.start_input_bounds(first_id))
            .expect("first row");
        let second_row = state
            .node(start_id)
            .and_then(|node| node.start_input_bounds(second_id))
            .expect("second row");

        let start_drag = Point::new(first_row.center_x(), first_row.center_y());
        let drag_to = Point::new(start_drag.x, second_row.center_y() + 2.0);

        state.on_event(left_pressed(start_drag));
        state.on_event(pointer_moved(drag_to));
        state.on_event(left_released(drag_to));

        let reordered = state
            .node(start_id)
            .map(|node| {
                node.start_inputs
                    .iter()
                    .map(|input| input.id)
                    .collect::<Vec<_>>()
            })
            .expect("start node");
        assert_eq!(reordered[0], second_id);
        assert_eq!(reordered[1], first_id);
    }

    #[test]
    fn start_output_panel_click_cycles_output_target_for_input() {
        let mut state = EditorState::new_demo();
        let start_id = start_node(&state);
        let first_input = state
            .node(start_id)
            .and_then(|node| node.start_inputs.first().map(|input| input.id))
            .expect("first input");
        let second_action = state.add_node(NodeKind::Action, Point::new(760.0, 110.0));
        state.selected_node = Some(start_id);

        let row_center = state
            .start_output_row_bounds(start_id, first_input)
            .map(|bounds| Point::new(bounds.center_x(), bounds.center_y()))
            .expect("start output panel row");

        let before = state.start_output_target(start_id, first_input);
        state.on_event(left_pressed(row_center));
        let first_target = state.start_output_target(start_id, first_input);
        assert_ne!(before, first_target);

        state.on_event(left_pressed(row_center));
        let second_target = state.start_output_target(start_id, first_input);
        assert_ne!(first_target, second_target);
        assert!(
            second_target.is_none()
                || second_target == Some(action_node(&state))
                || second_target == Some(second_action)
        );
    }

    #[test]
    fn action_kind_click_opens_menu_and_apply_changes_value() {
        let mut state = EditorState::new_demo();
        let action_id = action_node(&state);
        let action_kind_center = state
            .node(action_id)
            .and_then(Node::action_kind_bounds)
            .map(|bounds| Point::new(bounds.center_x(), bounds.center_y()))
            .expect("action kind bounds");

        state.on_event(left_pressed(action_kind_center));

        assert_eq!(
            state.action_kind_menu.as_ref().map(|menu| menu.node),
            Some(action_id)
        );
        state.apply_action_kind(action_id, ActionKind::Match);
        let selected_kind = state
            .node(action_id)
            .map(|node| node.action_kind)
            .expect("action node");
        assert_eq!(selected_kind, ActionKind::Match);
        assert!(state.action_kind_menu.is_none());
    }

    #[test]
    fn start_input_connection_replacement_keeps_single_target() {
        let mut state = EditorState::new_demo();
        let start_id = start_node(&state);
        let input_id = state
            .node(start_id)
            .and_then(|node| node.start_inputs.first().map(|input| input.id))
            .expect("start input");
        let other_action = state.add_node(NodeKind::Action, Point::new(900.0, 140.0));

        let first_input = state
            .input_port(action_node(&state))
            .expect("first action input");
        let second_input = state.input_port(other_action).expect("second action input");
        let from = PortRef {
            node: start_id,
            side: PortSide::Output,
            kind: PortKind::StartInput(input_id),
        };

        state.connect(from, first_input, ConnectionKind::Flow);
        state.connect(from, second_input, ConnectionKind::Flow);

        let outgoing = state
            .connections
            .iter()
            .filter(|connection| {
                connection.kind == ConnectionKind::Flow
                    && connection.from.node == start_id
                    && connection.from.kind == PortKind::StartInput(input_id)
            })
            .count();

        assert_eq!(outgoing, 1);
    }

    #[test]
    fn start_output_panel_detects_hit_when_selected() {
        let mut state = EditorState::new_demo();
        let start_id = start_node(&state);
        let input_id = state
            .node(start_id)
            .and_then(|node| node.start_inputs.first().map(|input| input.id))
            .expect("input");

        state.selected_node = Some(start_id);
        let row_center = state
            .start_output_row_bounds(start_id, input_id)
            .map(|bounds| Point::new(bounds.center_x(), bounds.center_y()))
            .expect("row bounds");

        assert_eq!(
            state.hit_test(row_center),
            HitTarget::StartOutputRow {
                node: start_id,
                input: input_id
            }
        );
    }

    #[test]
    fn variable_type_hit_target_detects_badge() {
        let mut state = EditorState::new_demo();
        let variable_id = variable_node(&state);
        let action_id = action_node(&state);
        let origin = state
            .output_port(variable_id)
            .and_then(|port| state.anchor(port))
            .expect("variable output anchor");
        let target = state
            .input_port(action_id)
            .and_then(|port| state.anchor(port))
            .expect("action input anchor");

        state.on_event(left_pressed(origin));
        state.on_event(pointer_moved(target));
        state.on_event(left_released(target));
        state.apply_variable_drop_option(VariableDropOption::SetInput);

        let handle_id = state
            .node(action_id)
            .and_then(|node| node.variables.first().map(|variable| variable.id))
            .expect("variable handle");
        let badge_center = state
            .variable_type_bounds(action_id, handle_id)
            .map(|bounds| Point::new(bounds.center_x(), bounds.center_y()))
            .expect("badge");

        assert_eq!(
            state.hit_test(badge_center),
            HitTarget::VariableType {
                node: action_id,
                variable: handle_id
            }
        );
    }

    #[test]
    fn variable_drop_on_blank_opens_blank_menu() {
        let mut state = EditorState::new_demo();
        let variable_id = variable_node(&state);
        let origin = state
            .output_port(variable_id)
            .and_then(|port| state.anchor(port))
            .expect("variable output anchor");
        let blank = Point::new(820.0, 510.0);

        state.on_event(left_pressed(origin));
        state.on_event(pointer_moved(blank));
        state.on_event(left_released(blank));

        let menu = state
            .variable_drop_menu
            .as_ref()
            .expect("variable drop menu should open");
        assert_eq!(menu.context, VariableDropContext::Blank);
        assert_eq!(state.variable_drop_options(), &BLANK_DROP_OPTIONS);
    }

    #[test]
    fn variable_drop_on_existing_node_opens_existing_menu() {
        let mut state = EditorState::new_demo();
        let variable_id = variable_node(&state);
        let action_id = action_node(&state);
        let origin = state
            .output_port(variable_id)
            .and_then(|port| state.anchor(port))
            .expect("variable output anchor");
        let target = state
            .input_port(action_id)
            .and_then(|port| state.anchor(port))
            .expect("action input anchor");

        state.on_event(left_pressed(origin));
        state.on_event(pointer_moved(target));
        state.on_event(left_released(target));

        let menu = state
            .variable_drop_menu
            .as_ref()
            .expect("variable drop menu should open");
        assert_eq!(menu.context, VariableDropContext::Existing(action_id));
        assert_eq!(state.variable_drop_options(), &EXISTING_DROP_OPTIONS);
    }

    #[test]
    fn choosing_existing_drop_option_creates_connection() {
        let mut state = EditorState::new_demo();
        let variable_id = variable_node(&state);
        let action_id = action_node(&state);
        let origin = state
            .output_port(variable_id)
            .and_then(|port| state.anchor(port))
            .expect("variable output anchor");
        let target = state
            .input_port(action_id)
            .and_then(|port| state.anchor(port))
            .expect("action input anchor");

        state.on_event(left_pressed(origin));
        state.on_event(pointer_moved(target));
        state.on_event(left_released(target));
        state.apply_variable_drop_option(VariableDropOption::SetInput);

        assert!(state.connections.iter().any(|connection| {
            connection.from.node == variable_id
                && connection.to.node == action_id
                && connection.kind == ConnectionKind::VariableSet
        }));
        assert_eq!(
            state
                .node(action_id)
                .map(|node| node.variables.len())
                .unwrap_or_default(),
            1
        );
    }

    #[test]
    fn choosing_blank_drop_option_creates_action_and_connection() {
        let mut state = EditorState::new_demo();
        let variable_id = variable_node(&state);
        let origin = state
            .output_port(variable_id)
            .and_then(|port| state.anchor(port))
            .expect("variable output anchor");
        let blank = Point::new(780.0, 470.0);
        let before_count = state.nodes.len();

        state.on_event(left_pressed(origin));
        state.on_event(pointer_moved(blank));
        state.on_event(left_released(blank));
        state.apply_variable_drop_option(VariableDropOption::CreateSetAction);

        assert_eq!(state.nodes.len(), before_count + 1);
        let created = state.nodes.last().expect("created action node");
        assert_eq!(created.kind, NodeKind::Action);
        assert_eq!(created.title, "Set Variable");
        assert!(state.connections.iter().any(|connection| {
            connection.from.node == variable_id
                && connection.to.node == created.id
                && connection.kind == ConnectionKind::VariableSet
        }));
        assert_eq!(created.variables.len(), 1);
    }

    #[test]
    fn can_attach_multiple_flow_sources_to_same_target() {
        let mut state = EditorState::new_demo();
        let target = action_node(&state);
        let source_two = state.add_node(NodeKind::Action, Point::new(120.0, 340.0));
        let input = state.input_port(target).expect("target should have input");

        let source_one_output = state.output_port(1).expect("start output");
        let source_two_output = state.output_port(source_two).expect("action output");

        state.connect(source_one_output, input, ConnectionKind::Flow);
        state.connect(source_two_output, input, ConnectionKind::Flow);

        let incoming = state
            .connections
            .iter()
            .filter(|connection| connection.to == input && connection.kind == ConnectionKind::Flow)
            .count();

        assert!(incoming >= 2);
    }

    #[test]
    fn variable_chip_can_move_with_drag() {
        let mut state = EditorState::new_demo();
        let variable_id = variable_node(&state);
        let action_id = action_node(&state);
        let origin = state
            .output_port(variable_id)
            .and_then(|port| state.anchor(port))
            .expect("variable output anchor");
        let target = state
            .input_port(action_id)
            .and_then(|port| state.anchor(port))
            .expect("action input anchor");

        state.on_event(left_pressed(origin));
        state.on_event(pointer_moved(target));
        state.on_event(left_released(target));
        state.apply_variable_drop_option(VariableDropOption::SetInput);

        let variable = state
            .node(action_id)
            .and_then(|node| node.variables.first())
            .cloned()
            .expect("variable handle should exist");
        let chip_bounds = state
            .variable_bounds(action_id, variable.id)
            .expect("chip bounds");
        let start = Point::new(chip_bounds.x + 5.0, chip_bounds.y + 5.0);
        let end = Point::new(start.x, start.y + 18.0);

        state.on_event(left_pressed(start));
        state.on_event(pointer_moved(end));
        state.on_event(left_released(end));

        let moved = state
            .node(action_id)
            .and_then(|node| node.variables.iter().find(|item| item.id == variable.id))
            .expect("moved variable handle");

        assert!(moved.y_offset > variable.y_offset);
    }

    #[test]
    fn variable_type_badge_cycles_dynamic_value_type() {
        let mut state = EditorState::new_demo();
        let variable_id = variable_node(&state);
        let action_id = action_node(&state);
        let origin = state
            .output_port(variable_id)
            .and_then(|port| state.anchor(port))
            .expect("variable output anchor");
        let target = state
            .input_port(action_id)
            .and_then(|port| state.anchor(port))
            .expect("action input anchor");

        state.on_event(left_pressed(origin));
        state.on_event(pointer_moved(target));
        state.on_event(left_released(target));
        state.apply_variable_drop_option(VariableDropOption::SetInput);

        let (variable_handle_id, before_kind) = state
            .node(action_id)
            .and_then(|node| node.variables.first())
            .map(|variable| (variable.id, variable.value_type))
            .expect("variable handle");
        let badge_center = state
            .variable_type_bounds(action_id, variable_handle_id)
            .map(|bounds| Point::new(bounds.center_x(), bounds.center_y()))
            .expect("variable type badge");

        state.on_event(left_pressed(badge_center));

        let after_kind = state
            .node(action_id)
            .and_then(|node| node.variables.first())
            .map(|variable| variable.value_type)
            .expect("variable handle");
        assert_ne!(before_kind, after_kind);
    }
}
