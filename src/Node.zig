const Token = @import("Token.zig");

pub const Type = union(enum) {
    boolean: bool,
    builtin: Token.Tag,
    float: f64,
    func_return: *Node,
    global: Token.Tag,
    ident: []const u8,
    int: i32,
    list: []Node,
    map: []Entry,
    loop_break,
    loop_continue,
    nil,
    stmt_end,
    string: []const Segment,
    uint: u32,

    assign: Assign,
    call: Call,
    conditional: Conditional,
    define: Assign,
    event: Event,
    func: Function,
    infix: Infix,
    loop: Loop,
    prefix: Prefix,
    range: Range,
    rec_range: RecRange,
    redir: Redir,
    subscript: Subscript,
};

offset: u16,
ty: Type,

const Node = @This();

pub fn new(ty: Type, offset: u16) Node {
    return .{ .offset = offset, .ty = ty };
}

const Call = struct {
    args: []Node,
    callee: *Node,
};

const Conditional = struct {
    condition: *Node,
    then_branch: []Node,
    else_branch: []Node,
};

pub const Combo = enum {
    none,
    add,
    sub,
    mul,
    div,
    mod,
};

const Assign = struct {
    combo: Combo = .none,
    lvalue: *Node,
    rvalue: *Node,
};

pub const Entry = struct {
    key: Node,
    value: Node,
};

const EventType = enum {
    init,
    file,
    rec,
    exit,
};

const Event = struct {
    nodes: []Node,
    ty: EventType,
};

const Function = struct {
    name: []const u8 = "",
    params: [][]const u8,
    body: []Node,
};

const Infix = struct {
    left: *Node,
    op: Token.Tag,
    right: *Node,
};

const Loop = struct {
    condition: *Node,
    body: []Node,
    is_do: bool,
};

const Prefix = struct {
    op: Token.Tag,
    operand: *Node,
};

const Range = struct {
    inclusive: bool,
    from: *Node,
    to: *Node,
};

const RecRange = struct {
    from: ?*Node,
    to: ?*Node,
    action: []Node,
    id: u8,
    exclusive: bool,
};

const Redir = struct {
    expr: *Node,
    file: *Node,
    clobber: bool,
};

pub const Ipol = struct {
    format: ?[]const u8,
    nodes: []Node,
    offset: u16,
};

pub const Segment = union(enum) {
    ipol: Ipol,
    plain: []const u8,
};

const Subscript = struct {
    container: *Node,
    index: *Node,
};
