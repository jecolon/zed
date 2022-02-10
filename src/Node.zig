const Token = @import("Token.zig");

pub const Type = union(enum) {
    boolean: bool,
    float: f64,
    func_return: *Node,
    ident: []const u8,
    int: isize,
    list: []Node,
    map: []Entry,
    loop_break,
    loop_continue,
    nil,
    stmt_end,
    string: []const u8,
    uint: usize,

    assign: Define,
    call: Call,
    conditional: Conditional,
    define: Define,
    func: Function,
    infix: Infix,
    loop: Loop,
    prefix: Prefix,
    range: Range,
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

const Define = struct {
    name: *Node,
    value: *Node,
};

pub const Entry = struct {
    key: Node,
    value: Node,
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

const Subscript = struct {
    container: *Node,
    index: *Node,
};
