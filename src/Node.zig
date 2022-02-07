const Token = @import("Token.zig");

pub const Type = union(enum) {
    boolean: bool,
    float: f64,
    int: isize,
    nil,
    stmt_end,
    string: []const u8,
    uint: usize,

    conditional: Conditional,
    infix: Infix,
    prefix: Prefix,
};

offset: u16,
token_index: u16,
ty: Type,

const Node = @This();

pub fn new(ty: Type, token_index: u16, offset: u16) Node {
    return .{
        .offset = offset,
        .token_index = token_index,
        .ty = ty,
    };
}

const Conditional = struct {
    condition: *Node,
    then_branch: []Node,
    else_branch: []Node,
};

const Prefix = struct {
    op: Token.Tag,
    operand: *Node,
};

const Infix = struct {
    left: *Node,
    op: Token.Tag,
    right: *Node,
};
