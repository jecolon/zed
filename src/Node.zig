pub const Type = union(enum) {
    boolean: bool,
    stmt_end,
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
