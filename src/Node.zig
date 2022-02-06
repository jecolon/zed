pub const Type = union(enum) {
    boolean: bool,
    float: f64,
    int: isize,
    stmt_end,
    string: []const u8,
    uint: usize,
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
