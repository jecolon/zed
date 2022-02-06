pub const Type = union(enum) {
    boolean: bool,
    float: f64,
    int: isize,
    string: []const u8,
    uint: usize,
};

node_index: u16,
offset: u16,
ty: Type,

const Value = @This();

pub fn new(ty: Type, node_index: usize, offset: u16) Value {
    return .{
        .node_index = @intCast(u16, node_index),
        .offset = offset,
        .ty = ty,
    };
}
