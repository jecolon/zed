pub const Type = union(enum) {
    boolean: bool,
};

node_index: u16,
offset: u16,
ty: Type,

const Value = @This();

pub fn new(ty: Type, node_index: u16, offset: u16) Value {
    return .{
        .node_index = node_index,
        .offset = offset,
        .ty = ty,
    };
}
