pub const Type = union(enum) {
    boolean: bool,
    float: f64,
    int: isize,
    string: []const u8,
    uint: usize,
};

offset: u16,
ty: Type,

const Value = @This();

pub fn new(ty: Type, offset: u16) Value {
    return .{ .offset = offset, .ty = ty };
}
