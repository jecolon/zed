const std = @import("std");

const Value = @import("Value.zig");

const Scope = @This();

pub const Type = enum {
    block,
    function,
    loop,
};

map: std.StringHashMap(Value),
ty: Type,

pub fn init(allocator: std.mem.Allocator, ty: Type) Scope {
    return Scope{ .map = std.StringHashMap(Value).init(allocator), .ty = ty };
}

// Debug
pub fn dump(self: Scope) void {
    var iter = self.map.iterator();
    while (iter.next()) |entry| std.debug.print("\t{s}: {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
}
