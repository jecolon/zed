const std = @import("std");

const Value = @import("Value.zig");

const Scope = @This();

pub const Type = enum {
    block,
    function,
    loop,
};

columns: *std.ArrayList(Value) = undefined,
map: std.StringHashMap(Value),
rec_buf: [1024 * 64]u8 = undefined,
rec_ranges: std.AutoHashMap(u8, void) = undefined,
record: []const u8 = undefined,
ty: Type,

pub fn init(allocator: std.mem.Allocator, ty: Type) Scope {
    return Scope{
        .map = std.StringHashMap(Value).init(allocator),
        .ty = ty,
    };
}

const globals = std.ComptimeStringMap(void, .{
    .{ "@rec", {} },
    .{ "@cols", {} },
    .{ "@ifs", {} },
    .{ "@irs", {} },
    .{ "@ofs", {} },
    .{ "@ors", {} },
    .{ "@rnum", {} },
    .{ "@frnum", {} },
    .{ "@ranges", {} },
    .{ "@file", {} },
});

const read_only = std.ComptimeStringMap(void, .{
    .{ "@rnum", {} },
    .{ "@frnum", {} },
    .{ "atan2", {} },
    .{ "cos", {} },
    .{ "exp", {} },
    .{ "int", {} },
    .{ "log", {} },
    .{ "print", {} },
    .{ "rand", {} },
    .{ "sin", {} },
    .{ "sqrt", {} },
});

pub fn isDefined(self: Scope, key: []const u8) bool {
    if (globals.has(key)) return true;
    return self.map.contains(key);
}

pub fn load(self: Scope, key: []const u8) ?Value {
    return self.map.get(key);
}

pub fn store(self: *Scope, key: []const u8, value: Value) !void {
    try self.map.put(key, value);
}

pub fn update(self: *Scope, key: []const u8, value: Value) !void {
    if (read_only.has(key)) return error.ReadOnlyGlobal;
    try self.map.put(key, value);
}

// Debug
pub fn dump(self: Scope) void {
    std.debug.print("\n*** Scope Dump Begin ***\n", .{});
    var iter = self.map.iterator();
    while (iter.next()) |entry| {
        std.debug.print("\t{s}: {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }
    std.debug.print("*** Scope Dump End ***\n", .{});
}
