const std = @import("std");

const Value = @import("Value.zig");

const Scope = @This();

pub const Type = enum {
    block,
    function,
    loop,
};

allocator: std.mem.Allocator,
columns: *std.ArrayList(Value) = undefined,
map: std.StringHashMap(Value),
rec_buf: [1024 * 64]u8 = undefined,
rec_ranges: std.AutoHashMap(u8, void) = undefined,
record: []const u8 = undefined,
ty: Type,

pub fn init(allocator: std.mem.Allocator, ty: Type) Scope {
    return Scope{
        .allocator = allocator,
        .map = std.StringHashMap(Value).init(allocator),
        .ty = ty,
    };
}

pub fn deinit(self: *Scope) void {
    var iter = self.map.iterator();
    while (iter.next()) |entry| {
        entry.value_ptr.deinit(self.allocator);
        self.allocator.free(entry.key_ptr.*);
    }
    self.map.deinit();
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
    const key_copy = try self.allocator.dupe(u8, key);
    try self.map.put(key_copy, try value.copy(self.allocator));
}

pub fn update(self: *Scope, key: []const u8, value: Value) !void {
    if (read_only.has(key)) return error.ReadOnlyGlobal;
    if (self.map.get(key)) |old_value| old_value.deinit(self.allocator);
    try self.map.put(key, try value.copy(self.allocator));
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
