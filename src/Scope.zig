const std = @import("std");

const Value = @import("Value.zig");

const Scope = @This();

allocator: std.mem.Allocator,
break_point: bool = false,
map: std.StringHashMap(Value),
parent: ?*Scope,

pub fn init(allocator: std.mem.Allocator, parent: ?*Scope) Scope {
    return Scope{
        .allocator = allocator,
        .map = std.StringHashMap(Value).init(allocator),
        .parent = parent,
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

pub fn isDefined(self: Scope, key: []const u8) bool {
    if (self.map.contains(key)) {
        return true;
    } else if (self.parent) |parent| {
        return parent.isDefined(key);
    } else {
        return false;
    }
}

pub fn load(self: Scope, key: []const u8) ?Value {
    if (self.map.get(key)) |value| {
        return value;
    } else if (self.parent) |parent| {
        return parent.load(key);
    } else {
        return null;
    }
}

pub fn store(self: *Scope, key: []const u8, value: Value) !void {
    const key_copy = try self.allocator.dupe(u8, key);
    try self.map.put(key_copy, try value.copy(self.allocator));
}

pub fn update(self: *Scope, key: []const u8, value: Value) !void {
    if (self.map.get(key)) |old_value| {
        old_value.deinit(self.allocator);
        try self.map.put(key, try value.copy(self.allocator));
    } else if (self.parent) |parent| {
        return parent.update(key, value);
    } else {
        unreachable;
    }
}

// Ranges
pub fn putRange(self: *Scope, key: usize) !void {
    try self.get("@ranges").?.ty.rec_range_map.put(key, {});
}

pub fn hasRange(self: Scope, key: usize) bool {
    return self.get("@ranges").?.ty.rec_range_map.contains(key);
}

pub fn deleteRange(self: *Scope, key: usize) void {
    _ = self.get("@ranges").?.ty.rec_range_map.remove(key);
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
