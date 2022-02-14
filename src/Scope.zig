const std = @import("std");

const Value = @import("Value.zig");

const Scope = @This();

allocator: std.mem.Allocator,
break_point: bool = false,
map: std.StringHashMap(Value),
parent: ?*Scope,
rec_buf: [1024 * 64]u8 = undefined,
record: []const u8 = undefined,
columns: *std.ArrayList(Value) = undefined,

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

    if (self.map.contains(key)) {
        return true;
    } else if (self.parent) |parent| {
        return parent.isDefined(key);
    } else {
        return false;
    }
}

pub fn load(self: Scope, key: []const u8) ?Value {
    if (std.mem.eql(u8, key, "@rec")) {
        if (self.parent) |parent| return parent.load(key);
        return Value.new(.{ .string = self.record }, 0);
    }
    if (std.mem.eql(u8, key, "@cols")) {
        if (self.parent) |parent| return parent.load(key);
        return Value.new(.{ .list = self.columns }, 0);
    }

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
    if (read_only.has(key)) return error.ReadOnlyGlobal;

    if (std.mem.eql(u8, key, "@rec")) {
        if (self.parent) |parent| return parent.update(key, value);
        if (value.ty != .string) return error.InvalidAtRec;
        self.record = value.ty.string;
        return;
    }
    if (std.mem.eql(u8, key, "@cols")) {
        if (self.parent) |parent| return parent.update(key, value);
        if (value.ty != .list) return error.InvalidAtCols;
        self.columns = value.ty.list;
        return;
    }

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
