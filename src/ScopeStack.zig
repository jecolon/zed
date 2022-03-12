const std = @import("std");

const Scope = @import("Scope.zig");
const Value = @import("Value.zig");

const ScopeStack = @This();

allocator: std.mem.Allocator,
columns: std.ArrayList(Value) = undefined,
global_scope: std.StringHashMap(Value),
stack: std.ArrayList(Scope),

// Current data filename
file: [*:0]const u8 = "",
// Record numbering
frnum: usize = 1,
rnum: usize = 1,
// Current record
rec_buf: [1024 * 64]u8 = undefined,
record: [*:0]const u8 = undefined,
// Record ranges
rec_ranges: std.AutoHashMap(u8, void) = undefined,
// Delimiters
ifs: [*:0]const u8 = ",",
irs: [*:0]const u8 = "\n",
ofs: [*:0]const u8 = ",",
ors: [*:0]const u8 = "\n",

const builtins = std.ComptimeStringMap(void, .{
    .{ "atan2", {} },
    .{ "chars", {} },
    .{ "contains", {} },
    .{ "cos", {} },
    .{ "each", {} },
    .{ "endsWith", {} },
    .{ "exp", {} },
    .{ "filter", {} },
    .{ "int", {} },
    .{ "indexOf", {} },
    .{ "join", {} },
    .{ "keys", {} },
    .{ "keysByValueAsc", {} },
    .{ "keysByValueDesc", {} },
    .{ "lastIndexOf", {} },
    .{ "len", {} },
    .{ "log", {} },
    .{ "map", {} },
    .{ "max", {} },
    .{ "mean", {} },
    .{ "median", {} },
    .{ "min", {} },
    .{ "mode", {} },
    .{ "print", {} },
    .{ "pop", {} },
    .{ "push", {} },
    .{ "rand", {} },
    .{ "reduce", {} },
    .{ "reverse", {} },
    .{ "sin", {} },
    .{ "sortAsc", {} },
    .{ "sortDesc", {} },
    .{ "split", {} },
    .{ "sqrt", {} },
    .{ "startsWith", {} },
    .{ "stdev", {} },
    .{ "toLower", {} },
    .{ "toUpper", {} },
    .{ "values", {} },
});

const globals = std.ComptimeStringMap(void, .{
    .{ "@cols", {} },
    .{ "@file", {} },
    .{ "@frnum", {} },
    .{ "@ifs", {} },
    .{ "@irs", {} },
    .{ "@ofs", {} },
    .{ "@ors", {} },
    .{ "@rec", {} },
    .{ "@rnum", {} },
});

const read_only = std.ComptimeStringMap(void, .{
    .{ "@file", {} },
    .{ "@frnum", {} },
    .{ "@rnum", {} },
});

pub fn init(allocator: std.mem.Allocator) ScopeStack {
    return ScopeStack{
        .allocator = allocator,
        .global_scope = std.StringHashMap(Value).init(allocator),
        .rec_ranges = std.AutoHashMap(u8, void).init(allocator),
        .stack = std.ArrayList(Scope).init(allocator),
    };
}

pub fn push(self: *ScopeStack, scope: Scope) !void {
    try self.stack.append(scope);
}

pub fn pop(self: *ScopeStack) Scope {
    std.debug.assert(self.stack.items.len > 0);
    return self.stack.pop();
}

pub fn atGlobalScope(self: ScopeStack) bool {
    return self.stack.items.len == 0;
}

pub fn isDefined(self: ScopeStack, key: []const u8) bool {
    if (builtins.has(key)) return true;
    if (globals.has(key)) return true;

    const len = self.stack.items.len;
    var i: usize = 1;

    while (i <= len) : (i += 1) {
        if (self.stack.items[len - i].map.contains(key)) return true;
        //if (self.stack.items[len - i].ty == .function) return self.stack.items[0].isDefined(key);
    }

    return self.global_scope.contains(key);
}

pub fn load(self: ScopeStack, key: []const u8) !?Value {
    if (std.mem.eql(u8, key, "@cols")) return try Value.newList(self.allocator, self.columns);
    if (std.mem.eql(u8, key, "@file")) return try Value.newStringP(self.allocator, self.file);
    if (std.mem.eql(u8, key, "@frnum")) return Value{ .ty = .{ .uint = @intCast(u64, self.frnum) } };
    if (std.mem.eql(u8, key, "@ifs")) return try Value.newStringP(self.allocator, self.ifs);
    if (std.mem.eql(u8, key, "@irs")) return try Value.newStringP(self.allocator, self.irs);
    if (std.mem.eql(u8, key, "@ofs")) return try Value.newStringP(self.allocator, self.ofs);
    if (std.mem.eql(u8, key, "@ors")) return try Value.newStringP(self.allocator, self.ors);
    if (std.mem.eql(u8, key, "@rec")) return try Value.newStringP(self.allocator, self.record);
    if (std.mem.eql(u8, key, "@rnum")) return Value{ .ty = .{ .uint = @intCast(u64, self.frnum) } };

    const len = self.stack.items.len;
    var i: usize = 1;

    while (i <= len) : (i += 1) {
        if (self.stack.items[len - i].map.get(key)) |value| return value;
        //if (self.stack.items[len - i].ty == .function) return self.stack.items[0].load(key);
    }

    return self.global_scope.get(key);
}

pub fn store(self: *ScopeStack, key: []const u8, value: Value) !void {
    if (self.stack.items.len > 0) {
        try self.stack.items[self.stack.items.len - 1].map.put(key, value);
    } else {
        const key_copy = try self.allocator.dupe(u8, key);
        try self.global_scope.put(key_copy, try value.copy(self.allocator));
    }
}

fn updateGlobals(self: *ScopeStack, key: []const u8, value: Value) !bool {
    if (std.mem.eql(u8, key, "@ifs")) {
        if (value.ty != .obj or value.ty.obj.* != .string) return error.InvalidAtIfs;
        self.ifs = try self.allocator.dupeZ(u8, std.mem.sliceTo(value.ty.obj.string, 0));
        return true;
    }
    if (std.mem.eql(u8, key, "@irs")) {
        if (value.ty != .obj or value.ty.obj.* != .string) return error.InvalidAtIrs;
        self.irs = try self.allocator.dupeZ(u8, std.mem.sliceTo(value.ty.obj.string, 0));
        return true;
    }
    if (std.mem.eql(u8, key, "@ofs")) {
        if (value.ty != .obj or value.ty.obj.* != .string) return error.InvalidAtOfs;
        self.ofs = try self.allocator.dupeZ(u8, std.mem.sliceTo(value.ty.obj.string, 0));
        return true;
    }
    if (std.mem.eql(u8, key, "@ors")) {
        if (value.ty != .obj or value.ty.obj.* != .string) return error.InvalidAtOrs;
        self.ors = try self.allocator.dupeZ(u8, std.mem.sliceTo(value.ty.obj.string, 0));
        return true;
    }
    if (std.mem.eql(u8, key, "@rec")) {
        if (value.ty != .obj or value.ty.obj.* != .string) return error.InvalidAtRec;
        self.record = try self.allocator.dupeZ(u8, std.mem.sliceTo(value.ty.obj.string, 0));
        return true;
    }
    if (std.mem.eql(u8, key, "@cols")) {
        if (value.ty != .obj or value.ty.obj.* != .list) return error.InvalidAtCols;
        self.columns = (try value.copy(self.allocator)).ty.obj.list;
        return true;
    }

    return false;
}

pub fn update(self: *ScopeStack, key: []const u8, value: Value) !void {
    if (read_only.has(key)) return error.ReadOnlyGlobal;
    if (try self.updateGlobals(key, value)) return;

    const len = self.stack.items.len;
    var i: usize = 1;

    while (i <= len) : (i += 1) {
        if (self.stack.items[len - i].map.contains(key)) return self.stack.items[len - i].map.put(key, value);
        //if (self.stack.items[len - i].ty == .function) return self.stack.items[0].update(key, value);
    }

    if (self.global_scope.get(key)) |old_value| old_value.deinit(self.allocator);
    try self.global_scope.put(key, try value.copy(self.allocator));
}

// Debug
pub fn dump(self: ScopeStack) void {
    std.debug.print("\n*** ScopeStack Dump Begin ***\n", .{});

    for (self.stack.items) |scope, i| {
        std.debug.print("*** Scope #{} Dump Begin ***\n", .{i});
        scope.dump();
        std.debug.print("*** Scope #{} Dump End ***\n", .{i});
    }

    std.debug.print("*** Global Scope Dump Begin ***\n", .{});
    var iter = self.global_scope.iterator();
    while (iter.next()) |entry| std.debug.print("\t{s}: {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    std.debug.print("*** Global Scope Dump End ***\n", .{});

    std.debug.print("*** ScopeStack Dump End ***\n", .{});
}
