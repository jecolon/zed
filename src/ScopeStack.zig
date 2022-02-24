const std = @import("std");

const Scope = @import("Scope.zig");
const Value = @import("Value.zig");

const ScopeStack = @This();

allocator: std.mem.Allocator,
columns: *std.ArrayList(Value) = undefined,
global_scope: std.StringHashMap(Value),
stack: std.ArrayList(Scope),

// Current data filename
file: []const u8 = "",
// Record numbering
frnum: usize = 1,
rnum: usize = 1,
// Current record
rec_buf: [1024 * 64]u8 = undefined,
record: []const u8 = undefined,
// Record ranges
rec_ranges: std.AutoHashMap(u8, void) = undefined,
// Delimiters
ifs: []const u8 = ",",
irs: []const u8 = "\n",
ofs: []const u8 = ",",
ors: []const u8 = "\n",

const builtins = std.ComptimeStringMap(Value, .{
    .{ "atan2", Value.new(.{ .builtin = .atan2 }, 0) },
    .{ "chars", Value.new(.{ .builtin = .chars }, 0) },
    .{ "contains", Value.new(.{ .builtin = .contains }, 0) },
    .{ "cos", Value.new(.{ .builtin = .cos }, 0) },
    .{ "each", Value.new(.{ .builtin = .each }, 0) },
    .{ "endsWith", Value.new(.{ .builtin = .endsWith }, 0) },
    .{ "exp", Value.new(.{ .builtin = .exp }, 0) },
    .{ "filter", Value.new(.{ .builtin = .filter }, 0) },
    .{ "int", Value.new(.{ .builtin = .int }, 0) },
    .{ "indexOf", Value.new(.{ .builtin = .indexOf }, 0) },
    .{ "join", Value.new(.{ .builtin = .join }, 0) },
    .{ "keys", Value.new(.{ .builtin = .keys }, 0) },
    .{ "lastIndexOf", Value.new(.{ .builtin = .lastIndexOf }, 0) },
    .{ "len", Value.new(.{ .builtin = .len }, 0) },
    .{ "log", Value.new(.{ .builtin = .log }, 0) },
    .{ "map", Value.new(.{ .builtin = .map }, 0) },
    .{ "max", Value.new(.{ .builtin = .max }, 0) },
    .{ "mean", Value.new(.{ .builtin = .mean }, 0) },
    .{ "median", Value.new(.{ .builtin = .median }, 0) },
    .{ "min", Value.new(.{ .builtin = .min }, 0) },
    .{ "mode", Value.new(.{ .builtin = .mode }, 0) },
    .{ "print", Value.new(.{ .builtin = .print }, 0) },
    .{ "pop", Value.new(.{ .builtin = .pop }, 0) },
    .{ "push", Value.new(.{ .builtin = .push }, 0) },
    .{ "rand", Value.new(.{ .builtin = .rand }, 0) },
    .{ "reduce", Value.new(.{ .builtin = .reduce }, 0) },
    .{ "reverse", Value.new(.{ .builtin = .reverse }, 0) },
    .{ "sin", Value.new(.{ .builtin = .sin }, 0) },
    .{ "sort", Value.new(.{ .builtin = .sort }, 0) },
    .{ "split", Value.new(.{ .builtin = .split }, 0) },
    .{ "sqrt", Value.new(.{ .builtin = .sqrt }, 0) },
    .{ "startsWith", Value.new(.{ .builtin = .startsWith }, 0) },
    .{ "stdev", Value.new(.{ .builtin = .stdev }, 0) },
    .{ "values", Value.new(.{ .builtin = .values }, 0) },
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

pub fn load(self: ScopeStack, key: []const u8) ?Value {
    if (builtins.get(key)) |v| return v;

    if (std.mem.eql(u8, key, "@cols")) return Value.new(.{ .list = self.columns }, 0);
    if (std.mem.eql(u8, key, "@file")) return Value.new(.{ .string = self.file }, 0);
    if (std.mem.eql(u8, key, "@frnum")) return Value.new(.{ .uint = @intCast(u64, self.frnum) }, 0);
    if (std.mem.eql(u8, key, "@ifs")) return Value.new(.{ .string = self.ifs }, 0);
    if (std.mem.eql(u8, key, "@irs")) return Value.new(.{ .string = self.irs }, 0);
    if (std.mem.eql(u8, key, "@ofs")) return Value.new(.{ .string = self.ofs }, 0);
    if (std.mem.eql(u8, key, "@ors")) return Value.new(.{ .string = self.ors }, 0);
    if (std.mem.eql(u8, key, "@rec")) return Value.new(.{ .string = self.record }, 0);
    if (std.mem.eql(u8, key, "@rnum")) return Value.new(.{ .uint = @intCast(u64, self.frnum) }, 0);

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
        if (value.ty != .string) return error.InvalidAtIfs;
        self.ifs = try self.allocator.dupe(u8, value.ty.string);
        return true;
    }
    if (std.mem.eql(u8, key, "@irs")) {
        if (value.ty != .string) return error.InvalidAtIrs;
        self.irs = try self.allocator.dupe(u8, value.ty.string);
        return true;
    }
    if (std.mem.eql(u8, key, "@ofs")) {
        if (value.ty != .string) return error.InvalidAtOfs;
        self.ofs = try self.allocator.dupe(u8, value.ty.string);
        return true;
    }
    if (std.mem.eql(u8, key, "@ors")) {
        if (value.ty != .string) return error.InvalidAtOrs;
        self.ors = try self.allocator.dupe(u8, value.ty.string);
        return true;
    }
    if (std.mem.eql(u8, key, "@rec")) {
        if (value.ty != .string) return error.InvalidAtRec;
        self.record = try self.allocator.dupe(u8, value.ty.string);
        return true;
    }
    if (std.mem.eql(u8, key, "@cols")) {
        if (value.ty != .list) return error.InvalidAtCols;
        self.columns = (try value.copy(self.allocator)).ty.list;
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
