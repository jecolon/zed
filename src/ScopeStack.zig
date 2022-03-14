const std = @import("std");

const Scope = @import("Scope.zig");
const value = @import("value.zig");
const Value = value.Value;

const ScopeStack = @This();

allocator: std.mem.Allocator,
columns: Value = value.val_nil,
global_scope: std.StringHashMap(Value),
stack: std.ArrayList(Scope),

// Current data filename
file: Value = value.val_nil,
// Record numbering
frnum: usize = 1,
rnum: usize = 1,
// Current record
rec_buf: [1024 * 64]u8 = undefined,
record: Value = value.val_nil,
// Record ranges
rec_ranges: std.AutoHashMap(u8, void) = undefined,
// Delimiters
ifs: Value = value.strToValue(","),
irs: Value = value.strToValue("\n"),
ofs: Value = value.strToValue(","),
ors: Value = value.strToValue("\n"),

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
    const len = self.stack.items.len;
    var i: usize = 1;
    while (i <= len) : (i += 1) {
        if (self.stack.items[len - i].map.get(key)) |v| return v;
        //if (self.stack.items[len - i].ty == .function) return self.stack.items[0].load(key);
    }

    return self.global_scope.get(key);
}

pub fn store(self: *ScopeStack, key: []const u8, v: Value) !void {
    if (self.stack.items.len > 0) {
        try self.stack.items[self.stack.items.len - 1].map.put(key, v);
    } else {
        const key_copy = try self.allocator.dupe(u8, key);
        try self.global_scope.put(key_copy, try value.copy(v, self.allocator));
    }
}

pub fn update(self: *ScopeStack, key: []const u8, v: Value) !void {
    const len = self.stack.items.len;
    var i: usize = 1;
    while (i <= len) : (i += 1) {
        if (self.stack.items[len - i].map.contains(key)) return self.stack.items[len - i].map.put(key, v);
        //if (self.stack.items[len - i].ty == .function) return self.stack.items[0].update(key, value);
    }

    try self.global_scope.put(key, try value.copy(v, self.allocator));
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
