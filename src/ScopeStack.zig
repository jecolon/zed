const std = @import("std");

const Scope = @import("Scope.zig");
const Value = @import("Value.zig");

const ScopeStack = @This();

stack: std.ArrayList(Scope),

pub fn init(allocator: std.mem.Allocator) ScopeStack {
    return ScopeStack{ .stack = std.ArrayList(Scope).init(allocator) };
}

pub fn push(self: *ScopeStack, scope: Scope) !void {
    try self.stack.append(scope);
}

pub fn pop(self: *ScopeStack) Scope {
    return self.stack.pop();
}

pub fn head(self: ScopeStack) *Scope {
    return &self.stack.items[self.stack.items.len - 1];
}

pub fn isDefined(self: ScopeStack, key: []const u8) bool {
    const len = self.stack.items.len;
    var i: usize = 1;

    while (i <= len) : (i += 1) {
        if (self.stack.items[len - i].isDefined(key)) return true;
        //if (self.stack.items[len - i].ty == .function) return self.stack.items[0].isDefined(key);
    }

    return false;
}

pub fn load(self: ScopeStack, key: []const u8) ?Value {
    if (std.mem.eql(u8, key, "@rec")) {
        return Value.new(.{ .string = self.stack.items[0].record }, 0);
    }
    if (std.mem.eql(u8, key, "@cols")) {
        return Value.new(.{ .list = self.stack.items[0].columns }, 0);
    }

    const len = self.stack.items.len;
    var i: usize = 1;

    while (i <= len) : (i += 1) {
        if (self.stack.items[len - i].load(key)) |value| return value;
        //if (self.stack.items[len - i].ty == .function) return self.stack.items[0].load(key);
    }

    return null;
}

pub fn store(self: *ScopeStack, key: []const u8, value: Value) !void {
    return self.stack.items[self.stack.items.len - 1].store(key, value);
}

pub fn update(self: *ScopeStack, key: []const u8, value: Value) !void {
    if (std.mem.eql(u8, key, "@rec")) {
        if (value.ty != .string) return error.InvalidAtRec;
        self.stack.items[0].record = value.ty.string;
        return;
    }
    if (std.mem.eql(u8, key, "@cols")) {
        if (value.ty != .list) return error.InvalidAtCols;
        self.stack.items[0].columns = value.ty.list;
        return;
    }

    const len = self.stack.items.len;
    var i: usize = 1;

    while (i <= len) : (i += 1) {
        if (self.stack.items[len - i].map.contains(key)) return self.stack.items[len - i].update(key, value);
        //if (self.stack.items[len - i].ty == .function) return self.stack.items[0].update(key, value);
    }
}

// Debug
pub fn dump(self: ScopeStack) void {
    std.debug.print("\n*** ScopeStack Dump Begin ***\n", .{});
    for (self.stack.items) |scope, i| {
        std.debug.print("*** Scope #{} Dump Begin ***\n", .{i});
        scope.dump();
        std.debug.print("*** Scope #{} Dump End ***\n", .{i});
    }
    std.debug.print("*** ScopeStack Dump End ***\n", .{});
}
