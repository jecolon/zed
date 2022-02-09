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
        if (entry.value_ptr.ty == .func) self.funcDeinit(entry.value_ptr.*);
        //if (entry.value_ptr.ty == .list) self.listDeinit(entry.value_ptr.ty.list);
        //if (entry.value_ptr.ty == .map) self.mapDeinit(entry.value_ptr.ty.map);
        if (entry.value_ptr.ty == .string) self.allocator.free(entry.value_ptr.ty.string);
        self.allocator.free(entry.key_ptr.*);
    }
    self.map.deinit();
}

fn funcCopy(self: *Scope, func: Value) !Value {
    const instructions_copy = try self.allocator.dupe(u8, func.ty.func.instructions);
    const name_copy = try self.allocator.dupe(u8, func.ty.func.name);
    var params_copy = try self.allocator.alloc([]const u8, func.ty.func.params.len);
    for (func.ty.func.params) |param, i| params_copy[i] = try self.allocator.dupe(u8, param);
    return Value.new(.{ .func = .{
        .instructions = instructions_copy,
        .name = name_copy,
        .params = params_copy,
    } }, func.offset);
}

fn listCopy(self: *Scope, list: Value) !Value {
    var copy_ptr = try self.allocator.create(std.ArrayList(Value));
    copy_ptr.* = try std.ArrayList(Value).initCapacity(self.allocator, list.ty.list.items.len);

    for (list.ty.list.items) |item| {
        if (item.ty == .string) {
            copy_ptr.appendAssumeCapacity(self.stringCopy(item));
        } else if (item.ty == .list) {
            copy_ptr.appendAssumeCapacity(self.listCopy(item));
        } else if (item.ty == .map) {
            copy_ptr.appendAssumeCapacity(self.mapCopy(item));
        } else {
            copy_ptr.appendAssumeCapacity(item);
        }
    }

    return Value.new(.{ .list = copy_ptr }, list.offset);
}

fn mapCopy(self: *Scope, map: Value) !Value {
    const copy_ptr = try self.allocator.create(std.StringHashMap(Value));
    copy_ptr.* = std.StringHashMap(Value).init(self.allocator);
    var iter = map.ty.map.iterator();

    while (iter.next()) |entry| {
        const key_copy = try self.allocator.dupe(u8, entry.key_ptr.*);
        const value_copy = switch (entry.value_ptr.ty) {
            .string => self.stringCopy(entry.value_ptr.*),
            .list => self.listCopy(entry.value_ptr.*),
            .map => self.mapCopy(entry.value_ptr.*),
            else => entry.value_ptr.*,
        };
        try copy_ptr.put(key_copy, value_copy);
    }

    return Value.new(.{ .map = copy_ptr }, map.offset);
}

fn stringCopy(self: *Scope, str: Value) !Value {
    const copy = try self.allocator.dupe(u8, str.ty.string);
    return Value.new(.{ .string = copy }, str.offset);
}

fn funcDeinit(self: *Scope, func: Value) void {
    self.allocator.free(func.ty.func.instructions);
    self.allocator.free(func.ty.func.name);
    for (func.ty.func.params) |param| self.allocator.free(param);
    self.allocator.free(func.ty.func.params);
}

fn listDeinit(self: *Scope, list_ptr: *std.ArrayList(Value)) void {
    for (list_ptr.items) |*item| {
        if (item.ty == .string) self.allocator.free(item.ty.string);
        if (item.ty == .list) self.listDeinit(item.ty.list);
        if (item.ty == .map) self.mapDeinit(item.ty.map);
    }
    list_ptr.deinit();
    self.allocator.destroy(list_ptr);
}

pub fn mapDeinit(self: *Scope, map_ptr: *std.StringHashMap(Value)) void {
    var iter = map_ptr.iterator();
    while (iter.next()) |entry| {
        if (entry.value_ptr.ty == .string) self.allocator.free(entry.value_ptr.ty.string);
        if (entry.value_ptr.ty == .list) self.listDeinit(entry.value_ptr.ty.list);
        if (entry.value_ptr.ty == .map) self.mapDeinit(entry.value_ptr.ty.map);
        self.allocator.free(entry.key_ptr.*);
    }
    map_ptr.deinit();
    self.allocator.destroy(map_ptr);
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

    if (value.ty == .func) {
        try self.map.put(key_copy, try self.funcCopy(value));
        //} else if (value.ty == .list) {
        //    try self.map.put(key_copy, try self.listCopy(value));
        //} else if (value.ty == .map) {
        //    try self.map.put(key_copy, try self.mapCopy(value));
    } else if (value.ty == .string) {
        try self.map.put(key_copy, try self.stringCopy(value));
    } else {
        try self.map.put(key_copy, value);
    }
}

pub fn update(self: *Scope, key: []const u8, value: Value) !void {
    if (self.map.get(key)) |old_value| {
        if (old_value.ty == .func) self.funcDeinit(old_value);
        //if (old_value.ty == .list) self.listDeinit(old_value.ty.list);
        //if (old_value.ty == .map) self.mapDeinit(old_value.ty.map);
        if (old_value.ty == .string) self.allocator.free(old_value.ty.string);

        if (value.ty == .func) {
            try self.map.put(key, try self.funcCopy(value));
            //} else if (value.ty == .list) {
            //    try self.map.put(key, try self.listCopy(value));
            //} else if (value.ty == .map) {
            //    try self.map.put(key, try self.mapCopy(value));
        } else if (value.ty == .string) {
            try self.map.put(key, try self.stringCopy(value));
        } else {
            try self.map.put(key, value);
        }
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
