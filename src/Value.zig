const std = @import("std");

const Builtin = enum {
    atan2,
    chars,
    contains,
    cos,
    each,
    exp,
    filter,
    indexOf,
    int,
    join,
    keys,
    lastIndexOf,
    len,
    log,
    map,
    max,
    mean,
    median,
    min,
    mode,
    print,
    push,
    rand,
    reduce,
    reverse,
    sin,
    sort,
    sqrt,
    split,
    stdev,
    values,
};

pub const Type = union(enum) {
    boolean: bool,
    builtin: Builtin,
    float: f64,
    func: Function,
    int: isize,
    list: *std.ArrayList(Value),
    map: *std.StringHashMap(Value),
    nil,
    range: [2]usize,
    rec_range_map: *std.AutoHashMap(usize, void),
    string: []const u8,
    uint: usize,
};

offset: u16,
ty: Type,

const Value = @This();

pub fn new(ty: Type, offset: u16) Value {
    return .{ .offset = offset, .ty = ty };
}

pub fn copy(self: Value, allocator: std.mem.Allocator) anyerror!Value {
    return switch (self.ty) {
        .boolean => self,
        .builtin => self,
        .float => self,
        .func => try self.copyFunc(allocator),
        .int => self,
        .list => try self.copyList(allocator),
        .map => try self.copyMap(allocator),
        .nil => self,
        .range => self,
        .rec_range_map => self,
        .string => try self.copyString(allocator),
        .uint => self,
    };
}
fn copyFunc(self: Value, allocator: std.mem.Allocator) anyerror!Value {
    const instructions_copy = try allocator.dupe(u8, self.ty.func.instructions);
    const name_copy = try allocator.dupe(u8, self.ty.func.name);
    var params_copy = try allocator.alloc([]const u8, self.ty.func.params.len);
    for (self.ty.func.params) |param, i| params_copy[i] = try allocator.dupe(u8, param);
    return Value.new(.{ .func = .{
        .instructions = instructions_copy,
        .name = name_copy,
        .params = params_copy,
    } }, self.offset);
}
fn copyList(self: Value, allocator: std.mem.Allocator) anyerror!Value {
    var copy_ptr = try allocator.create(std.ArrayList(Value));
    copy_ptr.* = try std.ArrayList(Value).initCapacity(allocator, self.ty.list.items.len);
    for (self.ty.list.items) |item| copy_ptr.appendAssumeCapacity(try item.copy(allocator));
    return Value.new(.{ .list = copy_ptr }, self.offset);
}
fn copyMap(self: Value, allocator: std.mem.Allocator) anyerror!Value {
    const copy_ptr = try allocator.create(std.StringHashMap(Value));
    copy_ptr.* = std.StringHashMap(Value).init(allocator);
    try copy_ptr.ensureTotalCapacity(self.ty.map.count());
    var iter = self.ty.map.iterator();

    while (iter.next()) |entry| {
        const key_copy = try allocator.dupe(u8, entry.key_ptr.*);
        try copy_ptr.put(key_copy, try entry.value_ptr.copy(allocator));
    }

    return Value.new(.{ .map = copy_ptr }, self.offset);
}
fn copyString(self: Value, allocator: std.mem.Allocator) anyerror!Value {
    const str_copy = try allocator.dupe(u8, self.ty.string);
    return Value.new(.{ .string = str_copy }, self.offset);
}

pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
    return switch (self.ty) {
        .func => self.deinitFunc(allocator),
        .list => self.deinitList(allocator),
        .map => self.deinitMap(allocator),
        .string => self.deinitString(allocator),

        else => {},
    };
}
fn deinitFunc(self: Value, allocator: std.mem.Allocator) void {
    allocator.free(self.ty.func.instructions);
    allocator.free(self.ty.func.name);
    for (self.ty.func.params) |param| allocator.free(param);
    allocator.free(self.ty.func.params);
}
fn deinitList(self: Value, allocator: std.mem.Allocator) void {
    for (self.ty.list.items) |*item| item.deinit(allocator);
    self.ty.list.deinit();
    allocator.destroy(self.ty.list);
}
fn deinitMap(self: Value, allocator: std.mem.Allocator) void {
    var iter = self.ty.map.iterator();
    while (iter.next()) |entry| {
        entry.value_ptr.deinit(allocator);
        allocator.free(entry.key_ptr.*);
    }
    self.ty.map.deinit();
    allocator.destroy(self.ty.map);
}
fn deinitString(self: Value, allocator: std.mem.Allocator) void {
    allocator.free(self.ty.string);
}

pub fn eql(self: Value, other: Value) bool {
    if (self.asFloat()) |f1| {
        if (other.asFloat()) |f2| return f1.ty.float == f2.ty.float;
    }

    if (!self.eqlType(other)) return false;

    return switch (self.ty) {
        .boolean => |b| b == other.ty.boolean,
        .func => |f| fnc: {
            if (!std.mem.eql(u8, f.instructions, other.ty.func.instructions)) break :fnc false;
            break :fnc for (f.params) |param, i| {
                if (!std.mem.eql(u8, param, other.ty.func.params[i])) break false;
            } else true;
        },
        .list => |l| lst: {
            if (l.items.len != other.ty.list.items.len) break :lst false;
            break :lst for (l.items) |item, i| {
                if (!item.eql(other.ty.list.items[i])) break false;
            } else true;
        },
        .map => |m| mp: {
            if (m.count() != other.ty.map.count()) break :mp false;
            var iter = m.iterator();
            break :mp while (iter.next()) |entry| {
                if (other.ty.map.get(entry.key_ptr.*)) |ov| {
                    if (!entry.value_ptr.eql(ov)) break false;
                }
            } else true;
        },
        .range => |r| r[0] == other.ty.range[0] and r[1] == other.ty.range[1],
        .string => |s| std.mem.eql(u8, s, other.ty.string),
        .nil => other.ty == .nil,
        else => unreachable,
    };
}

pub fn eqlType(self: Value, other: Value) bool {
    return switch (self.ty) {
        .boolean => other.ty == .boolean,
        .builtin => other.ty == .builtin,
        .float => other.ty == .float,
        .func => other.ty == .func,
        .int => other.ty == .int,
        .list => other.ty == .list,
        .map => other.ty == .map,
        .range => other.ty == .range,
        .rec_range_map => other.ty == .rec_range_map,
        .string => other.ty == .string,
        .uint => other.ty == .uint,
        .nil => other.ty == .nil,
    };
}

pub fn asFloat(self: Value) ?Value {
    return switch (self.ty) {
        .float => self,
        .int => |i| Value.new(.{ .float = @intToFloat(f64, i) }, self.offset),
        .string => |s| if (std.fmt.parseFloat(f64, s)) |f| Value.new(.{ .float = f }, self.offset) else |_| null,
        .uint => |u| Value.new(.{ .float = @intToFloat(f64, u) }, self.offset),

        else => null,
    };
}

fn isFloatStr(src: []const u8) bool {
    if (std.mem.indexOf(u8, src, ".")) |_| return true;
    if (std.mem.indexOf(u8, src, "e+")) |_| return true;
    if (std.mem.indexOf(u8, src, "e-")) |_| return true;
    if (std.mem.indexOf(u8, src, "E+")) |_| return true;
    if (std.mem.indexOf(u8, src, "E-")) |_| return true;
    if (std.mem.indexOf(u8, src, "p")) |_| return true;
    return false;
}

fn strToNum(self: Value) anyerror!Value {
    return if (isFloatStr(self.ty.string))
        Value.new(.{ .float = try std.fmt.parseFloat(f64, self.ty.string) }, self.offset)
    else if ('-' == self.ty.string[0] or '+' == self.ty.string[0])
        Value.new(.{ .int = try std.fmt.parseInt(isize, self.ty.string, 0) }, self.offset)
    else
        Value.new(.{ .uint = try std.fmt.parseUnsigned(usize, self.ty.string, 0) }, self.offset);
}

fn addString(self: Value, other: Value) anyerror!Value {
    const converted = try strToNum(self);
    return converted.add(other);
}

fn addFloat(self: Value, other: Value) anyerror!Value {
    if (other.ty == .float) return Value.new(.{ .float = self.ty.float + other.ty.float }, self.offset);
    const other_float = other.asFloat() orelse return error.InvalidAddition;
    return Value.new(.{ .float = self.ty.float + other_float.ty.float }, self.offset);
}

fn addInt(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .int => Value.new(.{ .int = self.ty.int + other.ty.int }, self.offset),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.int) + other.ty.float }, self.offset),
        .uint => Value.new(.{ .int = self.ty.int + @intCast(isize, other.ty.uint) }, self.offset),
        .string => self.add(try other.strToNum()),
        else => error.InvalidAddition,
    };
}

fn addUint(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .uint => Value.new(.{ .uint = self.ty.uint + other.ty.uint }, self.offset),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.uint) + other.ty.float }, self.offset),
        .int => Value.new(.{ .int = @intCast(isize, self.ty.uint) + other.ty.int }, self.offset),
        .string => self.add(try other.strToNum()),
        else => error.InvalidAddition,
    };
}

pub fn add(self: Value, other: Value) anyerror!Value {
    return switch (self.ty) {
        .float => self.addFloat(other),
        .int => self.addInt(other),
        .uint => self.addUint(other),
        .string => self.addString(other),
        else => error.InvalidAddition,
    };
}

fn subString(self: Value, other: Value) anyerror!Value {
    const converted = try strToNum(self);
    return converted.sub(other);
}

fn subFloat(self: Value, other: Value) anyerror!Value {
    if (other.ty == .float) return Value.new(.{ .float = self.ty.float - other.ty.float }, self.offset);
    const other_float = other.asFloat() orelse return error.InvalidSubtraction;
    return Value.new(.{ .float = self.ty.float - other_float.ty.float }, self.offset);
}

fn subInt(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .int => Value.new(.{ .int = self.ty.int - other.ty.int }, self.offset),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.int) - other.ty.float }, self.offset),
        .uint => Value.new(.{ .int = self.ty.int - @intCast(isize, other.ty.uint) }, self.offset),
        .string => self.sub(try other.strToNum()),
        else => error.InvalidSubtraction,
    };
}

fn subUint(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.uint) - other.ty.float }, self.offset),
        .int => Value.new(.{ .int = @intCast(isize, self.ty.uint) - other.ty.int }, self.offset),
        .uint => if (self.ty.uint < other.ty.uint)
            Value.new(.{ .int = @intCast(isize, self.ty.uint) - @intCast(isize, other.ty.uint) }, self.offset)
        else
            Value.new(.{ .uint = self.ty.uint - other.ty.uint }, self.offset),
        .string => self.sub(try other.strToNum()),
        else => error.InvalidSubtraction,
    };
}
pub fn sub(self: Value, other: Value) anyerror!Value {
    return switch (self.ty) {
        .float => self.subFloat(other),
        .int => self.subInt(other),
        .uint => self.subUint(other),
        .string => self.subString(other),
        else => error.InvalidSubtraction,
    };
}

fn mulString(self: Value, other: Value) anyerror!Value {
    const converted = try strToNum(self);
    return converted.mul(other);
}

fn mulFloat(self: Value, other: Value) anyerror!Value {
    if (other.ty == .float) return Value.new(.{ .float = self.ty.float * other.ty.float }, self.offset);
    const other_float = other.asFloat() orelse return error.InvalidMultiplication;
    return Value.new(.{ .float = self.ty.float * other_float.ty.float }, self.offset);
}

fn mulInt(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .int => Value.new(.{ .int = self.ty.int * other.ty.int }, self.offset),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.int) * other.ty.float }, self.offset),
        .uint => Value.new(.{ .int = self.ty.int * @intCast(isize, other.ty.uint) }, self.offset),
        .string => self.mul(try other.strToNum()),
        else => error.InvalidMultiplication,
    };
}

fn mulUint(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .uint => Value.new(.{ .uint = self.ty.uint * other.ty.uint }, self.offset),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.uint) * other.ty.float }, self.offset),
        .int => Value.new(.{ .int = @intCast(isize, self.ty.uint) * other.ty.int }, self.offset),
        .string => self.mul(try other.strToNum()),
        else => error.InvalidMultiplication,
    };
}

pub fn mul(self: Value, other: Value) anyerror!Value {
    return switch (self.ty) {
        .float => self.mulFloat(other),
        .int => self.mulInt(other),
        .uint => self.mulUint(other),
        .string => self.mulString(other),
        else => error.InvalidMultiplication,
    };
}

fn divString(self: Value, other: Value) anyerror!Value {
    const converted = try strToNum(self);
    return converted.div(other);
}

fn divFloat(self: Value, other: Value) anyerror!Value {
    if (other.ty == .float) return Value.new(.{ .float = self.ty.float / other.ty.float }, self.offset);
    const other_float = other.asFloat() orelse return error.InvaidDivision;
    return Value.new(.{ .float = self.ty.float / other_float.ty.float }, self.offset);
}

fn divInt(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .int => Value.new(.{ .int = @divFloor(self.ty.int, other.ty.int) }, self.offset),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.int) / other.ty.float }, self.offset),
        .uint => Value.new(.{ .int = @divFloor(self.ty.int, @intCast(isize, other.ty.uint)) }, self.offset),
        .string => self.div(try other.strToNum()),
        else => error.InvaidDivision,
    };
}

fn divUint(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .uint => Value.new(.{ .uint = @divFloor(self.ty.uint, other.ty.uint) }, self.offset),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.uint) / other.ty.float }, self.offset),
        .int => Value.new(.{ .int = @divFloor(@intCast(isize, self.ty.uint), other.ty.int) }, self.offset),
        .string => self.div(try other.strToNum()),
        else => error.InvaidDivision,
    };
}

pub fn div(self: Value, other: Value) anyerror!Value {
    return switch (self.ty) {
        .float => self.divFloat(other),
        .int => self.divInt(other),
        .uint => self.divUint(other),
        .string => self.divString(other),
        else => error.InvaidDivision,
    };
}

fn modString(self: Value, other: Value) anyerror!Value {
    const converted = try strToNum(self);
    return converted.mod(other);
}

fn modFloat(self: Value, other: Value) anyerror!Value {
    if (other.ty == .float) return Value.new(.{ .float = @rem(self.ty.float, other.ty.float) }, self.offset);
    const other_float = other.asFloat() orelse return error.InvalidModulo;
    return Value.new(.{ .float = @rem(self.ty.float, other_float.ty.float) }, self.offset);
}

fn modInt(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .int => Value.new(.{ .int = @rem(self.ty.int, other.ty.int) }, self.offset),
        .float => Value.new(.{ .float = @rem(@intToFloat(f64, self.ty.int), other.ty.float) }, self.offset),
        .uint => Value.new(.{ .int = @rem(self.ty.int, @intCast(isize, other.ty.uint)) }, self.offset),
        .string => self.mod(try other.strToNum()),
        else => error.InvalidModulo,
    };
}

fn modUint(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .uint => Value.new(.{ .uint = @rem(self.ty.uint, other.ty.uint) }, self.offset),
        .float => Value.new(.{ .float = @rem(@intToFloat(f64, self.ty.uint), other.ty.float) }, self.offset),
        .int => Value.new(.{ .int = @rem(@intCast(isize, self.ty.uint), other.ty.int) }, self.offset),
        .string => self.mod(try other.strToNum()),
        else => error.InvalidModulo,
    };
}

pub fn mod(self: Value, other: Value) anyerror!Value {
    return switch (self.ty) {
        .float => self.modFloat(other),
        .int => self.modInt(other),
        .uint => self.modUint(other),
        .string => self.modString(other),
        else => error.InvalidModulo,
    };
}

fn cmpString(self: Value, other: Value) anyerror!std.math.Order {
    if (other.ty == .string) {
        var order = std.math.order(self.ty.string.len, other.ty.string.len);
        if (order != .eq) return order;
        return for (self.ty.string) |byte, i| {
            order = std.math.order(byte, other.ty.string[i]);
            if (order != .eq) break order;
        } else .eq;
    }

    const converted = try strToNum(self);
    return converted.cmp(other);
}

fn cmpFloat(self: Value, other: Value) anyerror!std.math.Order {
    if (other.ty == .float) return std.math.order(self.ty.float, other.ty.float);
    const other_float = other.asFloat() orelse return error.InvalidComparison;
    return std.math.order(self.ty.float, other_float.ty.float);
}

fn cmpInt(self: Value, other: Value) anyerror!std.math.Order {
    return switch (other.ty) {
        .int => return std.math.order(self.ty.int, other.ty.int),
        .float => return std.math.order(@intToFloat(f64, self.ty.int), other.ty.float),
        .uint => return std.math.order(self.ty.int, @intCast(isize, other.ty.uint)),
        .string => return self.cmp(try other.strToNum()),
        else => error.InvalidComparison,
    };
}

fn cmpUint(self: Value, other: Value) anyerror!std.math.Order {
    return switch (other.ty) {
        .uint => std.math.order(self.ty.uint, other.ty.uint),
        .float => std.math.order(@intToFloat(f64, self.ty.uint), other.ty.float),
        .int => std.math.order(@intCast(isize, self.ty.uint), other.ty.int),
        .string => self.cmp(try other.strToNum()),
        else => error.InvalidComparison,
    };
}

pub fn cmp(self: Value, other: Value) anyerror!std.math.Order {
    return switch (self.ty) {
        .float => self.cmpFloat(other),
        .int => self.cmpInt(other),
        .uint => self.cmpUint(other),
        .string => self.cmpString(other),
        else => error.InvalidComparison,
    };
}

pub fn lessThan(_: void, a: Value, b: Value) bool {
    return a.cmp(b) catch unreachable == .lt;
}

pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = fmt;
    _ = options;

    switch (self.ty) {
        .boolean => |b| _ = try writer.print("{}", .{b}),
        .float => |f| _ = try writer.print("{d}", .{f}),
        .int => |i| _ = try writer.print("{}", .{i}),
        .list => |l| try printList(l, writer),
        .map => |m| try printMap(m, writer),
        .string => |s| _ = try writer.print("{s}", .{s}),
        .uint => |u| _ = try writer.print("{}", .{u}),

        else => {},
    }
}

fn printList(list: *std.ArrayList(Value), writer: anytype) !void {
    try writer.writeByte('[');

    for (list.items) |element, i| {
        if (i != 0) try writer.writeAll(", ");
        _ = try writer.print("{}", .{element});
    }

    try writer.writeByte(']');
}

fn printMap(map: *std.StringHashMap(Value), writer: anytype) !void {
    try writer.writeByte('[');
    var iter = map.iterator();
    var i: usize = 0;

    while (iter.next()) |entry| : (i += 1) {
        if (i != 0) try writer.writeAll(", ");
        _ = try writer.print("{s}: {}", .{ entry.key_ptr.*, entry.value_ptr.* });
    }

    try writer.writeByte(']');
}

pub const Function = struct {
    instructions: []const u8,
    name: []const u8 = "",
    params: [][]const u8,
};
