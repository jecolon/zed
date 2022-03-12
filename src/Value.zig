const std = @import("std");

const Value = @This();

pub const Tag = enum {
    boolean,
    float,
    int,
    nil,
    obj,
    uint,
};

pub const Object = union(enum) {
    func: Function,
    list: std.ArrayList(Value),
    map: std.StringHashMap(Value),
    range: [2]usize,
    string: [*:0]const u8,
};

pub const Type = union(Tag) {
    boolean: bool,
    float: f64,
    int: i64,
    nil,
    obj: *Object,
    uint: u64,
};

ty: Type,

//pub const S = struct {
//    pub var i: usize = 0;
//};

pub fn new(ty: Type) Value {
    //S.i += 1;
    return .{ .ty = ty };
}
pub fn newFunc(allocator: std.mem.Allocator, func: Function) !Value {
    const obj_ptr = try allocator.create(Value.Object);
    obj_ptr.* = .{ .func = func };
    return Value.new(.{ .obj = obj_ptr });
}
pub fn newList(allocator: std.mem.Allocator, list: std.ArrayList(Value)) !Value {
    const obj_ptr = try allocator.create(Value.Object);
    obj_ptr.* = .{ .list = list };
    return Value.new(.{ .obj = obj_ptr });
}
pub fn newMap(allocator: std.mem.Allocator, map: std.StringHashMap(Value)) !Value {
    const obj_ptr = try allocator.create(Value.Object);
    obj_ptr.* = .{ .map = map };
    return Value.new(.{ .obj = obj_ptr });
}
pub fn newRange(allocator: std.mem.Allocator, range: [2]usize) !Value {
    const obj_ptr = try allocator.create(Value.Object);
    obj_ptr.* = .{ .range = range };
    return Value.new(.{ .obj = obj_ptr });
}
pub fn newString(allocator: std.mem.Allocator, str: []const u8) !Value {
    var buf = try allocator.alloc(u8, str.len + 1);
    std.mem.copy(u8, buf, str);
    buf[buf.len - 1] = 0;
    const obj_ptr = try allocator.create(Value.Object);
    obj_ptr.* = .{ .string = @ptrCast([*:0]const u8, &buf[0]) };
    return Value.new(.{ .obj = obj_ptr });
}
pub fn newStringZ(allocator: std.mem.Allocator, str: []const u8) !Value {
    const obj_ptr = try allocator.create(Value.Object);
    obj_ptr.* = .{ .string = @ptrCast([*:0]const u8, &str[0]) };
    return Value.new(.{ .obj = obj_ptr });
}
pub fn newStringP(allocator: std.mem.Allocator, str: [*:0]const u8) !Value {
    const obj_ptr = try allocator.create(Value.Object);
    obj_ptr.* = .{ .string = str };
    return Value.new(.{ .obj = obj_ptr });
}

pub fn copy(self: Value, allocator: std.mem.Allocator) anyerror!Value {
    return switch (self.ty) {
        .boolean => self,
        .float => self,
        .int => self,
        .nil => self,
        .obj => self.copyObject(allocator),
        .uint => self,
    };
}
fn copyObject(self: Value, allocator: std.mem.Allocator) anyerror!Value {
    return switch (self.ty.obj.*) {
        .func => |f| copyFunc(allocator, f),
        .list => |l| copyList(allocator, l),
        .map => |m| copyMap(allocator, m),
        .range => |r| Value.newRange(allocator, [2]usize{ r[0], r[1] }),
        .string => |s| copyString(allocator, s),
    };
}
fn copyFunc(allocator: std.mem.Allocator, func: Function) anyerror!Value {
    const instructions_copy = try allocator.dupe(u8, func.instructions);
    const name_copy = try allocator.dupe(u8, func.name);
    var params_copy = try allocator.alloc([]const u8, func.params.len);
    for (func.params) |param, i| params_copy[i] = try allocator.dupe(u8, param);
    const copy_ptr = try allocator.create(Object);
    copy_ptr.* = .{ .func = Function{
        .instructions = instructions_copy,
        .name = name_copy,
        .params = params_copy,
    } };
    return Value.new(.{ .obj = copy_ptr });
}
fn copyList(allocator: std.mem.Allocator, list: std.ArrayList(Value)) anyerror!Value {
    var list_copy = try std.ArrayList(Value).initCapacity(allocator, list.items.len);
    for (list.items) |item| list_copy.appendAssumeCapacity(try item.copy(allocator));
    const obj_ptr = try allocator.create(Object);
    obj_ptr.* = .{ .list = list_copy };
    return Value.new(.{ .obj = obj_ptr });
}
fn copyMap(allocator: std.mem.Allocator, map: std.StringHashMap(Value)) anyerror!Value {
    var map_copy = std.StringHashMap(Value).init(allocator);
    try map_copy.ensureTotalCapacity(map.count());
    var iter = map.iterator();

    while (iter.next()) |entry| {
        const key_copy = try allocator.dupe(u8, entry.key_ptr.*);
        map_copy.putAssumeCapacity(key_copy, try entry.value_ptr.copy(allocator));
    }

    const obj_ptr = try allocator.create(Object);
    obj_ptr.* = .{ .map = map_copy };
    return Value.new(.{ .obj = obj_ptr });
}
fn copyString(allocator: std.mem.Allocator, str: [*:0]const u8) anyerror!Value {
    const str_copy = try allocator.dupeZ(u8, std.mem.sliceTo(str, 0));
    const obj_ptr = try allocator.create(Object);
    obj_ptr.* = .{ .string = str_copy };
    return Value.new(.{ .obj = obj_ptr });
}

pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
    if (self.ty == .obj) {
        switch (self.ty.obj.*) {
            .func => |f| deinitFunc(allocator, f),
            .list => |*l| deinitList(allocator, l),
            .map => |*m| deinitMap(allocator, m),
            .string => |s| deinitString(allocator, s),

            else => {},
        }
        allocator.destroy(self.ty.obj);
    }
}
fn deinitFunc(allocator: std.mem.Allocator, func: Function) void {
    allocator.free(func.instructions);
    allocator.free(func.name);
    for (func.params) |param| allocator.free(param);
    allocator.free(func.params);
}
fn deinitList(allocator: std.mem.Allocator, list: *std.ArrayList(Value)) void {
    for (list.items) |*item| item.deinit(allocator);
    list.deinit();
}
fn deinitMap(allocator: std.mem.Allocator, map: *std.StringHashMap(Value)) void {
    var iter = map.iterator();
    while (iter.next()) |entry| {
        allocator.free(entry.key_ptr.*);
        entry.value_ptr.deinit(allocator);
    }
    map.deinit();
}
fn deinitString(allocator: std.mem.Allocator, str: [*:0]const u8) void {
    allocator.free(std.mem.sliceTo(str, 0));
}

pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = fmt;
    _ = options;

    switch (self.ty) {
        .boolean => |b| _ = try writer.print("{}", .{b}),
        .float => |f| _ = try writer.print("{d}", .{f}),
        .int => |i| _ = try writer.print("{}", .{i}),
        .obj => |o| {
            switch (o.*) {
                .list => |l| try printList(l, writer),
                .map => |m| try printMap(m, writer),
                .range => |r| _ = try writer.print("{}..<{}", .{ r[0], r[1] }),
                .string => |s| _ = try writer.print("{s}", .{std.mem.sliceTo(s, 0)}),
                else => {},
            }
        },
        .uint => |u| _ = try writer.print("{}", .{u}),

        else => {},
    }
}

fn printList(list: std.ArrayList(Value), writer: anytype) !void {
    try writer.writeByte('[');

    for (list.items) |element, i| {
        if (i != 0) try writer.writeAll(", ");
        _ = try writer.print("{}", .{element});
    }

    try writer.writeByte(']');
}
fn printMap(map: std.StringHashMap(Value), writer: anytype) !void {
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
    name: []const u8,
    params: [][]const u8,
};

pub fn eql(self: Value, other: Value) bool {
    if (self.asFloat()) |f1| {
        if (other.asFloat()) |f2| return f1.ty.float == f2.ty.float;
    }

    if (!self.eqlType(other)) return false;

    return switch (self.ty) {
        .boolean => |b| b == other.ty.boolean,
        .obj => |o| obj: {
            if (o != other.ty.obj) return false;

            break :obj switch (o.*) {
                .list => |l| lst: {
                    if (l.items.len != other.ty.obj.list.items.len) break :lst false;
                    break :lst for (l.items) |item, i| {
                        if (!item.eql(other.ty.obj.list.items[i])) break false;
                    } else true;
                },
                .map => |m| mp: {
                    if (m.count() != other.ty.obj.map.count()) break :mp false;
                    var iter = m.iterator();
                    break :mp while (iter.next()) |entry| {
                        if (other.ty.obj.map.get(entry.key_ptr.*)) |ov| {
                            if (!entry.value_ptr.eql(ov)) break false;
                        }
                    } else true;
                },
                .string => |s| std.mem.eql(u8, std.mem.sliceTo(s, 0), std.mem.sliceTo(other.ty.obj.string, 0)),
                else => false,
            };
        },
        .nil => other.ty == .nil,
        else => false,
    };
}

pub fn eqlType(self: Value, other: Value) bool {
    return switch (self.ty) {
        .boolean => other.ty == .boolean,
        .float => other.ty == .float,
        .int => other.ty == .int,
        .obj => |o| obj: {
            if (other.ty != .obj) break :obj false;
            break :obj switch (o.*) {
                .func => other.ty.obj.* == .func,
                .list => other.ty.obj.* == .list,
                .map => other.ty.obj.* == .map,
                .range => other.ty.obj.* == .range,
                .string => other.ty.obj.* == .string,
            };
        },
        .uint => other.ty == .uint,
        .nil => other.ty == .nil,
    };
}

pub fn asFloat(self: Value) ?Value {
    return switch (self.ty) {
        .float => self,
        .int => |i| Value.new(.{ .float = @intToFloat(f64, i) }),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj if (std.fmt.parseFloat(f64, std.mem.sliceTo(o.string, 0))) |f| Value.new(.{ .float = f }) else |_| null;
            }
            break :obj null;
        },
        .uint => |u| Value.new(.{ .float = @intToFloat(f64, u) }),

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
    const str = std.mem.sliceTo(self.ty.obj.string, 0);

    return if (isFloatStr(str))
        Value.new(.{ .float = try std.fmt.parseFloat(f64, str) })
    else if ('-' == str[0] or '+' == str[0])
        Value.new(.{ .int = try std.fmt.parseInt(isize, str, 0) })
    else
        Value.new(.{ .uint = try std.fmt.parseUnsigned(usize, str, 0) });
}

fn addString(self: Value, other: Value) anyerror!Value {
    const converted = try strToNum(self);
    return converted.add(other);
}

fn addFloat(self: Value, other: Value) anyerror!Value {
    if (other.ty == .float) return Value.new(.{ .float = self.ty.float + other.ty.float });
    const other_float = other.asFloat() orelse return error.InvalidAddition;
    return Value.new(.{ .float = self.ty.float + other_float.ty.float });
}

fn addInt(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .int => Value.new(.{ .int = self.ty.int + other.ty.int }),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.int) + other.ty.float }),
        .uint => Value.new(.{ .int = self.ty.int + @intCast(isize, other.ty.uint) }),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.add(try other.strToNum());
            }
            break :obj error.InvalidAddition;
        },
        else => error.InvalidAddition,
    };
}

fn addUint(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .uint => Value.new(.{ .uint = self.ty.uint + other.ty.uint }),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.uint) + other.ty.float }),
        .int => Value.new(.{ .int = @intCast(isize, self.ty.uint) + other.ty.int }),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.add(try other.strToNum());
            }
            break :obj error.InvalidAddition;
        },
        else => error.InvalidAddition,
    };
}

pub fn add(self: Value, other: Value) anyerror!Value {
    return switch (self.ty) {
        .float => self.addFloat(other),
        .int => self.addInt(other),
        .uint => self.addUint(other),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.add(try other.strToNum());
            }
            break :obj error.InvalidAddition;
        },
        else => error.InvalidAddition,
    };
}

fn subString(self: Value, other: Value) anyerror!Value {
    const converted = try strToNum(self);
    return converted.sub(other);
}

fn subFloat(self: Value, other: Value) anyerror!Value {
    if (other.ty == .float) return Value.new(.{ .float = self.ty.float - other.ty.float });
    const other_float = other.asFloat() orelse return error.InvalidSubtraction;
    return Value.new(.{ .float = self.ty.float - other_float.ty.float });
}

fn subInt(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .int => Value.new(.{ .int = self.ty.int - other.ty.int }),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.int) - other.ty.float }),
        .uint => Value.new(.{ .int = self.ty.int - @intCast(isize, other.ty.uint) }),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.sub(try other.strToNum());
            }
            break :obj error.InvalidSubtraction;
        },
        else => error.InvalidSubtraction,
    };
}

fn subUint(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.uint) - other.ty.float }),
        .int => Value.new(.{ .int = @intCast(isize, self.ty.uint) - other.ty.int }),
        .uint => if (self.ty.uint < other.ty.uint)
            Value.new(.{ .int = @intCast(isize, self.ty.uint) - @intCast(isize, other.ty.uint) })
        else
            Value.new(.{ .uint = self.ty.uint - other.ty.uint }),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.sub(try other.strToNum());
            }
            break :obj error.InvalidSubtraction;
        },
        else => error.InvalidSubtraction,
    };
}
pub fn sub(self: Value, other: Value) anyerror!Value {
    return switch (self.ty) {
        .float => self.subFloat(other),
        .int => self.subInt(other),
        .uint => self.subUint(other),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.sub(try other.strToNum());
            }
            break :obj error.InvalidSubtraction;
        },
        else => error.InvalidSubtraction,
    };
}

fn mulString(self: Value, other: Value) anyerror!Value {
    const converted = try strToNum(self);
    return converted.mul(other);
}

fn mulFloat(self: Value, other: Value) anyerror!Value {
    if (other.ty == .float) return Value.new(.{ .float = self.ty.float * other.ty.float });
    const other_float = other.asFloat() orelse return error.InvalidMultiplication;
    return Value.new(.{ .float = self.ty.float * other_float.ty.float });
}

fn mulInt(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .int => Value.new(.{ .int = self.ty.int * other.ty.int }),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.int) * other.ty.float }),
        .uint => Value.new(.{ .int = self.ty.int * @intCast(isize, other.ty.uint) }),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.mul(try other.strToNum());
            }
            break :obj error.InvalidMultiplication;
        },
        else => error.InvalidMultiplication,
    };
}

fn mulUint(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .uint => Value.new(.{ .uint = self.ty.uint * other.ty.uint }),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.uint) * other.ty.float }),
        .int => Value.new(.{ .int = @intCast(isize, self.ty.uint) * other.ty.int }),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.mul(try other.strToNum());
            }
            break :obj error.InvalidMultiplication;
        },
        else => error.InvalidMultiplication,
    };
}

pub fn mul(self: Value, other: Value) anyerror!Value {
    return switch (self.ty) {
        .float => self.mulFloat(other),
        .int => self.mulInt(other),
        .uint => self.mulUint(other),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.mul(try other.strToNum());
            }
            break :obj error.InvalidMultiplication;
        },
        else => error.InvalidMultiplication,
    };
}

fn divString(self: Value, other: Value) anyerror!Value {
    const converted = try strToNum(self);
    return converted.div(other);
}

fn divFloat(self: Value, other: Value) anyerror!Value {
    if (other.ty == .float) return Value.new(.{ .float = self.ty.float / other.ty.float });
    const other_float = other.asFloat() orelse return error.InvaidDivision;
    return Value.new(.{ .float = self.ty.float / other_float.ty.float });
}

fn divInt(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .int => Value.new(.{ .int = @divFloor(self.ty.int, other.ty.int) }),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.int) / other.ty.float }),
        .uint => Value.new(.{ .int = @divFloor(self.ty.int, @intCast(isize, other.ty.uint)) }),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.div(try other.strToNum());
            }
            break :obj error.InvalidDivision;
        },
        else => error.InvaidDivision,
    };
}

fn divUint(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .uint => Value.new(.{ .uint = @divFloor(self.ty.uint, other.ty.uint) }),
        .float => Value.new(.{ .float = @intToFloat(f64, self.ty.uint) / other.ty.float }),
        .int => Value.new(.{ .int = @divFloor(@intCast(isize, self.ty.uint), other.ty.int) }),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.div(try other.strToNum());
            }
            break :obj error.InvalidDivision;
        },
        else => error.InvaidDivision,
    };
}

pub fn div(self: Value, other: Value) anyerror!Value {
    return switch (self.ty) {
        .float => self.divFloat(other),
        .int => self.divInt(other),
        .uint => self.divUint(other),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.div(try other.strToNum());
            }
            break :obj error.InvalidDivision;
        },
        else => error.InvaidDivision,
    };
}

fn modString(self: Value, other: Value) anyerror!Value {
    const converted = try strToNum(self);
    return converted.mod(other);
}

fn modFloat(self: Value, other: Value) anyerror!Value {
    if (other.ty == .float) return Value.new(.{ .float = @rem(self.ty.float, other.ty.float) });
    const other_float = other.asFloat() orelse return error.InvalidModulo;
    return Value.new(.{ .float = @rem(self.ty.float, other_float.ty.float) });
}

fn modInt(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .int => Value.new(.{ .int = @rem(self.ty.int, other.ty.int) }),
        .float => Value.new(.{ .float = @rem(@intToFloat(f64, self.ty.int), other.ty.float) }),
        .uint => Value.new(.{ .int = @rem(self.ty.int, @intCast(isize, other.ty.uint)) }),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.mod(try other.strToNum());
            }
            break :obj error.InvalidModulo;
        },
        else => error.InvalidModulo,
    };
}

fn modUint(self: Value, other: Value) anyerror!Value {
    return switch (other.ty) {
        .uint => Value.new(.{ .uint = @rem(self.ty.uint, other.ty.uint) }),
        .float => Value.new(.{ .float = @rem(@intToFloat(f64, self.ty.uint), other.ty.float) }),
        .int => Value.new(.{ .int = @rem(@intCast(isize, self.ty.uint), other.ty.int) }),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.mod(try other.strToNum());
            }
            break :obj error.InvalidModulo;
        },
        else => error.InvalidModulo,
    };
}

pub fn mod(self: Value, other: Value) anyerror!Value {
    return switch (self.ty) {
        .float => self.modFloat(other),
        .int => self.modInt(other),
        .uint => self.modUint(other),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.mod(try other.strToNum());
            }
            break :obj error.InvalidModulo;
        },
        else => error.InvalidModulo,
    };
}

fn cmpString(self: Value, other: Value) anyerror!std.math.Order {
    if (other.ty == .obj and other.ty.obj.* == .string) {
        const str_a = std.mem.sliceTo(self.ty.obj.string, 0);
        const str_b = std.mem.sliceTo(other.ty.obj.string, 0);
        return std.mem.order(u8, str_a, str_b);
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
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.cmp(try other.strToNum());
            }
            break :obj error.InvalidComparison;
        },
        else => error.InvalidComparison,
    };
}

fn cmpUint(self: Value, other: Value) anyerror!std.math.Order {
    return switch (other.ty) {
        .uint => std.math.order(self.ty.uint, other.ty.uint),
        .float => std.math.order(@intToFloat(f64, self.ty.uint), other.ty.float),
        .int => std.math.order(@intCast(isize, self.ty.uint), other.ty.int),
        .obj => |o| obj: {
            if (o.* == .string) {
                break :obj self.cmp(try other.strToNum());
            }
            break :obj error.InvalidComparison;
        },
        else => error.InvalidComparison,
    };
}

pub fn cmp(self: Value, other: Value) anyerror!std.math.Order {
    return switch (self.ty) {
        .float => self.cmpFloat(other),
        .int => self.cmpInt(other),
        .uint => self.cmpUint(other),
        .obj => |o| obj: {
            if (o.* == .string) break :obj self.cmpString(other);
            break :obj error.InvalidComparison;
        },
        else => error.InvalidComparison,
    };
}

pub fn lessThan(_: void, a: Value, b: Value) bool {
    return a.cmp(b) catch unreachable == .lt;
}
pub fn greaterThan(_: void, a: Value, b: Value) bool {
    return a.cmp(b) catch unreachable == .gt;
}
