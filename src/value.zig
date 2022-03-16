const std = @import("std");

// Value NaN box.
pub const Value = u64;

// Bit masks
// Quite NaN bits
const mask_qnan: u64 = 0b0_1111111111111_00_0000000000000000_00000000000000000000000000000000;
const mask_sign: u64 = 0b1_0000000000000_00_0000000000000000_00000000000000000000000000000000;
// nil, false, and true
const mask_nil: u64 = 0b0_0000000000000_00_0000000000000000_00000000000000000000000000000001;
const mask_false: u64 = 0b0_0000000000000_00_0000000000000000_00000000000000000000000000000010;
const mask_true: u64 = 0b0_0000000000000_00_0000000000000000_00000000000000000000000000000011;
// Numbers
const mask_uint: u64 = 0b0_0000000000000_01_0000000000000000_00000000000000000000000000000000;
const mask_int: u64 = 0b0_0000000000000_10_0000000000000000_00000000000000000000000000000000;
// Short Strings (len < 7; max 6 bytes; 48 bits)
const mask_str: u64 = 0b0_0000000000000_11_0000000000000000_00000000000000000000000000000000;

// Nil
pub const val_nil = mask_qnan | mask_nil;
// Booleans
pub const val_false = mask_qnan | mask_false;
pub const val_true = mask_qnan | mask_true;

pub fn boolToValue(b: bool) Value {
    return if (b) val_true else val_false;
}
pub fn asBool(v: Value) ?bool {
    return if (isBool(v)) v == val_true else null;
}
pub fn isBool(v: Value) bool {
    return val_true == v or val_false == v;
}
// Floats
pub fn floatToValue(f: f64) Value {
    return @bitCast(Value, f);
}
pub fn asFloat(v: Value) ?f64 {
    return if (isFloat(v)) @bitCast(f64, v) else null;
}
pub fn isFloat(v: Value) bool {
    return (v & mask_qnan) != mask_qnan;
}
// Pointers
pub fn addrToValue(addr: usize) Value {
    return @bitCast(Value, (mask_sign | mask_qnan | addr));
}
pub fn asAddr(v: Value) ?usize {
    return if (isAddr(v)) @bitCast(usize, v & ~(mask_sign | mask_qnan)) else null;
}
pub fn isAddr(v: Value) bool {
    return v & (mask_sign | mask_qnan) == (mask_sign | mask_qnan);
}
// Uints
pub fn uintToValue(u: u32) Value {
    return @bitCast(Value, (mask_uint | mask_qnan | u));
}
pub fn asUint(v: Value) ?u32 {
    return if (isUint(v)) @intCast(u32, v & ~(mask_uint | mask_qnan)) else null;
}
pub fn isUint(v: Value) bool {
    return (v & (mask_uint | mask_qnan) == (mask_uint | mask_qnan)) and (v & mask_str != mask_str);
}
// Ints
pub fn intToValue(i: i32) Value {
    return @bitCast(Value, (mask_int | mask_qnan | @bitCast(u32, i)));
}
pub fn asInt(v: Value) ?i32 {
    return if (isInt(v)) @bitCast(i32, @truncate(u32, v & ~(mask_int | mask_qnan))) else null;
}
pub fn isInt(v: Value) bool {
    return (v & (mask_int | mask_qnan) == (mask_int | mask_qnan)) and (v & mask_str != mask_str);
}
// Short Strings
pub fn strToValue(str: []const u8) Value {
    std.debug.assert(str.len < 7);
    var buf = [_]u8{0} ** 8;
    std.mem.copy(u8, &buf, str);
    const str_u64 = @bitCast(u64, buf);
    return mask_str | mask_qnan | str_u64;
}
// There's no `asStr` because it would require allocation in a function context.
// It's more efficient to just do the following:
// const str_bits = unboxStr(v).?; // isStr(v) == true
// const str = std.mem.asBytes(&str_bits);
pub fn unboxStr(v: Value) ?u64 {
    return if (isStr(v)) v & ~(mask_str | mask_qnan) else null;
}
pub fn isStr(v: Value) bool {
    return v & (mask_str | mask_qnan) == (mask_str | mask_qnan);
}

// Object convenience functions.
pub fn asFunc(v: Value) ?*const Object {
    if (asAddr(v)) |addr| {
        const obj_ptr = @intToPtr(*const Object, addr);
        if (obj_ptr.* == .func) return obj_ptr;
    }
    return null;
}
pub fn asList(v: Value) ?*Object {
    if (asAddr(v)) |addr| {
        const obj_ptr = @intToPtr(*Object, addr);
        if (obj_ptr.* == .list) return obj_ptr;
    }
    return null;
}
pub fn asMap(v: Value) ?*Object {
    if (asAddr(v)) |addr| {
        const obj_ptr = @intToPtr(*Object, addr);
        if (obj_ptr.* == .map) return obj_ptr;
    }
    return null;
}
pub fn asRange(v: Value) ?*const Object {
    if (asAddr(v)) |addr| {
        const obj_ptr = @intToPtr(*const Object, addr);
        if (obj_ptr.* == .range) return obj_ptr;
    }
    return null;
}
pub fn asString(v: Value) ?*const Object {
    if (asAddr(v)) |addr| {
        const obj_ptr = @intToPtr(*const Object, addr);
        if (obj_ptr.* == .string) return obj_ptr;
    }
    return null;
}

pub fn isAnyStr(v: Value) bool {
    if (unboxStr(v)) |_| {
        return true;
    } else if (asAddr(v)) |addr| {
        const obj_ptr = @intToPtr(*const Object, addr);
        return obj_ptr.* == .string;
    } else {
        return false;
    }
}

fn eqlInner(a: Value, b: Value) bool {
    if (isAddr(a) and isAddr(b)) return true;
    if (isFloat(a) and isFloat(b)) return true;
    if (isInt(a) and isInt(b)) return true;
    if (isUint(a) and isUint(b)) return true;
    if (isStr(a) and isStr(b)) return true;
    return false;
}

pub fn eql(a: Value, b: Value) bool {
    // If the bits are equal, they're equal.
    if (a == b) return true;

    // Check inner type equality.
    if (!eqlInner(a, b)) return false;

    if (asAddr(a)) |obj_addr_a| {
        // Deep object equality.
        const obj_ptr_a = @intToPtr(*const Object, obj_addr_a);
        const obj_ptr_b = @intToPtr(*const Object, asAddr(b).?);
        return obj_ptr_a.eqlObject(obj_ptr_b.*);
    }

    return false;
}

pub fn toFloat(v: Value) ?f64 {
    if (asFloat(v)) |f| return f;
    if (asInt(v)) |i| return @intToFloat(f64, i);
    if (asUint(v)) |u| return @intToFloat(f64, u);
    if (unboxStr(v)) |u| return if (std.fmt.parseFloat(f64, std.mem.sliceTo(std.mem.asBytes(&u), 0))) |f| f else |_| null;
    if (asAddr(v)) |addr| {
        const obj_ptr = @intToPtr(*const Object, addr);
        if (obj_ptr.* == .string) return if (std.fmt.parseFloat(f64, obj_ptr.string)) |f| f else |_| null;
    }
    return null;
}

fn isFloatStr(str: []const u8) bool {
    return std.mem.containsAtLeast(u8, str, 1, ".") or
        std.mem.containsAtLeast(u8, str, 1, "e+") or
        std.mem.containsAtLeast(u8, str, 1, "e-") or
        std.mem.containsAtLeast(u8, str, 1, "E+") or
        std.mem.containsAtLeast(u8, str, 1, "E-") or
        std.mem.containsAtLeast(u8, str, 1, "p");
}

fn strToNum(v: Value) Value {
    std.debug.assert(isAnyStr(v));
    var str = if (unboxStr(v)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else asString(v).?.string;

    if (isFloatStr(str)) {
        return if (std.fmt.parseFloat(f64, str)) |f| floatToValue(f) else |_| 0;
    } else if ('-' == str[0] or '+' == str[0]) {
        return if (std.fmt.parseInt(i32, str, 0)) |i| intToValue(i) else |_| 0;
    } else {
        return if (std.fmt.parseInt(u32, str, 0)) |u| uintToValue(u) else |_| 0;
    }
}

// Addition
fn addFloat(a: Value, b: Value) Value {
    if (asFloat(b)) |fb| return floatToValue(asFloat(a).? + fb);
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? + fb);
}
fn addInt(a: Value, b: Value) Value {
    if (asInt(b)) |ib| return intToValue(asInt(a).? + ib);
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? + fb);
}
fn addUint(a: Value, b: Value) Value {
    if (asUint(b)) |ub| return uintToValue(asUint(a).? + ub);
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? + fb);
}
pub fn add(a: Value, b: Value) anyerror!Value {
    if (isFloat(a)) return addFloat(a, b);
    if (isInt(a)) return addInt(a, b);
    if (isUint(a)) return addUint(a, b);
    if (isAnyStr(a)) return add(strToNum(a), b);
    return error.InvalidAdd;
}

// Subtraction
fn subFloat(a: Value, b: Value) Value {
    if (asFloat(b)) |fb| return floatToValue(asFloat(a).? - fb);
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? - fb);
}
fn subInt(a: Value, b: Value) Value {
    if (asInt(b)) |ib| return intToValue(asInt(a).? - ib);
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? - fb);
}
fn subUint(a: Value, b: Value) Value {
    if (asUint(b)) |ub| return uintToValue(asUint(a).? - ub);
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? - fb);
}
pub fn sub(a: Value, b: Value) anyerror!Value {
    if (isFloat(a)) return subFloat(a, b);
    if (isInt(a)) return subInt(a, b);
    if (isUint(a)) return subUint(a, b);
    if (isAnyStr(a)) return sub(strToNum(a), b);
    return error.InvalidSubtract;
}

// Multiplication
fn mulFloat(a: Value, b: Value) Value {
    if (asFloat(b)) |fb| return floatToValue(asFloat(a).? * fb);
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? * fb);
}
fn mulInt(a: Value, b: Value) Value {
    if (asInt(b)) |ib| return intToValue(asInt(a).? * ib);
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? * fb);
}
fn mulUint(a: Value, b: Value) Value {
    if (asUint(b)) |ub| return uintToValue(asUint(a).? * ub);
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? * fb);
}
pub fn mul(a: Value, b: Value) anyerror!Value {
    if (isFloat(a)) return mulFloat(a, b);
    if (isInt(a)) return mulInt(a, b);
    if (isUint(a)) return mulUint(a, b);
    if (isAnyStr(a)) return mul(strToNum(a), b);
    return error.InvalidMultiply;
}

// Division
fn divFloat(a: Value, b: Value) Value {
    if (asFloat(b)) |fb| return floatToValue(asFloat(a).? / fb);
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? / fb);
}
fn divInt(a: Value, b: Value) Value {
    if (asInt(b)) |ib| return intToValue(@divFloor(asInt(a).?, ib));
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? / fb);
}
fn divUint(a: Value, b: Value) Value {
    if (asUint(b)) |ub| return uintToValue(@divFloor(asUint(a).?, ub));
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(asFloat(a).? / fb);
}
pub fn div(a: Value, b: Value) anyerror!Value {
    if (isFloat(a)) return divFloat(a, b);
    if (isInt(a)) return divInt(a, b);
    if (isUint(a)) return divUint(a, b);
    if (isAnyStr(a)) return div(strToNum(a), b);
    return error.InvalidDivide;
}

// Modulo
fn modFloat(a: Value, b: Value) Value {
    if (asFloat(b)) |fb| return floatToValue(@rem(asFloat(a).?, fb));
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(@rem(asFloat(a).?, fb));
}
fn modInt(a: Value, b: Value) Value {
    if (asInt(b)) |ib| return intToValue(@rem(asInt(a).?, ib));
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(@rem(asFloat(a).?, fb));
}
fn modUint(a: Value, b: Value) Value {
    if (asUint(b)) |ub| return uintToValue(@rem(asUint(a).?, ub));
    const fb: f64 = toFloat(b) orelse 0;
    return floatToValue(@rem(asFloat(a).?, fb));
}
pub fn mod(a: Value, b: Value) anyerror!Value {
    if (isFloat(a)) return modFloat(a, b);
    if (isInt(a)) return modInt(a, b);
    if (isUint(a)) return modUint(a, b);
    if (isAnyStr(a)) return mod(strToNum(a), b);
    return error.InvalidModulo;
}

// Multiplication
fn cmpFloat(a: Value, b: Value) std.math.Order {
    if (asFloat(b)) |fb| return std.math.order(asFloat(a).?, fb);
    const fb: f64 = toFloat(b) orelse 0;
    return std.math.order(asFloat(a).?, fb);
}
fn cmpInt(a: Value, b: Value) std.math.Order {
    if (asInt(b)) |ib| return std.math.order(asInt(a).?, ib);
    const fb: f64 = toFloat(b) orelse 0;
    return std.math.order(asFloat(a).?, fb);
}
fn cmpUint(a: Value, b: Value) std.math.Order {
    if (asUint(b)) |ub| return std.math.order(asUint(a).?, ub);
    const fb: f64 = toFloat(b) orelse 0;
    return std.math.order(asFloat(a).?, fb);
}
fn cmpStr(a: Value, b: Value) anyerror!std.math.Order {
    if (isAnyStr(b)) {
        const a_str = if (unboxStr(a)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else asString(a).?.string;
        const b_str = if (unboxStr(b)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else asString(b).?.string;
        return std.mem.order(u8, a_str, b_str);
    }

    return cmp(strToNum(a), b);
}
pub fn cmp(a: Value, b: Value) anyerror!std.math.Order {
    if (isFloat(a)) return cmpFloat(a, b);
    if (isInt(a)) return cmpInt(a, b);
    if (isUint(a)) return cmpUint(a, b);
    if (isAnyStr(a)) return cmpStr(a, b);
    return error.InvalidCompare;
}
pub fn asc(_: void, a: Value, b: Value) bool {
    return cmp(a, b) catch unreachable == .lt;
}
pub fn desc(_: void, a: Value, b: Value) bool {
    return cmp(a, b) catch unreachable == .gt;
}

// Value copy for global scope.
pub fn copy(v: Value, allocator: std.mem.Allocator) anyerror!Value {
    if (val_nil == v) return v;
    if (isBool(v)) return v;
    if (isFloat(v)) return v;
    if (isInt(v)) return v;
    if (isUint(v)) return v;
    if (isStr(v)) return v;
    if (isAddr(v)) return copyObject(v, allocator);
    unreachable;
}

fn copyObject(v: Value, allocator: std.mem.Allocator) anyerror!Value {
    const obj_addr = asAddr(v).?;
    const obj_ptr = @intToPtr(*const Object, obj_addr);

    return switch (obj_ptr.*) {
        .func => |f| copyFunc(allocator, f),
        .list => |l| copyList(allocator, l),
        .map => |m| copyMap(allocator, m),
        .range => |r| copyRange(allocator, r),
        .string => |s| copyString(allocator, s),
    };
}
fn copyFunc(allocator: std.mem.Allocator, func: Function) anyerror!Value {
    var params: ?[]u16 = null;
    if (func.params) |fparams| {
        var params_copy = try allocator.alloc(u16, fparams.len);
        for (fparams) |param, i| params_copy[i] = param;
        params = params_copy;
    }
    var bytecode: ?[]const u8 = null;
    if (func.bytecode) |bc| bytecode = try allocator.dupe(u8, bc);

    const obj_ptr = try allocator.create(Object);
    obj_ptr.* = .{ .func = .{
        .name = func.name,
        .params = params,
        .bytecode = bytecode,
    } };
    const obj_addr = @ptrToInt(obj_ptr);
    return addrToValue(obj_addr);
}
fn copyList(allocator: std.mem.Allocator, list: std.ArrayList(Value)) anyerror!Value {
    var list_copy = try std.ArrayList(Value).initCapacity(allocator, list.items.len);
    for (list.items) |item| list_copy.appendAssumeCapacity(try copy(item, allocator));
    const obj_ptr = try allocator.create(Object);
    obj_ptr.* = .{ .list = list_copy };
    const obj_addr = @ptrToInt(obj_ptr);
    return addrToValue(obj_addr);
}
fn copyMap(allocator: std.mem.Allocator, map: std.StringHashMap(Value)) anyerror!Value {
    var map_copy = std.StringHashMap(Value).init(allocator);
    try map_copy.ensureTotalCapacity(map.count());
    var iter = map.iterator();

    while (iter.next()) |entry| {
        const key_copy = try allocator.dupe(u8, entry.key_ptr.*);
        map_copy.putAssumeCapacity(key_copy, try copy(entry.value_ptr.*, allocator));
    }

    const obj_ptr = try allocator.create(Object);
    obj_ptr.* = .{ .map = map_copy };
    const obj_addr = @ptrToInt(obj_ptr);
    return addrToValue(obj_addr);
}
fn copyRange(allocator: std.mem.Allocator, r: [2]u32) anyerror!Value {
    const obj_ptr = try allocator.create(Object);
    obj_ptr.* = .{ .range = [2]u32{ r[0], r[1] } };
    const obj_addr = @ptrToInt(obj_ptr);
    return addrToValue(obj_addr);
}
fn copyString(allocator: std.mem.Allocator, str: []const u8) anyerror!Value {
    const str_copy = try allocator.dupe(u8, str);
    const obj_ptr = try allocator.create(Object);
    obj_ptr.* = .{ .string = str_copy };
    const obj_addr = @ptrToInt(obj_ptr);
    return addrToValue(obj_addr);
}

// Format interface
pub fn print(v: Value, writer: anytype) !void {
    if (asBool(v)) |b| _ = try writer.print("{}", .{b});
    if (asFloat(v)) |f| _ = try writer.print("{d}", .{f});
    if (asInt(v)) |i| _ = try writer.print("{}", .{i});
    if (asUint(v)) |u| _ = try writer.print("{}", .{u});
    if (unboxStr(v)) |u| _ = try writer.writeAll(std.mem.sliceTo(std.mem.asBytes(&u), 0));
    if (isAddr(v)) try printObject(v, writer);
}

fn printObject(v: Value, writer: anytype) !void {
    const obj_addr = asAddr(v).?;
    const obj_ptr = @intToPtr(*const Object, obj_addr);

    switch (obj_ptr.*) {
        .func => try writer.writeAll("fn()"),
        .list => |l| try printList(l, writer),
        .map => |m| try printMap(m, writer),
        .range => |r| _ = try writer.print("{} ..< {}", .{ r[0], r[1] }),
        .string => |s| try writer.writeAll(s),
    }
}

fn printList(list: std.ArrayList(Value), writer: anytype) !void {
    try writer.writeByte('[');
    for (list.items) |item, i| {
        if (i != 0) try writer.writeAll(", ");
        _ = try writer.print("{}", .{item});
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

// Values accessed by reference.
pub const Object = union(enum) {
    func: Function,
    list: std.ArrayList(Value),
    map: std.StringHashMap(Value),
    range: [2]u32,
    string: []const u8,

    fn eqlTag(a: Object, b: Object) bool {
        return switch (a) {
            .func => b == .func,
            .list => b == .list,
            .map => b == .map,
            .range => b == .range,
            .string => b == .string,
        };
    }

    pub fn eqlObject(a: Object, b: Object) bool {
        if (!eqlTag(a, b)) return false;

        return switch (a) {
            .func => false,
            .list => eqlList(a, b),
            .map => eqlMap(a, b),
            .range => eqlRange(a, b),
            .string => eqlStr(a, b),
        };
    }
    pub fn eqlList(a: Object, b: Object) bool {
        if (a.list.items.len != b.list.items.len) return false;

        return for (a.list.items) |item_a, i| {
            if (!eql(item_a, b.list.items[i])) break false;
        } else true;
    }
    pub fn eqlMap(a: Object, b: Object) bool {
        if (a.map.count() != b.map.count()) return false;

        var iter = a.map.iterator();
        return while (iter.next()) |entry_a| {
            if (b.map.get(entry_a.key_ptr.*)) |vb| {
                if (!eql(entry_a.value_ptr.*, vb)) break false;
            } else break false;
        } else true;
    }
    pub fn eqlRange(a: Object, b: Object) bool {
        return a.range[0] == b.range[0] and a.range[1] == b.range[1];
    }
    pub fn eqlStr(a: Object, b: Object) bool {
        return std.mem.eql(u8, a.string, b.string);
    }
};

// Runtime function value.
pub const Function = struct {
    name: ?u16,
    params: ?[]const u16,
    bytecode: ?[]const u8,
};
