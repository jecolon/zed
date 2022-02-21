const std = @import("std");

const Value = @This();

pub const Tag = enum {
    boolean,
    float,
    int,
    nil,
    string,
    uint,
};

pub const Type = union(Tag) {
    boolean: bool,
    float: f64,
    int: i64,
    nil,
    string: []const u8,
    uint: u64,
};

offset: u16,
ty: Type,

pub fn new(ty: Type, offset: u16) Value {
    return .{ .offset = offset, .ty = ty };
}

pub fn deinit(_: Value, _: std.mem.Allocator) void {}

pub fn asBytes(self: Value, bytes: *std.ArrayList(u8)) !void {
    switch (self.ty) {
        // Predefined constant values
        .boolean => |b| {
            try bytes.append(if (b) 1 else 0);
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]u16{self.offset}));
            try bytes.append(@enumToInt(Tag.boolean));
        },
        .nil => {
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]u16{self.offset}));
            try bytes.append(@enumToInt(Tag.nil));
        },
        // Numbers
        .float => |f| {
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]f64{f}));
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]u16{self.offset}));
            try bytes.append(@enumToInt(Tag.float));
        },
        .int => |i| {
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]i64{i}));
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]u16{self.offset}));
            try bytes.append(@enumToInt(Tag.int));
        },
        .uint => |u| {
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]u64{u}));
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]u16{self.offset}));
            try bytes.append(@enumToInt(Tag.uint));
        },
        // Strings
        .string => |s| {
            try bytes.appendSlice(s);
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, s.len)}));
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]u16{self.offset}));
            try bytes.append(@enumToInt(Tag.string));
        },
    }
}

pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = fmt;
    _ = options;

    switch (self.ty) {
        .boolean => |b| _ = try writer.print("{}", .{b}),
        .float => |f| _ = try writer.print("{d}", .{f}),
        .int => |i| _ = try writer.print("{}", .{i}),
        //.list => |l| try printList(l, writer),
        //.map => |m| try printMap(m, writer),
        .string => |s| _ = try writer.print("{s}", .{s}),
        .uint => |u| _ = try writer.print("{}", .{u}),

        else => {},
    }
}

pub const ValueStack = struct {
    allocator: std.mem.Allocator,
    bytes: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) ValueStack {
        return .{
            .allocator = allocator,
            .bytes = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn push(self: *ValueStack, value: Value) !void {
        try value.asBytes(&self.bytes);
    }

    pub fn pop(self: *ValueStack) !Value {
        const tag = self.getTag();
        const offset = self.getU16();

        return switch (tag) {
            // Predefined constant values
            .boolean => Value.new(.{ .boolean = self.bytes.pop() == 1 }, offset),
            .nil => Value.new(.nil, offset),
            // Numbers
            .float => Value.new(.{ .float = self.getNumber(f64) }, offset),
            .int => Value.new(.{ .int = self.getNumber(i64) }, offset),
            .uint => Value.new(.{ .uint = self.getNumber(u64) }, offset),
            // Strings
            .string => str: {
                const len = self.getU16();
                const s = try self.allocator.alloc(u8, len);
                var i: usize = 1;
                while (i <= len) : (i += 1) s[len - i] = self.bytes.pop();
                break :str Value.new(.{ .string = s }, offset);
            },
        };
    }

    fn getU16(self: *ValueStack) u16 {
        var buf: [2]u8 = undefined;
        buf[1] = self.bytes.pop();
        buf[0] = self.bytes.pop();
        return std.mem.bytesAsValue(u16, &buf).*;
    }

    fn getNumber(self: *ValueStack, comptime T: type) T {
        var buf: [8]u8 = undefined;
        var i: usize = 1;
        while (i <= 8) : (i += 1) buf[buf.len - i] = self.bytes.pop();
        return std.mem.bytesAsSlice(T, &buf)[0];
    }

    fn getTag(self: *ValueStack) Tag {
        return @intToEnum(Tag, self.bytes.pop());
    }
};

test "ValueStack push / pop" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var stack = ValueStack.init(allocator);

    try stack.push(Value.new(.{ .boolean = true }, 23));
    var result = try stack.pop();

    try std.testing.expectEqual(Tag.boolean, result.ty);
    try std.testing.expectEqual(true, result.ty.boolean);
    try std.testing.expectEqual(@as(u16, 23), result.offset);

    try stack.push(Value.new(.nil, 42));
    result = try stack.pop();

    try std.testing.expectEqual(Tag.nil, result.ty);
    try std.testing.expectEqual(@as(u16, 42), result.offset);

    try stack.push(Value.new(.{ .float = 3.1415 }, 42));
    result = try stack.pop();

    try std.testing.expectEqual(Tag.float, result.ty);
    try std.testing.expectEqual(@as(f64, 3.1415), result.ty.float);
    try std.testing.expectEqual(@as(u16, 42), result.offset);

    try stack.push(Value.new(.{ .int = -3 }, 42));
    result = try stack.pop();

    try std.testing.expectEqual(Tag.int, result.ty);
    try std.testing.expectEqual(@as(i64, -3), result.ty.int);
    try std.testing.expectEqual(@as(u16, 42), result.offset);

    try stack.push(Value.new(.{ .uint = 9 }, 42));
    result = try stack.pop();

    try std.testing.expectEqual(Tag.uint, result.ty);
    try std.testing.expectEqual(@as(u64, 9), result.ty.uint);
    try std.testing.expectEqual(@as(u16, 42), result.offset);

    try stack.push(Value.new(.{ .string = "foobar" }, 42));
    result = try stack.pop();

    try std.testing.expectEqual(Tag.string, result.ty);
    try std.testing.expectEqualStrings("foobar", result.ty.string);
    try std.testing.expectEqual(@as(u16, 42), result.offset);
}
