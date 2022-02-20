const std = @import("std");

const Value = @This();

pub const Tag = enum {
    boolean,
    nil,
};

pub const Type = union(Tag) {
    boolean: bool,
    nil,
};

offset: u16,
ty: Type,

pub fn new(ty: Type, offset: u16) Value {
    return .{ .offset = offset, .ty = ty };
}

pub fn asBytes(self: Value, bytes: *std.ArrayList(u8)) !void {
    switch (self.ty) {
        .boolean => |b| {
            try bytes.append(if (b) 1 else 0);
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]u16{self.offset}));
            try bytes.append(@enumToInt(Tag.boolean));
        },
        .nil => {
            try bytes.appendSlice(std.mem.sliceAsBytes(&[1]u16{self.offset}));
            try bytes.append(@enumToInt(Tag.nil));
        },
    }
}

pub const ValueStack = struct {
    bytes: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) ValueStack {
        return .{ .bytes = std.ArrayList(u8).init(allocator) };
    }

    pub fn push(self: *ValueStack, value: Value) !void {
        try value.asBytes(&self.bytes);
    }

    pub fn pop(self: *ValueStack) Value {
        const tag = self.getTag();
        const offset = self.getOffset();

        return switch (tag) {
            .boolean => Value.new(.{ .boolean = self.bytes.pop() == 1 }, offset),
            .nil => Value.new(.nil, offset),
        };
    }

    fn getOffset(self: *ValueStack) u16 {
        var buf: [2]u8 = undefined;
        buf[1] = self.bytes.pop();
        buf[0] = self.bytes.pop();
        return std.mem.bytesAsValue(u16, &buf).*;
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
    var result = stack.pop();

    try std.testing.expectEqual(Tag.boolean, result.ty);
    try std.testing.expectEqual(true, result.ty.boolean);
    try std.testing.expectEqual(@as(u16, 23), result.offset);

    try stack.push(Value.new(.nil, 42));
    result = stack.pop();

    try std.testing.expectEqual(Tag.nil, result.ty);
    try std.testing.expectEqual(@as(u16, 42), result.offset);
}
