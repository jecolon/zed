const std = @import("std");

const Compiler = @import("Compiler.zig");
const Value = @import("Value.zig");

allocator: std.mem.Allocator,
frame_stack: std.ArrayList(Frame),
instructions: []const u8 = undefined,
ip: *u16 = undefined,

last_popped: Value = Value.new(.nil, 0),
value_stack: Value.ValueStack,

const Vm = @This();

pub fn init(allocator: std.mem.Allocator, instructions: []const u8) !Vm {
    var self = Vm{
        .allocator = allocator,
        .frame_stack = std.ArrayList(Frame).init(allocator),
        .value_stack = Value.ValueStack.init(allocator),
    };
    try self.pushFrame(instructions);
    return self;
}

pub fn run(self: *Vm) !void {
    while (self.ip.* < self.instructions.len) {
        const opcode = @intToEnum(Compiler.Opcode, self.instructions[self.ip.*]);

        switch (opcode) {
            // Stack operations
            .pop => {
                self.last_popped = try self.value_stack.pop();
                self.ip.* += 1;
            },
            // Predefined constant values
            .bool_false => {
                try self.value_stack.push(Value.new(.{ .boolean = false }, self.getU16()));
                self.ip.* += 3;
            },
            .bool_true => {
                try self.value_stack.push(Value.new(.{ .boolean = true }, self.getU16()));
                self.ip.* += 3;
            },
            .nil => {
                try self.value_stack.push(Value.new(.nil, self.getU16()));
                self.ip.* += 3;
            },
            // Numbers
            .float => {
                const f = std.mem.bytesAsSlice(f64, self.instructions[self.ip.* + 3 .. self.ip.* + 11])[0];
                try self.value_stack.push(Value.new(.{ .float = f }, self.getU16()));
                self.ip.* += 11;
            },
            .int => {
                const i = std.mem.bytesAsSlice(i64, self.instructions[self.ip.* + 3 .. self.ip.* + 11])[0];
                try self.value_stack.push(Value.new(.{ .int = i }, self.getU16()));
                self.ip.* += 11;
            },
            .uint => {
                const u = std.mem.bytesAsSlice(u64, self.instructions[self.ip.* + 3 .. self.ip.* + 11])[0];
                try self.value_stack.push(Value.new(.{ .uint = u }, self.getU16()));
                self.ip.* += 11;
            },
            // Strings
            .plain => {
                const len = self.getU16();
                const s = self.instructions[self.ip.* + 3 .. self.ip.* + 3 + len];
                try self.value_stack.push(Value.new(.{ .string = s }, 0));
                self.ip.* += 3 + len;
            },
            .string => {
                const offset = self.getU16();
                const len = std.mem.bytesAsSlice(u16, self.instructions[self.ip.* + 3 .. self.ip.* + 5])[0];

                var buf = std.ArrayList(u8).init(self.allocator);
                var writer = buf.writer();
                var i: usize = 0;
                while (i < len) : (i += 1) _ = try writer.print("{}", .{self.value_stack.pop()});
                try self.value_stack.push(Value.new(.{ .string = buf.items }, offset));

                self.ip.* += 5;
            },
        }
    }
}

// Stack Frame

const Frame = struct {
    instructions: []const u8,
    ip: u16 = 0,
};

fn pushFrame(self: *Vm, instructions: []const u8) !void {
    try self.frame_stack.append(.{ .instructions = instructions });
    self.instructions = instructions;
    self.ip = &self.frame_stack.items[self.frame_stack.items.len - 1].ip;
}

fn popFrame(self: *Vm) void {
    _ = self.frame_stack.pop();
    self.instructions = self.frame_stack.items[self.frame_stack.items.len - 1].instructions;
    self.ip = &self.frame_stack.items[self.frame_stack.items.len - 1].ip;
}

// Helpers

fn getU16(self: *Vm) u16 {
    return std.mem.bytesAsSlice(u16, self.instructions[self.ip.* + 1 .. self.ip.* + 3])[0];
}

// Tests

fn testVmValue(allocator: std.mem.Allocator, input: []const u8) !Value {
    const Lexer = @import("Lexer.zig");
    const Parser = @import("Parser.zig");

    var lexer = Lexer{
        .allocator = allocator,
        .filename = "inline",
        .src = input,
    };
    var tokens = try lexer.lex();

    var parser = Parser{
        .allocator = allocator,
        .filename = "inline",
        .src = input,
        .tokens = tokens,
    };
    const program = try parser.parse();

    var compiler = try Compiler.init(allocator);
    for (program.rules) |n| try compiler.compile(n);

    var vm = try init(allocator, compiler.instructions.items);
    try vm.run();

    try std.testing.expectEqual(@as(usize, 0), vm.value_stack.bytes.items.len);
    return vm.last_popped;
}

test "Compiler predefined constant values" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "true");
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(true, got.ty.boolean);
    try std.testing.expectEqual(@as(u16, 0), got.offset);

    got = try testVmValue(allocator, "false");
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(false, got.ty.boolean);
    try std.testing.expectEqual(@as(u16, 0), got.offset);

    got = try testVmValue(allocator, "nil");
    try std.testing.expectEqual(Value.Tag.nil, got.ty);
    try std.testing.expectEqual(@as(u16, 0), got.offset);

    got = try testVmValue(allocator, "3.1415");
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 3.1415), got.ty.float);
    try std.testing.expectEqual(@as(u16, 0), got.offset);

    got = try testVmValue(allocator, "-3");
    try std.testing.expectEqual(Value.Tag.int, got.ty);
    try std.testing.expectEqual(@as(i64, -3), got.ty.int);
    try std.testing.expectEqual(@as(u16, 0), got.offset);

    got = try testVmValue(allocator, "9");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 9), got.ty.uint);
    try std.testing.expectEqual(@as(u16, 0), got.offset);

    got = try testVmValue(allocator, "\"foobar\"");
    try std.testing.expectEqual(Value.Tag.string, got.ty);
    try std.testing.expectEqualStrings("foobar", got.ty.string);
    try std.testing.expectEqual(@as(u16, 0), got.offset);
}
