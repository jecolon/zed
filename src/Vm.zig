const std = @import("std");

const Value = @import("Value.zig");
const Bytecode = @import("Bytecode.zig");

const Frame = struct {
    instructions: []const u8,
    ip: u16 = 0,
};

call_stack: std.ArrayList(Frame),
constants: []const Value,
last_popped: ?Value = null,
value_stack: std.ArrayList(Value),

const Vm = @This();

pub fn init(allocator: std.mem.Allocator, constants: []const Value, insts: []const u8) !Vm {
    var self = Vm{
        .call_stack = std.ArrayList(Frame).init(allocator),
        .constants = constants,
        .value_stack = std.ArrayList(Value).init(allocator),
    };
    try self.call_stack.append(.{ .instructions = insts });
    return self;
}

pub fn run(self: *Vm) !void {
    while (self.ip().* < self.instructions().len) {
        const opcode = @intToEnum(Bytecode.Opcode, self.instructions()[self.ip().*]);

        switch (opcode) {
            .pop => {
                self.last_popped = self.value_stack.pop();
                self.ip().* += 1;
            },

            .constant => {
                const ip_ptr = self.ip();
                const index = std.mem.bytesAsSlice(u16, self.instructions()[ip_ptr.* + 1 .. ip_ptr.* + 3])[0];
                try self.value_stack.append(self.constants[index]);
                ip_ptr.* += 3;
            },
        }
    }
}

// Helpers
fn instructions(self: Vm) []const u8 {
    return self.call_stack.items[self.call_stack.items.len - 1].instructions;
}

fn ip(self: *Vm) *u16 {
    return &self.call_stack.items[self.call_stack.items.len - 1].ip;
}

fn testLastValue(input: []const u8, expected: Value) !void {
    const Lexer = @import("Lexer.zig");
    const Parser = @import("Parser.zig");
    const Compiler = @import("Compiler.zig");

    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var lexer = Lexer{ .filename = "inline", .src = input };
    var tokens = try lexer.lex(arena.allocator());
    var parser = Parser{
        .allocator = arena.allocator(),
        .filename = "inline",
        .src = input,
        .tokens = tokens,
    };
    const program = try parser.parse();
    var compiler = try Compiler.init(arena.allocator());
    try compiler.compile(program);
    var vm = try init(arena.allocator(), compiler.constants.items, compiler.instructions.items);
    try vm.run();

    const last_popped = vm.last_popped.?;
    try std.testing.expectEqual(@as(usize, 0), vm.value_stack.items.len);

    switch (expected.ty) {
        .boolean => |b| try std.testing.expectEqual(b, last_popped.ty.boolean),
        .float => |f| try std.testing.expectEqual(f, last_popped.ty.float),
        .int => |i| try std.testing.expectEqual(i, last_popped.ty.int),
        .string => |s| try std.testing.expectEqualStrings(s, last_popped.ty.string),
        .uint => |u| try std.testing.expectEqual(u, last_popped.ty.uint),
    }
}

test "Vm eval booleans" {
    try testLastValue("false true", Value.new(.{ .boolean = true }, 2, 6));
}

test "Vm eval uint" {
    try testLastValue("123", Value.new(.{ .uint = 123 }, 0, 0));
    try testLastValue("0b00000011", Value.new(.{ .uint = 3 }, 0, 0));
}

test "Vm eval int" {
    try testLastValue("-123", Value.new(.{ .int = -123 }, 0, 0));
    try testLastValue("-0b00000011", Value.new(.{ .int = -3 }, 0, 0));
}

test "Vm eval float" {
    try testLastValue("1.23", Value.new(.{ .float = 1.23 }, 0, 0));
    try testLastValue("-1.23", Value.new(.{ .float = -1.23 }, 0, 0));
}

test "Vm eval string" {
    try testLastValue("\"Hello World!\"", Value.new(.{ .string = "Hello World!" }, 0, 0));
}
