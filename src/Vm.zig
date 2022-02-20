const std = @import("std");

const Compiler = @import("Compiler.zig");
const Value = @import("Value.zig");

frame_stack: std.ArrayList(Frame),
instructions: []const u8 = undefined,
ip: *u16 = undefined,

last_popped: Value = Value.new(.nil, 0),
value_stack: Value.ValueStack,

const Vm = @This();

pub fn init(allocator: std.mem.Allocator, instructions: []const u8) !Vm {
    var self = Vm{
        .frame_stack = std.ArrayList(Frame).init(allocator),
        .value_stack = Value.ValueStack.init(allocator),
    };
    try self.pushFrame(instructions);
    return self;
}

fn getOffset(self: *Vm) u16 {
    return std.mem.bytesAsSlice(u16, self.instructions[self.ip.* + 1 .. self.ip.* + 3])[0];
}

pub fn run(self: *Vm) !void {
    while (self.ip.* < self.instructions.len) {
        const opcode = @intToEnum(Compiler.Opcode, self.instructions[self.ip.*]);

        switch (opcode) {
            // Stack operations
            .pop => {
                self.last_popped = self.value_stack.pop();
                self.ip.* += 1;
            },
            // Predefined constant values
            .bool_false => {
                try self.value_stack.push(Value.new(.{ .boolean = false }, self.getOffset()));
                self.ip.* += 3;
            },
            .bool_true => {
                try self.value_stack.push(Value.new(.{ .boolean = true }, self.getOffset()));
                self.ip.* += 3;
            },
            .nil => {
                try self.value_stack.push(Value.new(.nil, self.getOffset()));
                self.ip.* += 3;
            },
        }
    }
}

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

test "Compiler predefined constant values" {
    const Lexer = @import("Lexer.zig");
    const Parser = @import("Parser.zig");

    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const input = "true false nil";

    var lexer = Lexer{
        .allocator = arena.allocator(),
        .filename = "inline",
        .src = input,
    };
    var tokens = try lexer.lex();

    var parser = Parser{
        .allocator = arena.allocator(),
        .filename = "inline",
        .src = input,
        .tokens = tokens,
    };
    const program = try parser.parse();

    var compiler = try Compiler.init(arena.allocator());
    for (program.rules) |n| try compiler.compile(n);

    var vm = try init(arena.allocator(), compiler.instructions.items);
    try vm.run();

    try std.testing.expectEqual(@as(usize, 0), vm.value_stack.bytes.items.len);
    try std.testing.expectEqual(Value.Tag.nil, vm.last_popped.ty);
    try std.testing.expectEqual(@as(u16, 11), vm.last_popped.offset);
}
