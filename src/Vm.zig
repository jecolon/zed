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

test "Vm eval booleans" {
    const Lexer = @import("Lexer.zig");
    const Parser = @import("Parser.zig");
    const Compiler = @import("Compiler.zig");

    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const input = "false true";
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

    try std.testing.expectEqual(vm.last_popped.?.ty.boolean, true);
}
