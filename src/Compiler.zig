const std = @import("std");

const Node = @import("Node.zig");

pub const Opcode = enum {
    // Predefined constant values
    bool_true,
    bool_false,
    nil,
    // Stack operations
    pop,
};

pub fn compile(self: *Compiler, node: Node) anyerror!void {
    switch (node.ty) {
        // Predefined constant values
        .boolean => |b| {
            try self.pushInstruction(if (b) .bool_true else .bool_false);
            try self.pushOffset(node.offset);
        },
        .nil => {
            try self.pushInstruction(.nil);
            try self.pushOffset(node.offset);
        },
        // Stack operations
        .stmt_end => try self.pushInstruction(.pop),

        else => unreachable,
    }
}

allocator: std.mem.Allocator,
ctx_stack: std.ArrayList(std.ArrayList(u8)),
instructions: *std.ArrayList(u8) = undefined,

const Compiler = @This();

pub fn init(allocator: std.mem.Allocator) !Compiler {
    var self = Compiler{
        .allocator = allocator,
        .ctx_stack = std.ArrayList(std.ArrayList(u8)).init(allocator),
    };
    try self.pushContext();
    return self;
}

fn head(self: Compiler) *std.ArrayList(u8) {
    std.debug.assert(self.ctx_stack.items.len != 0);
    return &self.ctx_stack.items[self.ctx_stack.items.len - 1];
}

fn pushContext(self: *Compiler) !void {
    try self.ctx_stack.append(std.ArrayList(u8).init(self.allocator));
    self.instructions = self.head();
}

fn popContext(self: *Compiler) []const u8 {
    std.debug.assert(self.ctx_stack.items.len > 1);
    var popped_instructions = self.ctx_stack.pop();
    self.instructions = self.head();
    return popped_instructions.toOwnedSlice();
}

fn pushInstruction(self: *Compiler, opcode: Opcode) !void {
    try self.instructions.append(@enumToInt(opcode));
}

fn pushOffset(self: *Compiler, offset: u16) !void {
    try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{offset}));
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

    var compiler = try init(arena.allocator());
    for (program.rules) |n| try compiler.compile(n);

    try std.testing.expectEqual(@as(usize, 12), compiler.instructions.items.len);
    try std.testing.expectEqual(Opcode.bool_true, @intToEnum(Opcode, compiler.instructions.items[0]));
    try std.testing.expectEqual(@as(u16, 0), std.mem.bytesAsSlice(u16, compiler.instructions.items[1..3])[0]);
    try std.testing.expectEqual(Opcode.pop, @intToEnum(Opcode, compiler.instructions.items[3]));
    try std.testing.expectEqual(Opcode.bool_false, @intToEnum(Opcode, compiler.instructions.items[4]));
    try std.testing.expectEqual(@as(u16, 5), std.mem.bytesAsSlice(u16, compiler.instructions.items[5..7])[0]);
    try std.testing.expectEqual(Opcode.pop, @intToEnum(Opcode, compiler.instructions.items[7]));
    try std.testing.expectEqual(Opcode.nil, @intToEnum(Opcode, compiler.instructions.items[8]));
    try std.testing.expectEqual(@as(u16, 11), std.mem.bytesAsSlice(u16, compiler.instructions.items[9..11])[0]);
    try std.testing.expectEqual(Opcode.pop, @intToEnum(Opcode, compiler.instructions.items[11]));
}
