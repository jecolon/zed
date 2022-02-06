const std = @import("std");

const Bytecode = @import("Bytecode.zig");
const Node = @import("Node.zig");
const Program = @import("Parser.zig").Program;
const Value = @import("Value.zig");

allocator: std.mem.Allocator,
constants: std.ArrayList(Value),
instructions: *std.ArrayList(u8),
stash: ?*std.ArrayList(u8) = null,

const Compiler = @This();

pub fn init(allocator: std.mem.Allocator) !Compiler {
    var self = .{
        .allocator = allocator,
        .constants = std.ArrayList(Value).init(allocator),
        .instructions = try allocator.create(std.ArrayList(u8)),
    };
    self.instructions.* = std.ArrayList(u8).init(allocator);
    return self;
}

pub fn compile(self: *Compiler, program: Program) anyerror!void {
    for (program.rules) |node, i| {
        switch (node.ty) {
            .boolean => |b| try self.pushConstant(Value.new(.{ .boolean = b }, @intCast(u16, i), node.offset)),
            .stmt_end => try self.pushInstruction(.pop),
        }
    }
}

// Helpers
fn pushConstant(self: *Compiler, value: Value) !void {
    try self.instructions.append(@enumToInt(Bytecode.Opcode.constant));
    const index_bytes = std.mem.sliceAsBytes(&[_]u16{@intCast(u16, self.constants.items.len)});
    try self.instructions.appendSlice(index_bytes);
    try self.constants.append(value);
}

fn pushInstruction(self: *Compiler, instruction: Bytecode.Opcode) !void {
    try self.instructions.append(@enumToInt(instruction));
}

test "Compiler booleans" {
    const Lexer = @import("Lexer.zig");
    const Parser = @import("Parser.zig");

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
    var compiler = try init(arena.allocator());
    try compiler.compile(program);

    try std.testing.expectEqual(@as(usize, 8), compiler.instructions.items.len);
    try std.testing.expectEqual(Bytecode.Opcode.constant, @intToEnum(Bytecode.Opcode, compiler.instructions.items[0]));
    var index = std.mem.bytesAsSlice(u16, compiler.instructions.items[1..3])[0];
    try std.testing.expectEqual(@as(u16, 0), index);
    try std.testing.expectEqual(Bytecode.Opcode.pop, @intToEnum(Bytecode.Opcode, compiler.instructions.items[3]));
    try std.testing.expectEqual(@as(usize, 2), compiler.constants.items.len);
    index = std.mem.bytesAsSlice(u16, compiler.instructions.items[5..7])[0];
    try std.testing.expectEqual(@as(u16, 1), index);
    try std.testing.expectEqual(Bytecode.Opcode.pop, @intToEnum(Bytecode.Opcode, compiler.instructions.items[3]));
    try std.testing.expectEqual(false, compiler.constants.items[0].ty.boolean);
    try std.testing.expectEqual(true, compiler.constants.items[1].ty.boolean);
}
