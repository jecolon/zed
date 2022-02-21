const std = @import("std");

const Node = @import("Node.zig");

pub const Opcode = enum {
    // Stack operations
    pop,
    // Predefined constant values
    bool_true,
    bool_false,
    nil,
    // Numbers
    float,
    int,
    uint,
    // Strings
    plain,
    string,
};

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

pub fn compile(self: *Compiler, node: Node) anyerror!void {
    switch (node.ty) {
        // Stack operations
        .stmt_end => try self.pushInstruction(.pop),
        // Predefined constant values
        .boolean => |b| {
            try self.pushInstruction(if (b) .bool_true else .bool_false);
            try self.pushOffset(node.offset);
        },
        .nil => {
            try self.pushInstruction(.nil);
            try self.pushOffset(node.offset);
        },
        // Numbers
        .float => |f| {
            try self.pushInstruction(.float);
            try self.pushOffset(node.offset);
            try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]f64{f}));
        },
        .int => |i| {
            try self.pushInstruction(.int);
            try self.pushOffset(node.offset);
            try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]i64{i}));
        },
        .uint => |u| {
            try self.pushInstruction(.uint);
            try self.pushOffset(node.offset);
            try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u64{u}));
        },
        // Strings
        .string => try self.compileString(node),

        else => unreachable,
    }
}

// Compile functions

fn compileString(self: *Compiler, node: Node) anyerror!void {
    const len = node.ty.string.len;
    var i: usize = 1;
    while (i <= len) : (i += 1) {
        const segment = node.ty.string[len - i];

        switch (segment) {
            .plain => |plain| {
                try self.pushInstruction(.plain);
                try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, plain.len)}));
                try self.instructions.appendSlice(plain);
            },
            .ipol => |_| {
                //try self.pushInstruction(.scope_in);
                //try self.instructions.append(@enumToInt(Scope.Type.block));
                //for (ipol.nodes) |n| try self.compile(n);
                //try self.pushInstruction(.scope_out);
                //try self.instructions.append(@enumToInt(Scope.Type.block));

                //if (ipol.format) |spec| {
                //    try self.pushConstant(Value.new(.{ .string = spec }, node.offset));
                //    try self.pushInstruction(.format);
                //}
            },
        }
    }

    try self.pushInstruction(.string);
    try self.pushOffset(node.offset);
    try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, len)}));
}

// Helpers

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

// Tests

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
