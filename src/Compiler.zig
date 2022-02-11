const std = @import("std");

const Bytecode = @import("Bytecode.zig");
const Node = @import("Node.zig");
const Program = @import("Parser.zig").Program;
const Value = @import("Value.zig");

allocator: std.mem.Allocator,
compile_Context: ?*CompileContext = null,
constants: std.ArrayList(Value),
current_loop_start: ?*Index = null, // Index of instruction at current loop start; if applicable.
instructions: *std.ArrayList(u8),
jump_updates: ?*JumpUpdates = null, // Indexes of jump instruciton operands that need updating.
stash: ?*std.ArrayList(u8) = null,

const CompileContext = struct {
    instructions: std.ArrayList(u8),
    prev: ?*CompileContext = null,
};

const Index = struct {
    index: u16,
    prev: ?*Index = null,
};

const JumpUpdates = struct {
    prev: ?*JumpUpdates = null,
    updates: std.ArrayList(usize),
};

const Compiler = @This();

pub fn init(allocator: std.mem.Allocator) !Compiler {
    var self = Compiler{
        .allocator = allocator,
        .constants = std.ArrayList(Value).init(allocator),
        .instructions = undefined,
    };
    try self.pushCompileContext();
    self.instructions = &self.compile_Context.?.instructions;
    return self;
}

pub fn compileProgram(self: *Compiler, program: Program) anyerror!void {
    for (program.rules) |node| try self.compile(node);
}

fn compile(self: *Compiler, node: Node) anyerror!void {
    switch (node.ty) {
        .boolean => |b| try self.pushConstant(Value.new(.{ .boolean = b }, node.offset)),
        .float => |f| try self.pushConstant(Value.new(.{ .float = f }, node.offset)),
        .int => |int| try self.pushConstant(Value.new(.{ .int = int }, node.offset)),
        .nil => try self.pushConstant(Value.new(.nil, node.offset)),
        .stmt_end => try self.pushInstruction(.pop),
        .uint => |u| try self.pushConstant(Value.new(.{ .uint = u }, node.offset)),

        .assign => try self.compileAssign(node),
        .call => try self.compileCall(node),
        .conditional => try self.compileConditional(node),
        .define => try self.compileDefine(node),
        .func => try self.compileFunc(node),
        .func_return => try self.compileReturn(node),
        .ident => try self.compileIdent(node),
        .infix => try self.compileInfix(node),
        .list => try self.compileList(node),
        .loop => try self.compileLoop(node),
        .loop_break => try self.compileBreak(),
        .loop_continue => try self.compileContinue(),
        .map => try self.compileMap(node),
        .prefix => try self.compilePrefix(node),
        .range => try self.compileRange(node),
        .string => try self.compileString(node),
        .subscript => try self.compileSubscript(node),
    }
}

// Eval functions
fn compileAssign(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.assign.rvalue.*);

    if (node.ty.assign.lvalue.ty == .ident) {
        try self.pushConstant(Value.new(.{ .string = node.ty.assign.lvalue.ty.ident }, node.ty.assign.lvalue.offset));
        try self.pushInstruction(.store);
        try self.instructions.append(@enumToInt(node.ty.assign.combo));
    } else {
        try self.compile(node.ty.assign.lvalue.ty.subscript.index.*);
        try self.compile(node.ty.assign.lvalue.ty.subscript.container.*);
        try self.pushInstruction(.set);
        try self.instructions.append(@enumToInt(node.ty.assign.combo));
    }
}

fn compileBreak(self: *Compiler) anyerror!void {
    try self.pushInstruction(.scope_out);
    try self.instructions.append(1);
    try self.pushInstruction(.jump);
    try self.jump_updates.?.updates.append(try self.pushZeroes(2));
}

fn compileCall(self: *Compiler, node: Node) anyerror!void {
    var i: usize = 1;
    const num_args = node.ty.call.args.len;
    while (i <= num_args) : (i += 1) try self.compile(node.ty.call.args[num_args - i]);
    try self.compile(node.ty.call.callee.*);
    try self.pushInstruction(.call);
    try self.instructions.append(@intCast(u8, num_args));
}

fn compileConditional(self: *Compiler, node: Node) anyerror!void {
    // Condition
    try self.compile(node.ty.conditional.condition.*);
    // Jump if false
    try self.pushInstruction(.jump_false);
    const jump_false_operand_index = try self.pushZeroes(2);
    // Then branch
    try self.pushInstruction(.scope_in);
    try self.instructions.append(0);
    for (node.ty.conditional.then_branch) |n| try self.compile(n);
    try self.pushInstruction(.scope_out);
    try self.instructions.append(0);
    // Unconditional jump
    try self.pushInstruction(.jump);
    const jump_operand_index = try self.pushZeroes(2);
    self.updateJumpIndex(jump_false_operand_index);
    // Else branch
    try self.pushInstruction(.scope_in);
    try self.instructions.append(0);
    for (node.ty.conditional.else_branch) |n| try self.compile(n);
    try self.pushInstruction(.scope_out);
    try self.instructions.append(0);

    self.updateJumpIndex(jump_operand_index);
}

fn compileContinue(self: *Compiler) anyerror!void {
    try self.pushInstruction(.scope_out);
    try self.instructions.append(1);
    try self.pushInstructionAndOperands(u16, .jump, &[_]u16{self.current_loop_start.?.index});
}

fn compileDefine(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.define.rvalue.*);
    try self.pushConstant(Value.new(.{ .string = node.ty.define.lvalue.ty.ident }, node.ty.define.lvalue.offset));
    try self.pushInstruction(.define);
}

fn compileFunc(self: *Compiler, node: Node) anyerror!void {
    // Compile function body.
    try self.pushCompileContext();
    for (node.ty.func.body) |n| try self.compile(n);
    const value = Value.new(.{ .func = .{
        .instructions = self.instructions.items,
        .name = node.ty.func.name,
        .params = node.ty.func.params,
    } }, node.offset);
    self.popCompileContext();
    // Put function value on stack.
    try self.pushConstant(value);
}

fn compileIdent(self: *Compiler, node: Node) anyerror!void {
    try self.pushConstant(Value.new(.{ .string = node.ty.ident }, node.offset));
    try self.pushInstruction(.load);
}

fn compileInfix(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.infix.left.*);
    try self.compile(node.ty.infix.right.*);

    switch (node.ty.infix.op) {
        .punct_plus => try self.pushInstruction(.add),
        .punct_minus => try self.pushInstruction(.sub),
        .punct_star => try self.pushInstruction(.mul),
        .punct_slash => try self.pushInstruction(.div),
        .punct_percent => try self.pushInstruction(.mod),
        .punct_lt => try self.pushInstruction(.lt),
        .op_lte => try self.pushInstruction(.lte),
        .punct_gt => try self.pushInstruction(.gt),
        .op_gte => try self.pushInstruction(.gte),
        .op_eq => try self.pushInstruction(.eq),
        .op_neq => try self.pushInstruction(.neq),
        //TODO: Short circuit logic and / or
        .kw_and => try self.pushInstruction(.logic_and),
        .kw_or => try self.pushInstruction(.logic_or),
        else => unreachable,
    }
}

fn compileList(self: *Compiler, node: Node) anyerror!void {
    var i: usize = 1;
    const list_len = node.ty.list.len;
    while (i <= list_len) : (i += 1) try self.compile(node.ty.list[list_len - i]);
    try self.pushInstructionAndOperands(u16, .list, &[_]u16{@intCast(u16, node.ty.list.len)});
}

fn compileMap(self: *Compiler, node: Node) anyerror!void {
    for (node.ty.map) |entry| {
        try self.compile(entry.key);
        try self.compile(entry.value);
    }
    try self.pushInstructionAndOperands(u16, .map, &[_]u16{@intCast(u16, node.ty.map.len)});
}

fn compilePrefix(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.prefix.operand.*);

    switch (node.ty.prefix.op) {
        .op_neg => try self.pushInstruction(.negative),
        .punct_bang => try self.pushInstruction(.logic_not),
        else => unreachable,
    }
}

fn compileLoop(self: *Compiler, node: Node) anyerror!void {
    // Breaks
    try self.pushJumpUpdates();
    defer self.popJumpUpdates();

    // Iterate / Continues
    try self.pushCurrentLoopIndex();
    defer self.popCurrentLoopIndex();

    // Condition
    try self.compile(node.ty.loop.condition.*);

    // Jump if false
    try self.pushInstruction(.jump_false);
    try self.jump_updates.?.updates.append(try self.pushZeroes(2));

    // Body
    try self.pushInstruction(.scope_in);
    try self.instructions.append(1);
    for (node.ty.loop.body) |n| try self.compile(n);
    try self.pushInstruction(.scope_out);
    try self.instructions.append(1);

    // Unconditional jump
    try self.pushInstructionAndOperands(u16, .jump, &[_]u16{self.current_loop_start.?.index});

    // Update break out jumps.
    while (self.jump_updates.?.updates.popOrNull()) |index| self.updateJumpIndex(index);

    // while loops always return nul.
    try self.pushConstant(Value.new(.nil, node.offset));
}

fn compileRange(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.range.from.*);
    try self.compile(node.ty.range.to.*);
    try self.pushInstruction(.range);
    try self.instructions.append(@boolToInt(node.ty.range.inclusive));
}

fn compileReturn(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.func_return.*);
    try self.pushInstruction(.func_return);
}

fn compileString(self: *Compiler, node: Node) anyerror!void {
    const len = node.ty.string.len;
    var i: usize = 1;
    while (i <= len) : (i += 1) {
        const segment = node.ty.string[len - i];

        switch (segment) {
            .plain => |plain| {
                const value = Value.new(.{ .string = plain }, node.offset);
                try self.pushConstant(value);
            },
            .ipol => |ipol| {
                try self.pushInstruction(.scope_in);
                try self.instructions.append(0);
                for (ipol.nodes) |n| try self.compile(n);
                try self.pushInstruction(.scope_out);
                try self.instructions.append(0);

                if (ipol.format) |spec| {
                    try self.pushConstant(Value.new(.{ .string = spec }, node.offset));
                    try self.pushInstruction(.format);
                }
            },
        }
    }

    try self.pushInstructionAndOperands(u16, .string, &[_]u16{@intCast(u16, len)});
}

fn compileSubscript(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.subscript.index.*);
    try self.compile(node.ty.subscript.container.*);
    try self.pushInstruction(.subscript);
}

// Helpers
fn pushConstant(self: *Compiler, value: Value) !void {
    try self.pushInstructionAndOperands(u16, .constant, &[_]u16{@intCast(u16, self.constants.items.len)});
    try self.constants.append(value);
}

fn pushInstruction(self: *Compiler, instruction: Bytecode.Opcode) !void {
    try self.instructions.append(@enumToInt(instruction));
}

fn pushOperands(self: *Compiler, comptime T: type, operands: []const T) !void {
    const operands_bytes = std.mem.sliceAsBytes(operands);
    try self.instructions.appendSlice(operands_bytes);
}

fn pushInstructionAndOperands(self: *Compiler, comptime T: type, opcode: Bytecode.Opcode, operands: []const T) !void {
    try self.pushInstruction(opcode);
    try self.pushOperands(T, operands);
}

fn pushJumpUpdates(self: *Compiler) anyerror!void {
    var jump_updates_ptr = try self.allocator.create(JumpUpdates);
    jump_updates_ptr.* = .{ .prev = self.jump_updates, .updates = std.ArrayList(usize).init(self.allocator) };
    self.jump_updates = jump_updates_ptr;
    //TODO: Try this.
    // jump_updates_ptr.updates.deinit();
    //self.allocator.destroy(jump_updates_ptr);
}

fn popJumpUpdates(self: *Compiler) void {
    std.debug.assert(self.jump_updates != null);
    self.jump_updates = self.jump_updates.?.prev;
}

fn pushCompileContext(self: *Compiler) anyerror!void {
    const ctx_ptr = try self.allocator.create(CompileContext);
    ctx_ptr.* = .{ .instructions = std.ArrayList(u8).init(self.allocator), .prev = self.compile_Context };
    self.compile_Context = ctx_ptr;
    self.instructions = &self.compile_Context.?.instructions;
}

fn popCompileContext(self: *Compiler) void {
    std.debug.assert(self.compile_Context != null);
    self.compile_Context = self.compile_Context.?.prev;
    self.instructions = &self.compile_Context.?.instructions;
}

fn pushCurrentLoopIndex(self: *Compiler) anyerror!void {
    var current_loop_start_ptr = try self.allocator.create(Index);
    current_loop_start_ptr.* = .{ .index = @intCast(u16, self.instructions.items.len), .prev = self.current_loop_start };
    self.current_loop_start = current_loop_start_ptr;
}

fn popCurrentLoopIndex(self: *Compiler) void {
    std.debug.assert(self.current_loop_start != null);
    self.current_loop_start = self.current_loop_start.?.prev;
    //TODO: Try this.
    //self.allocator.destroy(current_loop_start_ptr);
}

// Returns index of first byte pushed.
fn pushZeroes(self: *Compiler, n: usize) !usize {
    var i: usize = 0;
    while (i < n) : (i += 1) try self.instructions.append(0);
    return self.instructions.items.len - n;
}

fn updateJumpIndex(self: *Compiler, ins_index: usize) void {
    std.debug.assert(ins_index < self.instructions.items.len);
    var jump_index_bytes = std.mem.sliceAsBytes(&[_]u16{@intCast(u16, self.instructions.items.len)});
    self.instructions.items[ins_index] = jump_index_bytes[0];
    self.instructions.items[ins_index + 1] = jump_index_bytes[1];
}

fn lastInstructionIs(self: Compiler, op: Bytecode.Opcode) bool {
    if (self.instructions.items.len == 0) return false;
    return self.instructions.items[self.instructions.items.len - 1] == @enumToInt(op);
}

// Tests

test "Compiler booleans" {
    const Lexer = @import("Lexer.zig");
    const Parser = @import("Parser.zig");

    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const input = "false true";
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
    try compiler.compileProgram(program);

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
