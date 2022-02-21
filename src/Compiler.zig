const std = @import("std");

const Node = @import("Node.zig");
const Scope = @import("Scope.zig");

pub const Opcode = enum {
    // Stack operations
    pop,
    // Predefined constant values
    bool_true,
    bool_false,
    nil,
    // Numbers
    float,
    float_ref,
    int,
    int_ref,
    uint,
    uint_ref,
    // Strings
    format,
    plain,
    plain_ref,
    string,
    // Scopes
    scope_in,
    scope_out,
    // Functions
    call,
    func,
    func_return,
    // Variables
    define,
    load,
    store,
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
        .boolean => try self.compileBoolean(node),
        .nil => try self.compileNil(node),
        // Numbers
        .float => try self.compileFloat(node),
        .int => try self.compileInt(node),
        .uint => try self.compileUint(node),
        // Strings
        .string => try self.compileString(node),
        // Functions
        .call => try self.compileCall(node),
        .func => try self.compileFunc(node),
        .func_return => try self.compileReturn(node),
        // Variables
        .define => try self.compileDefine(node),
        .ident => try self.compileLoad(node),
        .assign => try self.compileStore(node),

        else => unreachable,
    }
}

// Compile functions
fn compileBoolean(self: *Compiler, node: Node) !void {
    try self.pushInstruction(if (node.ty.boolean) .bool_true else .bool_false);
    try self.pushOffset(node.offset);
}

fn compileNil(self: *Compiler, node: Node) !void {
    try self.pushInstruction(.nil);
    try self.pushOffset(node.offset);
}

fn compileFloat(self: *Compiler, node: Node) !void {
    var instruction = Opcode.float;
    var slice: []const u8 = std.mem.sliceAsBytes(&[1]f64{node.ty.float});

    if (std.mem.indexOf(u8, self.instructions.items, slice)) |index| {
        instruction = Opcode.float_ref;
        slice = std.mem.sliceAsBytes(&[1]u16{@intCast(u16, index)});
    }

    try self.pushInstruction(instruction);
    try self.pushOffset(node.offset);
    try self.instructions.appendSlice(slice);
}

fn compileInt(self: *Compiler, node: Node) !void {
    var instruction = Opcode.int;
    var slice: []const u8 = std.mem.sliceAsBytes(&[1]i64{node.ty.int});

    if (std.mem.indexOf(u8, self.instructions.items, slice)) |index| {
        instruction = Opcode.int_ref;
        slice = std.mem.sliceAsBytes(&[1]u16{@intCast(u16, index)});
    }

    try self.pushInstruction(instruction);
    try self.pushOffset(node.offset);
    try self.instructions.appendSlice(slice);
}

fn compileUint(self: *Compiler, node: Node) !void {
    var instruction = Opcode.uint;
    var slice: []const u8 = std.mem.sliceAsBytes(&[1]u64{node.ty.uint});

    if (std.mem.indexOf(u8, self.instructions.items, slice)) |index| {
        instruction = Opcode.uint_ref;
        slice = std.mem.sliceAsBytes(&[1]u16{@intCast(u16, index)});
    }

    try self.pushInstruction(instruction);
    try self.pushOffset(node.offset);
    try self.instructions.appendSlice(slice);
}

fn compileString(self: *Compiler, node: Node) anyerror!void {
    const len = node.ty.string.len;
    var i: usize = 1;
    while (i <= len) : (i += 1) {
        const segment = node.ty.string[len - i];

        switch (segment) {
            .plain => |plain| {
                var instruction = Opcode.plain;
                var slice = plain;

                if (std.mem.indexOf(u8, self.instructions.items, slice)) |index| {
                    instruction = Opcode.plain_ref;
                    slice = std.mem.sliceAsBytes(&[_]u16{@intCast(u16, index)});
                }

                try self.pushInstruction(instruction);
                try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, plain.len)}));
                try self.instructions.appendSlice(slice);
            },
            .ipol => |ipol| {
                try self.pushInstruction(.scope_in);
                try self.instructions.append(@enumToInt(Scope.Type.block));
                for (ipol.nodes) |n| try self.compile(n);
                try self.pushInstruction(.scope_out);
                try self.instructions.append(@enumToInt(Scope.Type.block));

                if (ipol.format) |spec| {
                    try self.pushInstruction(.format);
                    try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, spec.len)}));
                    try self.instructions.appendSlice(spec);
                }
            },
        }
    }

    try self.pushInstruction(.string);
    try self.pushOffset(node.offset);
    try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, len)}));
}

fn compileFunc(self: *Compiler, node: Node) anyerror!void {
    // Compile function body to instructions.
    try self.pushContext();
    for (node.ty.func.body) |n| try self.compile(n);
    const func_instructions = self.popContext();

    // Serialize function to bytes.
    try self.pushInstruction(.func);
    try self.pushOffset(node.offset);
    // Function name
    try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, node.ty.func.name.len)}));
    if (node.ty.func.name.len != 0) try self.instructions.appendSlice(node.ty.func.name);
    // Function params
    try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, node.ty.func.params.len)}));
    if (node.ty.func.params.len != 0) {
        for (node.ty.func.params) |param| {
            try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, param.len)}));
            try self.instructions.appendSlice(param);
        }
    }
    // Function instructions
    try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, func_instructions.len)}));
    if (func_instructions.len != 0) try self.instructions.appendSlice(func_instructions);
}

fn compileReturn(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.func_return.*);
    try self.pushInstruction(.func_return);
}

fn compileCall(self: *Compiler, node: Node) anyerror!void {
    var i: usize = 1;
    const num_args = node.ty.call.args.len;
    while (i <= num_args) : (i += 1) try self.compile(node.ty.call.args[num_args - i]);
    try self.compile(node.ty.call.callee.*);
    try self.pushInstruction(.call);
    try self.instructions.append(@intCast(u8, num_args));
}

fn compileDefine(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.define.rvalue.*);
    try self.pushInstruction(.define);
    try self.pushOffset(node.offset);
    try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, node.ty.define.lvalue.ty.ident.len)}));
    try self.instructions.appendSlice(node.ty.define.lvalue.ty.ident);
}

fn compileLoad(self: *Compiler, node: Node) anyerror!void {
    try self.pushInstruction(.load);
    try self.pushOffset(node.offset);
    try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, node.ty.ident.len)}));
    try self.instructions.appendSlice(node.ty.ident);
}

fn compileStore(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.assign.rvalue.*);
    try self.pushInstruction(.store);
    try self.pushOffset(node.offset);
    try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, node.ty.assign.lvalue.ty.ident.len)}));
    try self.instructions.appendSlice(node.ty.assign.lvalue.ty.ident);

    //TODO: Subscript assign
    //if (node.ty.assign.lvalue.ty == .ident) {
    //    try self.pushConstant(Value.new(.{ .string = node.ty.assign.lvalue.ty.ident }, node.ty.assign.lvalue.offset));
    //    try self.pushInstruction(.store);
    //    try self.instructions.append(@enumToInt(node.ty.assign.combo));
    //} else {
    //    try self.compile(node.ty.assign.lvalue.ty.subscript.index.*);
    //    try self.compile(node.ty.assign.lvalue.ty.subscript.container.*);
    //    try self.pushInstruction(.set);
    //    try self.instructions.append(@enumToInt(node.ty.assign.combo));
    //}
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
