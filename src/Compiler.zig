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
    ident,
    ident_ref,
    load,
    set,
    store,
    // Infix
    add,
    sub,
    mul,
    div,
    mod,
    lt,
    lte,
    gt,
    gte,
    eq,
    neq,
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
        // Operators
        .infix => try self.compileInfix(node),

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
    try self.pushSlice(slice);
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
    try self.pushSlice(slice);
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
    try self.pushSlice(slice);
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
                try self.pushLen(plain.len);
                try self.pushSlice(slice);
            },
            .ipol => |ipol| {
                try self.pushInstruction(.scope_in);
                try self.pushEnum(Scope.Type.block);
                for (ipol.nodes) |n| try self.compile(n);
                try self.pushInstruction(.scope_out);
                try self.pushEnum(Scope.Type.block);

                if (ipol.format) |spec| {
                    try self.pushInstruction(.format);
                    try self.pushLen(spec.len);
                    try self.pushSlice(spec);
                }
            },
        }
    }

    try self.pushInstruction(.string);
    try self.pushOffset(node.offset);
    try self.pushLen(len);
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
    try self.pushLen(node.ty.func.name.len);
    if (node.ty.func.name.len != 0) try self.pushSlice(node.ty.func.name);
    // Function params
    try self.pushLen(node.ty.func.params.len);
    if (node.ty.func.params.len != 0) {
        for (node.ty.func.params) |param| {
            try self.pushLen(param.len);
            try self.pushSlice(param);
        }
    }
    // Function instructions
    try self.pushLen(func_instructions.len);
    if (func_instructions.len != 0) try self.pushSlice(func_instructions);
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
    try self.pushByte(num_args);
}

fn compileDefine(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.define.rvalue.*);
    try self.pushInstruction(.define);
    try self.pushOffset(node.offset);
    if (std.mem.indexOf(u8, self.instructions.items, node.ty.define.lvalue.ty.ident)) |index| {
        try self.pushInstruction(.ident_ref);
        try self.pushLen(index); // pushIndex?
        try self.pushLen(node.ty.define.lvalue.ty.ident.len);
    } else {
        try self.pushInstruction(.ident);
        try self.pushLen(node.ty.define.lvalue.ty.ident.len);
        try self.pushSlice(node.ty.define.lvalue.ty.ident);
    }
}

fn compileLoad(self: *Compiler, node: Node) anyerror!void {
    try self.pushInstruction(.load);
    try self.pushOffset(node.offset);
    if (std.mem.indexOf(u8, self.instructions.items, node.ty.ident)) |index| {
        try self.pushInstruction(.ident_ref);
        try self.pushLen(index); // pushIndex?
        try self.pushLen(node.ty.ident.len);
    } else {
        try self.pushInstruction(.ident);
        try self.pushLen(node.ty.ident.len);
        try self.pushSlice(node.ty.ident);
    }
}

fn compileStore(self: *Compiler, node: Node) anyerror!void {
    try self.compile(node.ty.assign.rvalue.*);

    if (node.ty.assign.lvalue.ty == .ident) {
        try self.pushInstruction(.store);
        try self.pushOffset(node.offset);
        try self.pushEnum(node.ty.assign.combo);

        if (std.mem.indexOf(u8, self.instructions.items, node.ty.assign.lvalue.ty.ident)) |index| {
            try self.pushInstruction(.ident_ref);
            try self.pushLen(index); // pushIndex?
            try self.pushLen(node.ty.assign.lvalue.ty.ident.len);
        } else {
            try self.pushInstruction(.ident);
            try self.pushLen(node.ty.assign.lvalue.ty.ident.len);
            try self.pushSlice(node.ty.assign.lvalue.ty.ident);
        }
    } else {
        try self.compile(node.ty.assign.lvalue.ty.subscript.index.*);
        try self.compile(node.ty.assign.lvalue.ty.subscript.container.*);
        try self.pushInstruction(.set);
        try self.pushOffset(node.offset);
        try self.pushEnum(node.ty.assign.combo);
    }
}

fn compileInfix(self: *Compiler, node: Node) anyerror!void {
    //TODO
    //if (node.ty.infix.op == .kw_and) return self.compileLogicAnd(node);
    //if (node.ty.infix.op == .kw_or) return self.compileLogicOr(node);

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
        else => unreachable,
    }

    try self.pushOffset(node.offset);
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

fn pushLen(self: *Compiler, len: usize) !void {
    try self.instructions.appendSlice(std.mem.sliceAsBytes(&[1]u16{@intCast(u16, len)}));
}

fn pushSlice(self: *Compiler, slice: []const u8) !void {
    try self.instructions.appendSlice(slice);
}

fn pushEnum(self: *Compiler, v: anytype) !void {
    try self.instructions.append(@enumToInt(v));
}

fn pushByte(self: *Compiler, n: anytype) !void {
    try self.instructions.append(@intCast(u8, n));
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
