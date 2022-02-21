const std = @import("std");

const Compiler = @import("Compiler.zig");
const Scope = @import("Scope.zig");
const ScopeStack = @import("ScopeStack.zig");
const Value = @import("Value.zig");
const runtimePrint = @import("fmt.zig").runtimePrint;

allocator: std.mem.Allocator,
last_popped: Value = Value.new(.nil, 0),

instructions: []const u8 = undefined,
ip: *u16 = undefined,
scope: *Scope,

scope_stack: ScopeStack,
frame_stack: std.ArrayList(Frame),
value_stack: std.ArrayList(Value),

const Vm = @This();

pub fn init(
    allocator: std.mem.Allocator,
    instructions: []const u8,
    scope_stack: ScopeStack,
) !Vm {
    var self = Vm{
        .allocator = allocator,
        .frame_stack = std.ArrayList(Frame).init(allocator),
        .scope_stack = scope_stack,
        .scope = scope_stack.head(),
        .value_stack = std.ArrayList(Value).init(allocator),
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
                self.last_popped = self.value_stack.pop();
                self.ip.* += 1;
            },
            // Scope
            .scope_in => try self.evalScopeIn(),
            .scope_out => try self.evalScopeOut(),
            // Predefined constant values
            .bool_false => {
                try self.value_stack.append(Value.new(.{ .boolean = false }, self.getU16()));
                self.ip.* += 3;
            },
            .bool_true => {
                try self.value_stack.append(Value.new(.{ .boolean = true }, self.getU16()));
                self.ip.* += 3;
            },
            .nil => {
                try self.value_stack.append(Value.new(.nil, self.getU16()));
                self.ip.* += 3;
            },
            // Numbers
            .float => {
                const f = std.mem.bytesAsSlice(f64, self.instructions[self.ip.* + 3 .. self.ip.* + 11])[0];
                try self.value_stack.append(Value.new(.{ .float = f }, self.getU16()));
                self.ip.* += 11;
            },
            .int => {
                const i = std.mem.bytesAsSlice(i64, self.instructions[self.ip.* + 3 .. self.ip.* + 11])[0];
                try self.value_stack.append(Value.new(.{ .int = i }, self.getU16()));
                self.ip.* += 11;
            },
            .uint => {
                const u = std.mem.bytesAsSlice(u64, self.instructions[self.ip.* + 3 .. self.ip.* + 11])[0];
                try self.value_stack.append(Value.new(.{ .uint = u }, self.getU16()));
                self.ip.* += 11;
            },
            // Strings
            .format => {
                const len = self.getU16();
                const spec = self.instructions[self.ip.* + 3 .. self.ip.* + 3 + len];

                const value = self.value_stack.pop();
                var buf = std.ArrayList(u8).init(self.allocator);
                var writer = buf.writer();
                try runtimePrint(
                    self.allocator,
                    "change_filename",
                    "change_src",
                    spec,
                    value,
                    writer,
                    value.offset,
                );
                try self.value_stack.append(Value.new(.{ .string = buf.items }, value.offset));

                self.ip.* += 3 + len;
            },
            .plain => {
                const len = self.getU16();
                const s = self.instructions[self.ip.* + 3 .. self.ip.* + 3 + len];
                try self.value_stack.append(Value.new(.{ .string = s }, 0));
                self.ip.* += 3 + len;
            },
            .string => {
                const offset = self.getU16();
                const len = std.mem.bytesAsSlice(u16, self.instructions[self.ip.* + 3 .. self.ip.* + 5])[0];

                var buf = std.ArrayList(u8).init(self.allocator);
                var writer = buf.writer();
                var i: usize = 0;
                while (i < len) : (i += 1) _ = try writer.print("{}", .{self.value_stack.pop()});
                try self.value_stack.append(Value.new(.{ .string = buf.items }, offset));

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

// Scopes
fn evalScopeIn(self: *Vm) anyerror!void {
    const child_scope_type = @intToEnum(Scope.Type, self.instructions[self.ip.* + 1]);
    try self.pushScope(Scope.init(self.allocator, child_scope_type));
    self.ip.* += 2;
}

fn evalScopeOut(self: *Vm) anyerror!void {
    const scope_type = @intToEnum(Scope.Type, self.instructions[self.ip.* + 1]);

    if (scope_type == .loop) {
        while (true) {
            var old_scope = self.popScope();
            old_scope.deinit();
            if (old_scope.ty == .loop) break;
        }
    } else {
        var old_scope = self.popScope();
        old_scope.deinit();
    }

    self.ip.* += 2;
}

fn pushScope(self: *Vm, scope: Scope) !void {
    self.scope = try self.scope_stack.push(scope);
}

fn popScope(self: *Vm) Scope {
    std.debug.assert(self.scope_stack.stack.items.len > 1);
    var old_scope = self.scope_stack.pop();
    self.scope = self.scope_stack.head();
    return old_scope;
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

    var scope_stack = ScopeStack.init(allocator);
    _ = try scope_stack.push(Scope.init(allocator, .block));

    var vm = try init(allocator, compiler.instructions.items, scope_stack);
    try vm.run();

    try std.testing.expectEqual(@as(usize, 0), vm.value_stack.items.len);
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
}

test "Vm strings" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "\"foobar\"");
    try std.testing.expectEqual(Value.Tag.string, got.ty);
    try std.testing.expectEqualStrings("foobar", got.ty.string);
    try std.testing.expectEqual(@as(u16, 0), got.offset);

    got = try testVmValue(allocator,
        \\"foo {#d:0>3# 2} bar"
    );
    try std.testing.expectEqual(Value.Tag.string, got.ty);
    try std.testing.expectEqualStrings("foo 002 bar", got.ty.string);
    try std.testing.expectEqual(@as(u16, 0), got.offset);
}
