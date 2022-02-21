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
            .pop => try self.execPop(),
            // Scope
            .scope_in => try self.execScopeIn(),
            .scope_out => try self.execScopeOut(),
            // Predefined constant values
            .bool_false => try self.execBoolFalse(),
            .bool_true => try self.execBoolTrue(),
            .nil => try self.execNil(),
            // Numbers
            .float => try self.execFloat(),
            .float_ref => try self.execFloatRef(),
            .int => try self.execInt(),
            .int_ref => try self.execIntRef(),
            .uint => try self.execUint(),
            .uint_ref => try self.execUintRef(),
            // Strings
            .format => try self.execFormat(),
            .plain => try self.execPlain(),
            .plain_ref => try self.execPlainRef(),
            .string => try self.execString(),
            // functions
            .func => try self.execFunc(),
            .func_return => {
                if (self.frame_stack.items.len == 1) {
                    // Return from main.
                    self.last_popped = self.value_stack.pop();
                    self.ip.* += 1;
                    break;
                }

                self.execReturn();
            },
        }
    }
}

// Exec functions
fn execPop(self: *Vm) !void {
    self.last_popped = self.value_stack.pop();
    self.ip.* += 1;
}
fn execBoolFalse(self: *Vm) !void {
    try self.value_stack.append(Value.new(.{ .boolean = false }, self.getOffset()));
    self.ip.* += 3;
}
fn execBoolTrue(self: *Vm) !void {
    try self.value_stack.append(Value.new(.{ .boolean = true }, self.getOffset()));
    self.ip.* += 3;
}
fn execNil(self: *Vm) !void {
    try self.value_stack.append(Value.new(.nil, self.getOffset()));
    self.ip.* += 3;
}

fn execFloat(self: *Vm) !void {
    const f = self.getNumber(f64, self.ip.* + 3, 8);
    try self.value_stack.append(Value.new(.{ .float = f }, self.getOffset()));
    self.ip.* += 11;
}
fn execFloatRef(self: *Vm) !void {
    const start = self.getNumber(u16, self.ip.* + 3, 2);
    const f = self.getNumber(f64, start, 8);
    try self.value_stack.append(Value.new(.{ .float = f }, self.getOffset()));
    self.ip.* += 5;
}
fn execInt(self: *Vm) !void {
    const i = self.getNumber(i64, self.ip.* + 3, 8);
    try self.value_stack.append(Value.new(.{ .int = i }, self.getOffset()));
    self.ip.* += 11;
}
fn execIntRef(self: *Vm) !void {
    const start = self.getNumber(u16, self.ip.* + 3, 2);
    const i = self.getNumber(i64, start, 8);
    try self.value_stack.append(Value.new(.{ .int = i }, self.getOffset()));
    self.ip.* += 5;
}
fn execUint(self: *Vm) !void {
    const u = self.getNumber(u64, self.ip.* + 3, 8);
    try self.value_stack.append(Value.new(.{ .uint = u }, self.getOffset()));
    self.ip.* += 11;
}
fn execUintRef(self: *Vm) !void {
    const start = self.getNumber(u16, self.ip.* + 3, 2);
    const u = self.getNumber(u64, start, 8);
    try self.value_stack.append(Value.new(.{ .uint = u }, self.getOffset()));
    self.ip.* += 5;
}

fn execFormat(self: *Vm) !void {
    const len = self.getU16(self.ip.* + 1);
    const spec = self.getString(self.ip.* + 3, len);

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
}
fn execPlain(self: *Vm) !void {
    const len = self.getU16(self.ip.* + 1);
    const s = self.getString(self.ip.* + 3, len);
    try self.value_stack.append(Value.new(.{ .string = s }, 0));
    self.ip.* += 3 + len;
}
fn execPlainRef(self: *Vm) !void {
    const len = self.getU16(self.ip.* + 1);
    const start = self.getU16(self.ip.* + 3);
    const s = self.getString(start, len);
    try self.value_stack.append(Value.new(.{ .string = s }, 0));
    self.ip.* += 5;
}
fn execString(self: *Vm) !void {
    const offset = self.getOffset();
    const len = self.getU16(self.ip.* + 3);

    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();
    var i: usize = 0;
    while (i < len) : (i += 1) _ = try writer.print("{}", .{self.value_stack.pop()});
    try self.value_stack.append(Value.new(.{ .string = buf.items }, offset));

    self.ip.* += 5;
}

fn execFunc(self: *Vm) !void {
    // Src offset
    const offset = self.getOffset(); //TODO: change to use current ip
    self.ip.* += 3;

    // Function name
    const func_name_len = self.getU16(self.ip.*);
    self.ip.* += 2;

    const func_name: []const u8 = if (func_name_len == 0) "" else self.getString(self.ip.*, func_name_len);
    self.ip.* += func_name_len;

    // function params
    const params_len = self.getU16(self.ip.*);
    self.ip.* += 2;

    var params: [][]const u8 = &[0][]const u8{};
    if (params_len != 0) {
        var params_list = std.ArrayList([]const u8).init(self.allocator);
        var param_index: usize = 0;

        while (param_index < params_len) : (param_index += 1) {
            const param_name_len = self.getU16(self.ip.*);
            self.ip.* += 2;

            try params_list.append(self.getString(self.ip.*, param_name_len));
            self.ip.* += param_name_len;
        }

        params = params_list.items;
    }

    // Function instructions
    const instructions_len = self.getU16(self.ip.*);
    self.ip.* += 2;
    const func_instructions: []const u8 = if (instructions_len == 0) "" else self.getString(self.ip.*, instructions_len);
    self.ip.* += instructions_len;

    try self.value_stack.append(Value.new(.{ .func = .{
        .name = func_name,
        .params = params,
        .instructions = func_instructions,
    } }, offset));
}

fn execReturn(self: *Vm) void {
    self.popFrame();
    // Unwind scopes up to the function's scope.
    while (true) if (self.popScope().ty == .function) break;
    self.ip.* += 1;
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
fn execScopeIn(self: *Vm) anyerror!void {
    const child_scope_type = @intToEnum(Scope.Type, self.instructions[self.ip.* + 1]);
    try self.pushScope(Scope.init(self.allocator, child_scope_type));
    self.ip.* += 2;
}

fn execScopeOut(self: *Vm) anyerror!void {
    const scope_type = @intToEnum(Scope.Type, self.instructions[self.ip.* + 1]);

    if (scope_type == .loop) {
        while (true) if (self.popScope().ty == .loop) break;
    } else {
        _ = self.popScope();
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
fn getOffset(self: Vm) u16 {
    return self.getU16(self.ip.* + 1);
}

fn getNumber(self: Vm, comptime T: type, start: usize, n: usize) T {
    return std.mem.bytesAsSlice(T, self.instructions[start .. start + n])[0];
}

fn getString(self: Vm, start: usize, len: usize) []const u8 {
    return self.instructions[start .. start + len];
}

fn getU16(self: Vm, start: usize) u16 {
    return self.getNumber(u16, start, 2);
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

    got = try testVmValue(allocator, "3.1415 3.1415");
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 3.1415), got.ty.float);
    try std.testing.expectEqual(@as(u16, 7), got.offset);

    got = try testVmValue(allocator, "-3");
    try std.testing.expectEqual(Value.Tag.int, got.ty);
    try std.testing.expectEqual(@as(i64, -3), got.ty.int);
    try std.testing.expectEqual(@as(u16, 0), got.offset);

    got = try testVmValue(allocator, "-3 -3");
    try std.testing.expectEqual(Value.Tag.int, got.ty);
    try std.testing.expectEqual(@as(i64, -3), got.ty.int);
    try std.testing.expectEqual(@as(u16, 3), got.offset);

    got = try testVmValue(allocator, "9");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 9), got.ty.uint);
    try std.testing.expectEqual(@as(u16, 0), got.offset);

    got = try testVmValue(allocator, "9 9");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 9), got.ty.uint);
    try std.testing.expectEqual(@as(u16, 2), got.offset);
}

test "Vm strings" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "\"foobar\"");
    try std.testing.expectEqual(Value.Tag.string, got.ty);
    try std.testing.expectEqualStrings("foobar", got.ty.string);
    try std.testing.expectEqual(@as(u16, 0), got.offset);

    got = try testVmValue(allocator, "\"foobar\" \"foobar\"");
    try std.testing.expectEqual(Value.Tag.string, got.ty);
    try std.testing.expectEqualStrings("foobar", got.ty.string);
    try std.testing.expectEqual(@as(u16, 9), got.offset);

    got = try testVmValue(allocator,
        \\"foo {#d:0>3# 2} bar"
    );
    try std.testing.expectEqual(Value.Tag.string, got.ty);
    try std.testing.expectEqualStrings("foo 002 bar", got.ty.string);
    try std.testing.expectEqual(@as(u16, 0), got.offset);
}

test "Vm strings" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\{ foo, bar => 1 }
    );
    try std.testing.expectEqual(Value.Tag.func, got.ty);
    try std.testing.expectEqual(@as(usize, 2), got.ty.func.params.len);
    try std.testing.expectEqualStrings("foo", got.ty.func.params[0]);
    try std.testing.expectEqualStrings("bar", got.ty.func.params[1]);
    try std.testing.expectEqual(@as(usize, 12), got.ty.func.instructions.len);
    try std.testing.expectEqual(Compiler.Opcode.uint, @intToEnum(Compiler.Opcode, got.ty.func.instructions[0]));
    try std.testing.expectEqual(@as(u16, 0), got.offset);
}
