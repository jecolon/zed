const std = @import("std");

const Bytecode = @import("Bytecode.zig");
const Location = @import("Location.zig");
const Value = @import("Value.zig");

const Frame = struct {
    instructions: []const u8,
    ip: u16 = 0,
};

call_stack: std.ArrayList(Frame),
constants: []const Value,
filename: []const u8,
instructions: *[]const u8,
ip: *u16,
last_popped: ?Value = null,
src: []const u8,
value_stack: std.ArrayList(Value),

const Vm = @This();

pub fn init(
    allocator: std.mem.Allocator,
    filename: []const u8,
    src: []const u8,
    constants: []const Value,
    insts: []const u8,
) !Vm {
    var self = Vm{
        .call_stack = std.ArrayList(Frame).init(allocator),
        .constants = constants,
        .filename = filename,
        .instructions = undefined,
        .ip = undefined,
        .src = src,
        .value_stack = std.ArrayList(Value).init(allocator),
    };

    try self.call_stack.append(.{ .instructions = insts });
    self.instructions = &self.call_stack.items[0].instructions;
    self.ip = &self.call_stack.items[0].ip;

    return self;
}

pub fn run(self: *Vm) !void {
    while (self.ip.* < self.instructions.len) {
        const opcode = @intToEnum(Bytecode.Opcode, self.instructions.*[self.ip.*]);

        switch (opcode) {
            // Instruction pointer movements.
            .jump => self.jump(),
            .jump_false => {
                const condition = self.value_stack.pop();
                if (!isTruthy(condition)) self.jump() else self.ip.* += 3;
            },
            .jump_true => {
                const condition = self.value_stack.pop();
                if (isTruthy(condition)) self.jump() else self.ip.* += 3;
            },
            // Stack clean up.
            .pop => {
                self.last_popped = self.value_stack.pop();
                self.ip.* += 1;
            },

            // Normal opcodes.
            .constant => {
                const index = std.mem.bytesAsSlice(u16, self.instructions.*[self.ip.* + 1 .. self.ip.* + 3])[0];
                try self.value_stack.append(self.constants[index]);
                self.ip.* += 3;
            },

            .logic_and, .logic_or => {
                const right = self.value_stack.pop();
                const left = self.value_stack.pop();

                if (left.ty != .boolean) {
                    const location = Location.getLocation(self.filename, self.src, left.offset);
                    std.log.err("Logical op on {s}; {}", .{ @tagName(left.ty), location });
                    return error.InvalidLogicOp;
                }
                if (right.ty != .boolean) {
                    const location = Location.getLocation(self.filename, self.src, right.offset);
                    std.log.err("Logical op on {s}; {}", .{ @tagName(right.ty), location });
                    return error.InvalidLogicOp;
                }

                switch (opcode) {
                    .logic_and => try self.value_stack.append(Value.new(.{ .boolean = left.ty.boolean and right.ty.boolean }, left.offset)),
                    .logic_or => try self.value_stack.append(Value.new(.{ .boolean = left.ty.boolean or right.ty.boolean }, left.offset)),
                    else => unreachable,
                }

                self.ip.* += 1;
            },

            .logic_not => {
                const value = self.value_stack.pop();

                if (value.ty != .boolean) {
                    const location = Location.getLocation(self.filename, self.src, value.offset);
                    std.log.err("Logical not op `!` not allowed on {s}; {}", .{ @tagName(value.ty), location });
                    return error.InvalidLogicNot;
                }

                try self.value_stack.append(Value.new(.{ .boolean = !value.ty.boolean }, value.offset));
                self.ip.* += 1;
            },

            .negative => {
                const value = self.value_stack.pop();

                switch (value.ty) {
                    .float => |f| try self.value_stack.append(Value.new(.{ .float = -f }, value.offset)),
                    .int => |i| try self.value_stack.append(Value.new(.{ .int = -i }, value.offset)),
                    else => {
                        const location = Location.getLocation(self.filename, self.src, value.offset);
                        std.log.err("Negative op `-` not allowed on {s}; {}", .{ @tagName(value.ty), location });
                        return error.InvalidNegative;
                    },
                }

                self.ip.* += 1;
            },

            .add => {
                const right = self.value_stack.pop();
                const left = self.value_stack.pop();
                if (left.add(right)) |sum| {
                    try self.value_stack.append(sum);
                } else |err| {
                    const location = Location.getLocation(self.filename, self.src, left.offset);
                    std.log.err("Unable to add {s} and {s}; {}", .{ @tagName(left.ty), @tagName(right.ty), location });
                    return err;
                }

                self.ip.* += 1;
            },
            .sub => {
                const right = self.value_stack.pop();
                const left = self.value_stack.pop();
                if (left.sub(right)) |diff| {
                    try self.value_stack.append(diff);
                } else |err| {
                    const location = Location.getLocation(self.filename, self.src, left.offset);
                    std.log.err("Unable to subtract {s} from {s}; {}", .{ @tagName(right.ty), @tagName(left.ty), location });
                    return err;
                }

                self.ip.* += 1;
            },
            .mul => {
                const right = self.value_stack.pop();
                const left = self.value_stack.pop();
                if (left.mul(right)) |product| {
                    try self.value_stack.append(product);
                } else |err| {
                    const location = Location.getLocation(self.filename, self.src, left.offset);
                    std.log.err("Unable to multiply {s} and {s}; {}", .{ @tagName(left.ty), @tagName(right.ty), location });
                    return err;
                }

                self.ip.* += 1;
            },
            .div => {
                const right = self.value_stack.pop();
                const left = self.value_stack.pop();
                if (left.div(right)) |quotient| {
                    try self.value_stack.append(quotient);
                } else |err| {
                    const location = Location.getLocation(self.filename, self.src, left.offset);
                    std.log.err("Unable to divide {s} by {s}; {}", .{ @tagName(left.ty), @tagName(right.ty), location });
                    return err;
                }

                self.ip.* += 1;
            },
            .mod => {
                const right = self.value_stack.pop();
                const left = self.value_stack.pop();
                if (left.mod(right)) |remainder| {
                    try self.value_stack.append(remainder);
                } else |err| {
                    const location = Location.getLocation(self.filename, self.src, left.offset);
                    std.log.err("Unable to get remainder of {s} by {s}; {}", .{ @tagName(left.ty), @tagName(right.ty), location });
                    return err;
                }

                self.ip.* += 1;
            },

            .eq, .neq => {
                const right = self.value_stack.pop();
                const left = self.value_stack.pop();
                var comparison = left.eql(right);
                if (opcode == .neq) comparison = !comparison;
                try self.value_stack.append(Value.new(.{ .boolean = comparison }, left.offset));

                self.ip.* += 1;
            },

            .lt,
            .lte,
            .gt,
            .gte,
            => {
                const right = self.value_stack.pop();
                const left = self.value_stack.pop();
                const comparison = left.cmp(right) catch |err| {
                    const location = Location.getLocation(self.filename, self.src, left.offset);
                    std.log.err("Unable to compare {s} with {s}; {}", .{ @tagName(left.ty), @tagName(right.ty), location });
                    return err;
                };

                const result = switch (opcode) {
                    .lt => Value.new(.{ .boolean = comparison == .lt }, left.offset),
                    .lte => Value.new(.{ .boolean = comparison == .lt or comparison == .eq }, left.offset),
                    .gt => Value.new(.{ .boolean = comparison == .gt }, left.offset),
                    .gte => Value.new(.{ .boolean = comparison == .gt or comparison == .eq }, left.offset),

                    else => unreachable,
                };

                try self.value_stack.append(result);

                self.ip.* += 1;
            },
        }
    }
}

// Helpers
fn isTruthy(value: Value) bool {
    return switch (value.ty) {
        .boolean => |b| b,
        .float => |f| f != 0.0,
        .int => |i| i != 0,
        .string => |s| s.len != 0,
        .uint => |u| u != 0,

        else => false,
    };
}

fn jump(self: *Vm) void {
    const index = std.mem.bytesAsSlice(u16, self.instructions.*[self.ip.* + 1 .. self.ip.* + 3])[0];
    self.ip.* = index;
}

// Tests

fn testLastValue(input: []const u8, expected: Value) !void {
    const Lexer = @import("Lexer.zig");
    const Parser = @import("Parser.zig");
    const Compiler = @import("Compiler.zig");

    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
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
    try compiler.compileProgram(program);
    var vm = try init(
        arena.allocator(),
        "inline",
        input,
        compiler.constants.items,
        compiler.instructions.items,
    );
    try vm.run();

    const last_popped = vm.last_popped.?;
    try std.testing.expectEqual(@as(usize, 0), vm.value_stack.items.len);

    switch (expected.ty) {
        .boolean => |b| try std.testing.expectEqual(b, last_popped.ty.boolean),
        .float => |f| try std.testing.expectEqual(f, last_popped.ty.float),
        .int => |i| try std.testing.expectEqual(i, last_popped.ty.int),
        .nil => try std.testing.expectEqual(Value.Type.nil, last_popped.ty),
        .string => |s| try std.testing.expectEqualStrings(s, last_popped.ty.string),
        .uint => |u| try std.testing.expectEqual(u, last_popped.ty.uint),
    }
}

test "Vm eval booleans" {
    try testLastValue("false true", Value.new(.{ .boolean = true }, 6));
}

test "Vm eval uint" {
    try testLastValue("123", Value.new(.{ .uint = 123 }, 0));
    try testLastValue("0b00000011", Value.new(.{ .uint = 3 }, 0));
}

test "Vm eval int" {
    try testLastValue("-123", Value.new(.{ .int = -123 }, 0));
    try testLastValue("-0b00000011", Value.new(.{ .int = -3 }, 0));
}

test "Vm eval float" {
    try testLastValue("1.23", Value.new(.{ .float = 1.23 }, 0));
    try testLastValue("-1.23", Value.new(.{ .float = -1.23 }, 0));
}

test "Vm eval string" {
    try testLastValue("\"Hello World!\"", Value.new(.{ .string = "Hello World!" }, 0));
}

test "Vm eval prefix" {
    try testLastValue("!true", Value.new(.{ .boolean = false }, 0));
    try testLastValue("!false", Value.new(.{ .boolean = true }, 0));
    //TODO: Negative op -
}

test "Vm eval infix add" {
    try testLastValue("1 + 1", Value.new(.{ .uint = 2 }, 0));
    try testLastValue("1 + -1", Value.new(.{ .int = 0 }, 0));
    try testLastValue("1 + 1.0", Value.new(.{ .float = 2 }, 0));
    try testLastValue("1 + \"1\"", Value.new(.{ .uint = 2 }, 0));
    try testLastValue("-1 + -1", Value.new(.{ .int = -2 }, 0));
    try testLastValue("-1 + 1", Value.new(.{ .int = 0 }, 0));
    try testLastValue("-1 + 1.0", Value.new(.{ .float = 0 }, 0));
    try testLastValue("-1 + \"1\"", Value.new(.{ .int = 0 }, 0));
    try testLastValue("1.0 + 1", Value.new(.{ .float = 2 }, 0));
    try testLastValue("1.0 + -1", Value.new(.{ .float = 0 }, 0));
    try testLastValue("1.0 + 1.0", Value.new(.{ .float = 2 }, 0));
    try testLastValue("1.0 + \"1\"", Value.new(.{ .float = 2 }, 0));
}

test "Vm eval infix subtract" {
    try testLastValue("1 - 1", Value.new(.{ .uint = 0 }, 0));
    try testLastValue("1 - -1", Value.new(.{ .int = 2 }, 0));
    try testLastValue("1 - 1.0", Value.new(.{ .float = 0 }, 0));
    try testLastValue("1 - \"1\"", Value.new(.{ .uint = 0 }, 0));
    try testLastValue("-1 - -1", Value.new(.{ .int = 0 }, 0));
    try testLastValue("-1 - 1", Value.new(.{ .int = -2 }, 0));
    try testLastValue("-1 - 1.0", Value.new(.{ .float = -2 }, 0));
    try testLastValue("-1 - \"1\"", Value.new(.{ .int = -2 }, 0));
    try testLastValue("1.0 - 1", Value.new(.{ .float = 0 }, 0));
    try testLastValue("1.0 - -1", Value.new(.{ .float = 2 }, 0));
    try testLastValue("1.0 - 1.0", Value.new(.{ .float = 0 }, 0));
    try testLastValue("1.0 - \"1\"", Value.new(.{ .float = 0 }, 0));
}

test "Vm eval infix multiply" {
    try testLastValue("1 * 1", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("1 * -1", Value.new(.{ .int = -1 }, 0));
    try testLastValue("1 * 1.0", Value.new(.{ .float = 1 }, 0));
    try testLastValue("1 * \"1\"", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("-1 * -1", Value.new(.{ .int = 1 }, 0));
    try testLastValue("-1 * 1", Value.new(.{ .int = -1 }, 0));
    try testLastValue("-1 * 1.0", Value.new(.{ .float = -1 }, 0));
    try testLastValue("-1 * \"1\"", Value.new(.{ .int = -1 }, 0));
    try testLastValue("1.0 * 1", Value.new(.{ .float = 1 }, 0));
    try testLastValue("1.0 * -1", Value.new(.{ .float = -1 }, 0));
    try testLastValue("1.0 * 1.0", Value.new(.{ .float = 1 }, 0));
    try testLastValue("1.0 * \"1\"", Value.new(.{ .float = 1 }, 0));
}

test "Vm eval infix divide" {
    try testLastValue("1 / 1", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("1 / -1", Value.new(.{ .int = -1 }, 0));
    try testLastValue("1 / 1.0", Value.new(.{ .float = 1 }, 0));
    try testLastValue("1 / \"1\"", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("-1 / -1", Value.new(.{ .int = 1 }, 0));
    try testLastValue("-1 / 1", Value.new(.{ .int = -1 }, 0));
    try testLastValue("-1 / 1.0", Value.new(.{ .float = -1 }, 0));
    try testLastValue("-1 / \"1\"", Value.new(.{ .int = -1 }, 0));
    try testLastValue("1.0 / 1", Value.new(.{ .float = 1 }, 0));
    try testLastValue("1.0 / -1", Value.new(.{ .float = -1 }, 0));
    try testLastValue("1.0 / 1.0", Value.new(.{ .float = 1 }, 0));
    try testLastValue("1.0 / \"1\"", Value.new(.{ .float = 1 }, 0));
}

test "Vm eval infix modulo" {
    try testLastValue("1 % 1", Value.new(.{ .uint = 0 }, 0));
    try testLastValue("1 % 1.0", Value.new(.{ .float = 0 }, 0));
    try testLastValue("1 % \"1\"", Value.new(.{ .uint = 0 }, 0));
    try testLastValue("-1 % 1", Value.new(.{ .int = 0 }, 0));
    try testLastValue("-1 % 1.0", Value.new(.{ .float = 0 }, 0));
    try testLastValue("-1 % \"1\"", Value.new(.{ .int = 0 }, 0));
    try testLastValue("1.0 % 1", Value.new(.{ .float = 0 }, 0));
    try testLastValue("1.0 % 1.0", Value.new(.{ .float = 0 }, 0));
    try testLastValue("1.0 % \"1\"", Value.new(.{ .float = 0 }, 0));
}

test "Vm eval infix comparisons" {
    try testLastValue("1 == 1", Value.new(.{ .boolean = true }, 0));
    try testLastValue("1 != 1", Value.new(.{ .boolean = false }, 0));
    try testLastValue("1 < 1", Value.new(.{ .boolean = false }, 0));
    try testLastValue("1 <= 1", Value.new(.{ .boolean = true }, 0));
    try testLastValue("1 > 1", Value.new(.{ .boolean = false }, 0));
    try testLastValue("1 >= 1", Value.new(.{ .boolean = true }, 0));
}

test "Vm eval infix mix" {
    try testLastValue("1 + 2 * 5 % 2 / 1 * \"1.0\" == 1", Value.new(.{ .boolean = true }, 0));
}

test "Vm eval infix logic and / or" {
    try testLastValue("true and true", Value.new(.{ .boolean = true }, 0));
}

test "Vm if conditional" {
    try testLastValue("if (true) 1 else 2", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("if (false) 1 else 2", Value.new(.{ .uint = 2 }, 0));
    try testLastValue("if (false) 1", Value.new(.nil, 0));
    try testLastValue("if (true) { 1 } else { 2 }", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("if (false) { 1 } else { 2 }", Value.new(.{ .uint = 2 }, 0));
    try testLastValue("if (false) { 1 }", Value.new(.nil, 0));
}
