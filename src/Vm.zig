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
            .pop => {
                self.last_popped = self.value_stack.pop();
                self.ip.* += 1;
            },

            .constant => {
                const index = std.mem.bytesAsSlice(u16, self.instructions.*[self.ip.* + 1 .. self.ip.* + 3])[0];
                try self.value_stack.append(self.constants[index]);
                self.ip.* += 3;
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
        }
    }
}

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
