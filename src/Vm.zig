const std = @import("std");

const Bytecode = @import("Bytecode.zig");
const Location = @import("Location.zig");
const Scope = @import("Scope.zig");
const Value = @import("Value.zig");

const Frame = struct {
    instructions: []const u8,
    ip: u16 = 0,
    scope: *Scope,
};

allocator: std.mem.Allocator,
call_stack: std.ArrayList(Frame),
constants: []const Value,
filename: []const u8,
global_scope: *Scope,
instructions: *[]const u8,
ip: *u16,
last_popped: Value = Value.new(.nil, 0),
scope: *Scope,
src: []const u8,
value_stack: std.ArrayList(Value),

const Vm = @This();

pub fn init(
    allocator: std.mem.Allocator,
    filename: []const u8,
    src: []const u8,
    constants: []const Value,
    insts: []const u8,
    scope: *Scope,
) !Vm {
    var self = Vm{
        .allocator = allocator,
        .call_stack = std.ArrayList(Frame).init(allocator),
        .constants = constants,
        .filename = filename,
        .global_scope = scope,
        .instructions = undefined,
        .ip = undefined,
        .scope = undefined,
        .src = src,
        .value_stack = std.ArrayList(Value).init(allocator),
    };

    try self.call_stack.append(.{ .instructions = insts, .scope = scope });
    self.instructions = &self.call_stack.items[0].instructions;
    self.ip = &self.call_stack.items[0].ip;
    self.scope = self.call_stack.items[0].scope;

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
            // Scope
            .scope_in => {
                var child_scope_ptr = try self.allocator.create(Scope);
                child_scope_ptr.* = Scope.init(self.allocator, self.scope);
                self.scope = child_scope_ptr;
                self.ip.* += 1;
            },
            .scope_out => {
                // TODO: Try this
                //var child_scope_ptr = self.scope;
                //self.scope = child_scope_ptr.parent.?;
                //child_scope_ptr.deinit();
                //self.allocator.destroy(child_scope_ptr);
                self.scope = self.scope.parent.?;
                self.ip.* += 1;
            },
            .scope_in_loop => {
                var child_scope_ptr = try self.allocator.create(Scope);
                child_scope_ptr.* = Scope.init(self.allocator, self.scope);
                self.scope = child_scope_ptr;
                self.scope.break_point = true;
                self.ip.* += 1;
            },
            .scope_out_loop => {
                // TODO: Try this
                //var child_scope_ptr = self.scope;
                //self.scope = child_scope_ptr.parent.?;
                //child_scope_ptr.deinit();
                //self.allocator.destroy(child_scope_ptr);
                while (true) {
                    const child_scope = self.scope;
                    self.scope = child_scope.parent.?;
                    if (child_scope.break_point) break;
                }

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
                    .uint => |u| try self.value_stack.append(Value.new(.{ .int = -@intCast(isize, u) }, value.offset)),
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

            // Names
            .define => {
                const name = self.value_stack.pop();
                const value = self.value_stack.pop();
                if (self.scope.isDefined(name.ty.string)) {
                    const location = Location.getLocation(self.filename, self.src, name.offset);
                    std.log.err("{s} already defined; {}", .{ name.ty.string, location });
                    return error.NameAlreadyDefined;
                }
                try self.scope.store(name.ty.string, value);
                try self.value_stack.append(value);
                self.ip.* += 1;
            },
            .load => {
                const name = self.value_stack.pop();

                if (self.scope.load(name.ty.string)) |value| {
                    try self.value_stack.append(value);
                } else {
                    const location = Location.getLocation(self.filename, self.src, name.offset);
                    std.log.err("{s} not defined; {}", .{ name.ty.string, location });
                    return error.NameNotDefined;
                }

                self.ip.* += 1;
            },
            .store => {
                const name = self.value_stack.pop();
                const value = self.value_stack.pop();
                if (!self.scope.isDefined(name.ty.string)) {
                    const location = Location.getLocation(self.filename, self.src, name.offset);
                    std.log.err("{s} not defined; {}", .{ name.ty.string, location });
                    return error.NameNotDefined;
                }
                try self.scope.update(name.ty.string, value);
                try self.value_stack.append(value);
                self.ip.* += 1;
            },

            // Functions
            .call => {
                // Get the function.
                const callee = self.value_stack.pop();
                if (callee.ty != .func) {
                    const location = Location.getLocation(self.filename, self.src, callee.offset);
                    std.log.err("Call op on {s}; {}", .{ @tagName(callee.ty), location });
                    return error.InvalidCall;
                }

                // Prepare the child scope.
                var func_scope_ptr = try self.allocator.create(Scope);
                func_scope_ptr.* = Scope.init(self.allocator, self.global_scope);

                // Self-references
                if (callee.ty.func.name.len != 0) try func_scope_ptr.store(callee.ty.func.name, callee);

                // Process args
                self.ip.* += 1;
                const num_args = self.instructions.*[self.ip.*];
                var i: usize = 0;
                while (i < num_args) : (i += 1) {
                    const arg = self.value_stack.pop();
                    if (i == 0) try func_scope_ptr.store("it", arg); // it
                    var buf: [4]u8 = undefined;
                    const auto_arg_name = try std.fmt.bufPrint(&buf, "@{}", .{i});
                    try func_scope_ptr.store(auto_arg_name, arg); // @0, @1, ...
                    if (i < callee.ty.func.params.len) try func_scope_ptr.store(callee.ty.func.params[i], arg);
                }

                // Push the function's frame.
                try self.pushFrame(callee.ty.func.instructions, func_scope_ptr);
            },
            .func_return => {
                if (self.call_stack.items.len == 1) {
                    // Return from main.
                    self.last_popped = self.value_stack.pop();
                    break;
                }

                // Pop the function's frame.
                self.popFrame();
                self.ip.* += 1;
            },

            .list => {
                const num_items = std.mem.bytesAsSlice(u16, self.instructions.*[self.ip.* + 1 .. self.ip.* + 3])[0];
                self.ip.* += 3;

                var list_ptr = try self.allocator.create(std.ArrayList(Value));
                list_ptr.* = std.ArrayList(Value).init(self.allocator);

                if (num_items == 0) {
                    try self.value_stack.append(Value.new(.{ .list = list_ptr }, 0));
                    continue;
                }

                var i: usize = 0;
                while (i < num_items) : (i += 1) try list_ptr.append(self.value_stack.pop());
                try self.value_stack.append(Value.new(.{ .list = list_ptr }, 0));
            },

            .map => {
                const num_entries = std.mem.bytesAsSlice(u16, self.instructions.*[self.ip.* + 1 .. self.ip.* + 3])[0];
                self.ip.* += 3;

                var map_ptr = try self.allocator.create(std.StringHashMap(Value));
                map_ptr.* = std.StringHashMap(Value).init(self.allocator);

                if (num_entries == 0) {
                    try self.value_stack.append(Value.new(.{ .map = map_ptr }, 0));
                    continue;
                }

                try map_ptr.ensureTotalCapacity(num_entries);
                var i: usize = 0;
                while (i < num_entries) : (i += 1) {
                    const value = self.value_stack.pop();
                    const key_val = self.value_stack.pop();
                    if (key_val.ty != .string) {
                        const location = Location.getLocation(self.filename, self.src, key_val.offset);
                        std.log.err("Map key must evaluate to a string; {}", .{location});
                        return error.InvalidMapKey;
                    }

                    try map_ptr.put(key_val.ty.string, value);
                }
                try self.value_stack.append(Value.new(.{ .map = map_ptr }, 0));
            },

            .range => {
                const inclusive = self.instructions.*[self.ip.* + 1] == 1;
                self.ip.* += 2;
                const end = self.value_stack.pop();
                const start = self.value_stack.pop();
                if (start.ty != .uint or end.ty != .uint) {
                    const location = Location.getLocation(self.filename, self.src, start.offset);
                    std.log.err("Range indexes must evaluate to unsigned integers; {}", .{location});
                    return error.InvalidRange;
                }

                const start_uint = start.ty.uint;
                const end_uint = if (inclusive) end.ty.uint + 1 else end.ty.uint;

                try self.value_stack.append(Value.new(.{ .range = [2]usize{ start_uint, end_uint } }, start.offset));
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

fn pushFrame(self: *Vm, instructions: []const u8, scope: *Scope) anyerror!void {
    try self.call_stack.append(.{ .instructions = instructions, .scope = scope });
    self.instructions = &self.call_stack.items[self.call_stack.items.len - 1].instructions;
    self.ip = &self.call_stack.items[self.call_stack.items.len - 1].ip;
    self.scope = self.call_stack.items[self.call_stack.items.len - 1].scope;
}

fn popFrame(self: *Vm) void {
    //TODO: Try this.
    //self.Scope.deinit();
    //self.allocator.destroy(self.scope);
    _ = self.call_stack.pop();
    self.instructions = &self.call_stack.items[self.call_stack.items.len - 1].instructions;
    self.ip = &self.call_stack.items[self.call_stack.items.len - 1].ip;
    self.scope = self.call_stack.items[self.call_stack.items.len - 1].scope;
}

pub fn dump(self: Vm) void {
    std.debug.print("\n*** VM Dump Begin ***\n", .{});
    std.debug.print("- Constants Dump\n", .{});

    for (self.constants) |constant, i| {
        std.debug.print("\t{}: {}\n", .{ i, constant });
    }

    std.debug.print("- Instructions Dump\n", .{});

    var ins_index: usize = 0;
    while (ins_index < self.instructions.len) {
        const ins = self.instructions.*[ins_index];

        if (Bytecode.Opcode.fromInt(ins)) |def| {
            if (def.bytes == 3) {
                const operand = std.mem.bytesAsSlice(u16, self.instructions.*[ins_index + 1 .. ins_index + 3])[0];
                std.debug.print("\t{}: {} -> {}\n", .{ ins_index, def.opcode, operand });
            } else if (def.bytes == 2) {
                std.debug.print("\t{}: {} -> {}\n", .{ ins_index, def.opcode, self.instructions.*[ins_index + 1] });
            } else {
                std.debug.print("\t{}: {}\n", .{ ins_index, def.opcode });
            }

            ins_index += def.bytes;
        } else {
            std.debug.print("\t{}: {}\n", .{ ins_index, ins });
            ins_index += 1;
        }
    }

    std.debug.print("*** VM Dump End ***\n", .{});
}

// Tests
const debug_dumps = false;

fn testLastValue(input: []const u8, expected: Value) !void {
    const Lexer = @import("Lexer.zig");
    const Parser = @import("Parser.zig");
    const Compiler = @import("Compiler.zig");

    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
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

    var scope = Scope.init(allocator, null);
    defer scope.deinit();

    var vm = try init(
        arena.allocator(),
        "inline",
        input,
        compiler.constants.items,
        compiler.instructions.items,
        &scope,
    );
    if (debug_dumps) vm.dump();
    try vm.run();

    const last_popped = vm.last_popped;
    std.testing.expectEqual(@as(usize, 0), vm.value_stack.items.len) catch |err| {
        for (vm.value_stack.items) |value| {
            std.debug.print("\n***{}***\n", .{value.ty});
        }
        return err;
    };

    switch (expected.ty) {
        .boolean => |b| try std.testing.expectEqual(b, last_popped.ty.boolean),
        .float => |f| try std.testing.expectEqual(f, last_popped.ty.float),
        .int => |i| try std.testing.expectEqual(i, last_popped.ty.int),
        .nil => try std.testing.expectEqual(Value.Type.nil, last_popped.ty),
        .string => |s| try std.testing.expectEqualStrings(s, last_popped.ty.string),
        .uint => |u| try std.testing.expectEqual(u, last_popped.ty.uint),

        .func,
        .list,
        .map,
        .range,
        => try std.testing.expect(expected.eql(last_popped)),
    }

    arena.deinit();

    if (debug_dumps) scope.dump();
}

test "Vm booleans" {
    try testLastValue("false true", Value.new(.{ .boolean = true }, 6));
}

test "Vm uint" {
    try testLastValue("123", Value.new(.{ .uint = 123 }, 0));
    try testLastValue("0b00000011", Value.new(.{ .uint = 3 }, 0));
}

test "Vm int" {
    try testLastValue("-123", Value.new(.{ .int = -123 }, 0));
    try testLastValue("-0b00000011", Value.new(.{ .int = -3 }, 0));
}

test "Vm float" {
    try testLastValue("1.23", Value.new(.{ .float = 1.23 }, 0));
    try testLastValue("-1.23", Value.new(.{ .float = -1.23 }, 0));
}

test "Vm string" {
    try testLastValue("\"Hello World!\"", Value.new(.{ .string = "Hello World!" }, 0));
}

test "Vm prefix" {
    try testLastValue("!true", Value.new(.{ .boolean = false }, 0));
    try testLastValue("!false", Value.new(.{ .boolean = true }, 0));
    try testLastValue("foo := 1; -foo", Value.new(.{ .int = -1 }, 0));
}

test "Vm infix add" {
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

test "Vm infix subtract" {
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

test "Vm infix multiply" {
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

test "Vm infix divide" {
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

test "Vm infix modulo" {
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

test "Vm infix comparisons" {
    try testLastValue("1 == 1", Value.new(.{ .boolean = true }, 0));
    try testLastValue("1 != 1", Value.new(.{ .boolean = false }, 0));
    try testLastValue("1 < 1", Value.new(.{ .boolean = false }, 0));
    try testLastValue("1 <= 1", Value.new(.{ .boolean = true }, 0));
    try testLastValue("1 > 1", Value.new(.{ .boolean = false }, 0));
    try testLastValue("1 >= 1", Value.new(.{ .boolean = true }, 0));
}

test "Vm infix mix" {
    try testLastValue("1 + 2 * 5 % 2 / 1 * \"1.0\" == 1", Value.new(.{ .boolean = true }, 0));
}

test "Vm infix logic and / or" {
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

test "Vm name definition, store, and load" {
    try testLastValue("foo := 1", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("foo := 1; foo = foo + 1; foo", Value.new(.{ .uint = 2 }, 0));
}

test "Vm child scopes" {
    const input =
        \\foo := 1
        \\if (2 >= 1.2) {
        \\  bar := 3
        \\  foo = foo * bar
        \\}
        \\foo
    ;
    try testLastValue(input, Value.new(.{ .uint = 3 }, 0));
}

test "Vm while loop" {
    const input =
        \\i := 0
        \\while (i < 3) {
        \\  if (true) { i = i + 1 }
        \\}
        \\i
    ;
    try testLastValue(input, Value.new(.{ .uint = 3 }, 0));
}

test "Vm loop break" {
    const input =
        \\i := 0
        \\iterations := 0
        \\j := 0
        \\
        \\while (i < 9) {
        \\  if (i == 4) break
        \\
        \\  while (j < 9) {
        \\      if (j == 2) break
        \\      j = j + 1
        \\  }
        \\
        \\  i = i + j
        \\  iterations = iterations + 1
        \\}
        \\iterations
    ;
    try testLastValue(input, Value.new(.{ .uint = 2 }, 0));
}

test "Vm loop scope" {
    const input =
        \\i := 0
        \\
        \\while (i < 9) {
        \\  j := 0
        \\  while (j < 4) {
        \\      if (j == 2) break
        \\
        \\      k := 0
        \\      while (k < 2) {
        \\          if (k == 1) break
        \\          k = k + 1
        \\      }
        \\
        \\      j = j + 1
        \\  }
        \\  i = i + 1
        \\}
        \\i
    ;
    try testLastValue(input, Value.new(.{ .uint = 9 }, 0));
}

test "Vm loop continue" {
    const input =
        \\i := 0
        \\total := 0
        \\
        \\while (i < 3) {
        \\  i = i + 1
        \\  if (i == 1) continue
        \\
        \\  j := 0
        \\  while (j < 3) {
        \\      j = j + 1
        \\      if (j == 1) continue
        \\  }
        \\
        \\  total = total + i
        \\}
        \\total
    ;
    try testLastValue(input, Value.new(.{ .uint = 5 }, 0));
}

test "Vm function literal" {
    const instructions = [_]u8{
        @enumToInt(Bytecode.Opcode.constant),
        0,
        0,
        @enumToInt(Bytecode.Opcode.func_return),
    };

    const func = Value.new(.{ .func = .{
        .instructions = &instructions,
        .params = &[_][]const u8{},
    } }, 0);

    try testLastValue("{ 1 }", func);
    try testLastValue("{ return 1 }", func);

    const instructions_2 = [_]u8{
        @enumToInt(Bytecode.Opcode.constant),
        0,
        0,
        @enumToInt(Bytecode.Opcode.func_return),
    };

    const func_2 = Value.new(.{ .func = .{
        .instructions = &instructions_2,
        .params = &[_][]const u8{},
    } }, 0);

    try testLastValue("{ return }", func_2);
    try testLastValue("return", Value.new(.nil, 0));
    try testLastValue("return 1", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("", Value.new(.nil, 0));
}

test "Vm function call" {
    try testLastValue("{ 1 }()", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("{ return 1 + 1 }() + { 1 }()", Value.new(.{ .uint = 3 }, 0));
}

test "Vm function recursion" {
    const input =
        \\fib := {
        \\  if (it < 2) return it
        \\  return fib(it - 1) + fib(it - 2)
        \\}
        \\fib(6)
    ;
    try testLastValue(input, Value.new(.{ .uint = 8 }, 0));
}

test "Vm more complex function" {
    const input =
        \\fib := { =>
        \\  a := 0
        \\  b := @1
        \\  i := 0
        \\
        \\  while (true) {
        \\      if (i >= it) break
        \\
        \\      tmp := a
        \\      a = b
        \\      b = tmp + a
        \\      i = i + 1
        \\  }
        \\
        \\  return a
        \\}
        \\fib(6, 1)
    ;
    try testLastValue(input, Value.new(.{ .uint = 8 }, 0));
}

test "Vm more complex recursion" {
    const input =
        \\wrapper := {
        \\  countdown := { x =>
        \\      if (x == 0) {
        \\          return 0
        \\      } else {
        \\          return countdown(x - 1)
        \\      }
        \\  }
        \\  countdown(1)
        \\}
        \\wrapper()
    ;
    try testLastValue(input, Value.new(.{ .uint = 0 }, 0));
}

test "Vm list literal" {
    const allocator = std.testing.allocator;
    var list_ptr = try allocator.create(std.ArrayList(Value));
    defer allocator.destroy(list_ptr);
    list_ptr.* = try std.ArrayList(Value).initCapacity(allocator, 3);
    defer list_ptr.deinit();
    list_ptr.appendAssumeCapacity(Value.new(.{ .uint = 1 }, 2));
    list_ptr.appendAssumeCapacity(Value.new(.{ .uint = 2 }, 5));
    list_ptr.appendAssumeCapacity(Value.new(.{ .uint = 3 }, 8));
    const list = Value.new(.{ .list = list_ptr }, 0);

    const input =
        \\[ 1, 2, 3 ]
    ;
    try testLastValue(input, list);
}

test "Vm list literal" {
    const allocator = std.testing.allocator;
    var map_ptr = try allocator.create(std.StringHashMap(Value));
    defer allocator.destroy(map_ptr);
    map_ptr.* = std.StringHashMap(Value).init(allocator);
    defer map_ptr.deinit();
    try map_ptr.ensureTotalCapacity(3);
    map_ptr.putAssumeCapacity("a", Value.new(.{ .uint = 1 }, 0));
    map_ptr.putAssumeCapacity("b", Value.new(.{ .uint = 2 }, 0));
    map_ptr.putAssumeCapacity("c", Value.new(.{ .uint = 3 }, 0));
    const map = Value.new(.{ .map = map_ptr }, 0);

    const input =
        \\[ "a": 1, "b": 2, "c": 3 ]
    ;
    try testLastValue(input, map);
}

test "Vm range" {
    try testLastValue("21 ..< 12", Value.new(.{ .range = [2]usize{ 21, 12 } }, 0));
    try testLastValue("21 ..= 12", Value.new(.{ .range = [2]usize{ 21, 13 } }, 0));
}
