const std = @import("std");

const Bytecode = @import("Bytecode.zig");
const Location = @import("Location.zig");
const Node = @import("Node.zig");
const Scope = @import("Scope.zig");
const Value = @import("Value.zig");
const GraphemeIterator = @import("ziglyph").GraphemeIterator;
const runtimePrint = @import("fmt.zig").runtimePrint;

allocator: std.mem.Allocator,
filename: []const u8,
src: []const u8,

constants: []const Value,
last_popped: Value = Value.new(.nil, 0),
output: *std.ArrayList(u8),

// Stacks
call_stack: std.ArrayList(Frame),
scope_stack: std.ArrayList(Scope),
value_stack: std.ArrayList(Value),

// Pointers that change depending on execution context.
instructions: []const u8 = undefined,
ip: *u16 = undefined,
scope: *Scope = undefined,

const Vm = @This();

pub fn init(
    allocator: std.mem.Allocator,
    filename: []const u8,
    src: []const u8,
    constants: []const Value,
    instructions: []const u8,
    scope: Scope,
    output: *std.ArrayList(u8),
) !Vm {
    var self = Vm{
        .allocator = allocator,
        .filename = filename,
        .src = src,

        .constants = constants,
        .instructions = instructions,
        .output = output,

        .call_stack = std.ArrayList(Frame).init(allocator),
        .scope_stack = std.ArrayList(Scope).init(allocator),
        .value_stack = std.ArrayList(Value).init(allocator),
    };

    try self.call_stack.append(.{ .instructions = instructions });
    try self.scope_stack.append(scope); // Global Scope

    self.ip = &self.call_stack.items[0].ip;
    self.scope = &self.scope_stack.items[0];

    return self;
}

pub fn run(self: *Vm) anyerror!void {
    while (self.ip.* < self.instructions.len) {
        const opcode = @intToEnum(Bytecode.Opcode, self.instructions[self.ip.*]);

        switch (opcode) {
            // Instruction pointer movements.
            .jump => self.jump(),
            .jump_false => self.evalJumpFalse(),
            .jump_true => self.evalJumpTrue(),

            // Stack clean up.
            .pop => {
                self.last_popped = self.value_stack.pop();
                self.ip.* += 1;
            },

            // Scope
            .scope_in => try self.evalScopeIn(),
            .scope_out => try self.evalScopeOut(),

            // Load constant
            .constant => try self.evalConstant(),

            // Prefix
            .logic_not => try self.evalLogicNot(),
            .negative => try self.evalNegative(),

            // Arithmetic
            .add => try self.evalAdd(),
            .sub => try self.evalSub(),
            .mul => try self.evalMul(),
            .div => try self.evalDiv(),
            .mod => try self.evalMod(),

            // Comparison ops
            .lt,
            .lte,
            .gt,
            .gte,
            => try self.evalComparison(opcode),
            .eq, .neq => try self.evalEqNeq(opcode),

            // Varibales
            .define => try self.evalDefine(),
            .load => try self.evalLoad(),
            .store => try self.evalStore(),

            // Functions
            .call => try self.evalCall(),
            .func_return => {
                if (self.call_stack.items.len == 1) {
                    // Return from main.
                    self.last_popped = self.value_stack.pop();
                    break;
                }
                self.evalReturn();
            },

            // Data structures
            .list => try self.evalList(),
            .map => try self.evalMap(),
            .range => try self.evalRange(),
            .string => try self.evalString(),

            // Data structure ops
            .subscript => try self.evalSubscript(),
            .set => try self.evalSet(),
            .format => try self.evalFormat(),

            // Record selection
            .rec_range => try self.evalRecRange(),
        }
    }
}

// Helpers
pub fn addBuiltins(scope: *Scope) anyerror!void {
    try scope.store("atan2", Value.new(.{ .builtin = .atan2 }, 0));
    try scope.store("chars", Value.new(.{ .builtin = .chars }, 0));
    try scope.store("contains", Value.new(.{ .builtin = .contains }, 0));
    try scope.store("cos", Value.new(.{ .builtin = .cos }, 0));
    try scope.store("each", Value.new(.{ .builtin = .each }, 0));
    try scope.store("endsWith", Value.new(.{ .builtin = .endsWith }, 0));
    try scope.store("exp", Value.new(.{ .builtin = .exp }, 0));
    try scope.store("filter", Value.new(.{ .builtin = .filter }, 0));
    try scope.store("int", Value.new(.{ .builtin = .int }, 0));
    try scope.store("indexOf", Value.new(.{ .builtin = .indexOf }, 0));
    try scope.store("join", Value.new(.{ .builtin = .join }, 0));
    try scope.store("keys", Value.new(.{ .builtin = .keys }, 0));
    try scope.store("lastIndexOf", Value.new(.{ .builtin = .lastIndexOf }, 0));
    try scope.store("len", Value.new(.{ .builtin = .len }, 0));
    try scope.store("log", Value.new(.{ .builtin = .log }, 0));
    try scope.store("map", Value.new(.{ .builtin = .map }, 0));
    try scope.store("max", Value.new(.{ .builtin = .max }, 0));
    try scope.store("mean", Value.new(.{ .builtin = .mean }, 0));
    try scope.store("median", Value.new(.{ .builtin = .median }, 0));
    try scope.store("min", Value.new(.{ .builtin = .min }, 0));
    try scope.store("mode", Value.new(.{ .builtin = .mode }, 0));
    try scope.store("print", Value.new(.{ .builtin = .print }, 0));
    try scope.store("push", Value.new(.{ .builtin = .push }, 0));
    try scope.store("rand", Value.new(.{ .builtin = .rand }, 0));
    try scope.store("reduce", Value.new(.{ .builtin = .reduce }, 0));
    try scope.store("reverse", Value.new(.{ .builtin = .reverse }, 0));
    try scope.store("sin", Value.new(.{ .builtin = .sin }, 0));
    try scope.store("sort", Value.new(.{ .builtin = .sort }, 0));
    try scope.store("split", Value.new(.{ .builtin = .split }, 0));
    try scope.store("sqrt", Value.new(.{ .builtin = .sqrt }, 0));
    try scope.store("startsWith", Value.new(.{ .builtin = .startsWith }, 0));
    try scope.store("stdev", Value.new(.{ .builtin = .stdev }, 0));
    try scope.store("values", Value.new(.{ .builtin = .values }, 0));
}

// Arithmetic
fn evalAdd(self: *Vm) anyerror!void {
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
}
fn evalSub(self: *Vm) anyerror!void {
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
}
fn evalMul(self: *Vm) anyerror!void {
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
}
fn evalDiv(self: *Vm) anyerror!void {
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
}
fn evalMod(self: *Vm) anyerror!void {
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
}

// Comparison
fn evalComparison(self: *Vm, opcode: Bytecode.Opcode) anyerror!void {
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
}
fn evalEqNeq(self: *Vm, opcode: Bytecode.Opcode) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();

    var comparison = left.eql(right);
    if (opcode == .neq) comparison = !comparison;
    try self.value_stack.append(Value.new(.{ .boolean = comparison }, left.offset));

    self.ip.* += 1;
}

fn evalCall(self: *Vm) anyerror!void {
    // Get the function.
    const callee = self.value_stack.pop();

    if (callee.ty == .builtin) {
        return switch (callee.ty.builtin) {
            .atan2 => try self.atan2(callee.offset),
            .chars => try self.strChars(callee.offset),
            .contains => try self.contains(callee.offset),
            .cos => try self.oneArgMath(callee),
            .each => try self.listEach(callee.offset),
            .endsWith => try self.strEndsWith(callee.offset),
            .exp => try self.oneArgMath(callee),
            .filter => try self.listFilter(callee.offset),
            .join => try self.listJoin(callee.offset),
            .indexOf => try self.indexOf(callee.offset),
            .int => try self.oneArgMath(callee),
            .keys => try self.mapKeys(callee.offset),
            .lastIndexOf => try self.lastIndexOf(callee.offset),
            .len => try self.length(callee.offset),
            .log => try self.oneArgMath(callee),
            .map => try self.listMap(callee.offset),
            .max => try self.listMax(callee.offset),
            .mean => try self.listMean(callee.offset),
            .median => try self.listMedian(callee.offset),
            .min => try self.listMin(callee.offset),
            .mode => try self.listMode(callee.offset),
            .print => try self.print(callee.offset, self.output.writer()),
            .push => try self.listPush(callee.offset),
            .rand => try self.rand(callee.offset),
            .reduce => try self.listReduce(callee.offset),
            .reverse => try self.listReverse(callee.offset),
            .sin => try self.oneArgMath(callee),
            .sort => try self.listSort(callee.offset),
            .split => try self.strSplit(callee.offset),
            .sqrt => try self.oneArgMath(callee),
            .startsWith => try self.strStartsWith(callee.offset),
            .stdev => try self.listStdev(callee.offset),
            .values => try self.mapValues(callee.offset),
        };
    }

    if (callee.ty != .func) {
        const location = Location.getLocation(self.filename, self.src, callee.offset);
        std.log.err("Call op on {s}; {}", .{ @tagName(callee.ty), location });
        return error.InvalidCall;
    }

    // Prepare the child scope.
    var func_scope = Scope.init(self.allocator, .function, &self.scope_stack.items[0]);

    // Self-references
    if (callee.ty.func.name.len != 0) try func_scope.store(callee.ty.func.name, callee);

    // Process args
    const num_args = self.instructions[self.ip.* + 1];
    self.ip.* += 1;

    var i: usize = 0;
    while (i < num_args) : (i += 1) {
        const arg = self.value_stack.pop();
        if (i == 0) try func_scope.store("it", arg); // it
        var buf: [4]u8 = undefined;
        const auto_arg_name = try std.fmt.bufPrint(&buf, "@{}", .{i});
        try func_scope.store(auto_arg_name, arg); // @0, @1, ...
        if (i < callee.ty.func.params.len) try func_scope.store(callee.ty.func.params[i], arg);
    }

    // Push the function's frame.
    try self.pushScope(func_scope);
    try self.pushFrame(callee.ty.func.instructions);
}

fn evalConstant(self: *Vm) anyerror!void {
    const index = std.mem.bytesAsSlice(u16, self.instructions[self.ip.* + 1 .. self.ip.* + 3])[0];
    try self.value_stack.append(self.constants[index]);
    self.ip.* += 3;
}

fn evalDefine(self: *Vm) anyerror!void {
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
}

fn evalFormat(self: *Vm) anyerror!void {
    const spec = self.value_stack.pop();
    if (spec.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, spec.offset);
        std.log.err("Invalid format spec; {}", .{location});
        return error.InvalidFormat;
    }

    const value = self.value_stack.pop();
    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();
    try runtimePrint(
        self.allocator,
        self.filename,
        self.src,
        spec.ty.string,
        value,
        writer,
        spec.offset,
    );
    try self.value_stack.append(Value.new(.{ .string = buf.items }, value.offset));

    self.ip.* += 1;
}

fn evalLoad(self: *Vm) anyerror!void {
    const name = self.value_stack.pop();

    if (self.scope.load(name.ty.string)) |value| {
        try self.value_stack.append(value);
    } else {
        const location = Location.getLocation(self.filename, self.src, name.offset);
        std.log.err("{s} not defined; {}", .{ name.ty.string, location });
        return error.NameNotDefined;
    }

    self.ip.* += 1;
}

fn evalStore(self: *Vm) anyerror!void {
    const name = self.value_stack.pop();
    const rvalue = self.value_stack.pop();
    const combo = @intToEnum(Node.Combo, self.instructions[self.ip.* + 1]);

    if (combo == .none) {
        if (!self.scope.isDefined(name.ty.string)) {
            const location = Location.getLocation(self.filename, self.src, name.offset);
            std.log.err("{s} not defined; {}", .{ name.ty.string, location });
            return error.NameNotDefined;
        }
        try self.scope.update(name.ty.string, rvalue);
        try self.value_stack.append(rvalue);
    } else {
        const old_value = self.scope.load(name.ty.string) orelse {
            const location = Location.getLocation(self.filename, self.src, name.offset);
            std.log.err("{s} not defined; {}", .{ name.ty.string, location });
            return error.NameNotDefined;
        };

        const new_value = switch (combo) {
            .none => unreachable,
            .add => try old_value.add(rvalue),
            .sub => try old_value.sub(rvalue),
            .mul => try old_value.mul(rvalue),
            .div => try old_value.div(rvalue),
            .mod => try old_value.mod(rvalue),
        };

        try self.scope.update(name.ty.string, new_value);
        try self.value_stack.append(new_value);
    }

    self.ip.* += 2;
}

fn evalString(self: *Vm) anyerror!void {
    const len = std.mem.bytesAsSlice(u16, self.instructions[self.ip.* + 1 .. self.ip.* + 3])[0];
    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();
    var i: usize = 0;
    while (i < len) : (i += 1) _ = try writer.print("{}", .{self.value_stack.pop()});
    try self.value_stack.append(Value.new(.{ .string = buf.items }, 0));

    self.ip.* += 3;
}

fn evalJumpFalse(self: *Vm) void {
    const condition = self.value_stack.pop();
    if (!isTruthy(condition)) self.jump() else self.ip.* += 3;
}

fn evalJumpTrue(self: *Vm) void {
    const condition = self.value_stack.pop();
    if (isTruthy(condition)) self.jump() else self.ip.* += 3;
}

fn evalList(self: *Vm) anyerror!void {
    const num_items = std.mem.bytesAsSlice(u16, self.instructions[self.ip.* + 1 .. self.ip.* + 3])[0];

    var list_ptr = try self.allocator.create(std.ArrayList(Value));
    list_ptr.* = std.ArrayList(Value).init(self.allocator);

    if (num_items == 0) {
        try self.value_stack.append(Value.new(.{ .list = list_ptr }, 0));
        return;
    }

    var i: usize = 0;
    while (i < num_items) : (i += 1) {
        const rvalue = self.value_stack.pop();
        const value_copy = try rvalue.copy(self.allocator);
        try list_ptr.append(value_copy);
    }

    try self.value_stack.append(Value.new(.{ .list = list_ptr }, 0));

    self.ip.* += 3;
}

fn evalMapSubscript(self: *Vm, container: Value) anyerror!void {
    const key = self.value_stack.pop();
    if (key.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, key.offset);
        std.log.err("Map subscript cannot be {s}; {}", .{ @tagName(key.ty), location });
        return error.InvalidSubscript;
    }

    if (container.ty.map.get(key.ty.string)) |value| {
        try self.value_stack.append(value);
    } else {
        try self.value_stack.append(Value.new(.nil, 0));
    }
}

fn evalListSubscript(self: *Vm, container: Value) anyerror!void {
    const index = self.value_stack.pop();
    if (index.ty != .uint and index.ty != .range) {
        const location = Location.getLocation(self.filename, self.src, index.offset);
        std.log.err("List subscript cannot be {s}; {}", .{ @tagName(index.ty), location });
        return error.InvalidSubscript;
    }

    if (index.ty == .uint) {
        if (index.ty.uint >= container.ty.list.items.len) {
            const location = Location.getLocation(self.filename, self.src, index.offset);
            std.log.err("Subscript index out of bounds; {}", .{location});
            return error.InvalidSubscript;
        }
        try self.value_stack.append(container.ty.list.items[index.ty.uint]);
    } else {
        if (index.ty.range[1] > container.ty.list.items.len) {
            const location = Location.getLocation(self.filename, self.src, index.offset);
            std.log.err("Range end index out of bounds; {}", .{location});
            return error.InvalidSubscript;
        }

        var new_list_ptr = try self.allocator.create(std.ArrayList(Value));
        new_list_ptr.* = try std.ArrayList(Value).initCapacity(self.allocator, index.ty.range[1] - index.ty.range[0]);
        for (container.ty.list.items[index.ty.range[0]..index.ty.range[1]]) |item|
            new_list_ptr.appendAssumeCapacity(try item.copy(self.allocator));

        try self.value_stack.append(Value.new(.{ .list = new_list_ptr }, 0));
    }
}

fn evalLogicNot(self: *Vm) anyerror!void {
    const value = self.value_stack.pop();

    if (value.ty != .boolean) {
        const location = Location.getLocation(self.filename, self.src, value.offset);
        std.log.err("Logical not op `!` not allowed on {s}; {}", .{ @tagName(value.ty), location });
        return error.InvalidLogicNot;
    }

    try self.value_stack.append(Value.new(.{ .boolean = !value.ty.boolean }, value.offset));

    self.ip.* += 1;
}

fn evalMap(self: *Vm) anyerror!void {
    const num_entries = std.mem.bytesAsSlice(u16, self.instructions[self.ip.* + 1 .. self.ip.* + 3])[0];

    var map_ptr = try self.allocator.create(std.StringHashMap(Value));
    map_ptr.* = std.StringHashMap(Value).init(self.allocator);

    if (num_entries == 0) {
        try self.value_stack.append(Value.new(.{ .map = map_ptr }, 0));
        return;
    }

    try map_ptr.ensureTotalCapacity(num_entries);
    var i: usize = 0;
    while (i < num_entries) : (i += 1) {
        const rvalue = self.value_stack.pop();
        const key_val = self.value_stack.pop();
        if (key_val.ty != .string) {
            const location = Location.getLocation(self.filename, self.src, key_val.offset);
            std.log.err("Map key must evaluate to a string; {}", .{location});
            return error.InvalidMapKey;
        }

        const key_copy = try self.allocator.dupe(u8, key_val.ty.string);
        const value_copy = try rvalue.copy(self.allocator);

        try map_ptr.put(key_copy, value_copy);
    }
    try self.value_stack.append(Value.new(.{ .map = map_ptr }, 0));

    self.ip.* += 3;
}

fn evalNegative(self: *Vm) anyerror!void {
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
}

fn evalRange(self: *Vm) anyerror!void {
    const inclusive = self.instructions[self.ip.* + 1] == 1;
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

    self.ip.* += 2;
}

fn evalRecRange(self: *Vm) anyerror!void {
    const range_id = self.instructions[self.ip.* + 1];
    const exclusive = self.instructions[self.ip.* + 2] == 1;
    const from = self.value_stack.pop();
    const to = self.value_stack.pop();
    const action_instructions = self.value_stack.pop();

    var result = Value.new(.nil, 0);
    var eval_action = false;

    if (self.scope.hasRange(range_id)) {
        // In range
        eval_action = true;

        if (isTruthy(to)) {
            // Range end.
            self.scope.deleteRange(range_id);
            if (exclusive) eval_action = false;
        }
    } else {
        // Not in range
        var start_range = false;

        if (from.ty != .nil) {
            // We have from
            start_range = isTruthy(from);
        } else {
            // No from; start only at row == 1.
            const rnum = self.scope.load("@rnum").?;
            start_range = rnum.ty.uint == 1;
        }

        if (start_range) {
            // We start a new range.
            try self.scope.putRange(range_id);
            eval_action = true;
        }
    }

    if (eval_action) {
        if (action_instructions.ty.string.len > 0) {
            // Set up Sub-VM arena.
            var vm_arena = std.heap.ArenaAllocator.init(self.allocator);
            defer vm_arena.deinit();
            const vm_allocator = vm_arena.allocator();

            var vm = try init(
                vm_allocator,
                self.filename,
                self.src,
                self.constants,
                action_instructions.ty.string,
                Scope.init(vm_allocator, .function, self.scope),
                self.output,
            );
            try vm.run();

            result = vm.last_popped;
        } else {
            // Default action
            const rec = self.scope.load("@rec").?;
            var writer = self.output.writer();
            _ = try writer.print("{}", .{rec});
        }
    }

    try self.value_stack.append(result);

    self.ip.* += 3;
}

fn evalReturn(self: *Vm) void {
    // Pop the function's frame.
    self.popFrame();
    // Unwind scopes up to the function's scope.
    while (true) {
        var old_scope = self.popScope();
        old_scope.deinit();
        if (old_scope.ty == .function) break;
    }

    self.ip.* += 1;
}

fn evalScopeIn(self: *Vm) anyerror!void {
    const child_scope_type = @intToEnum(Scope.Type, self.instructions[self.ip.* + 1]);
    var child_scope = Scope.init(self.allocator, child_scope_type, self.scope);
    try self.pushScope(child_scope);
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

fn evalSet(self: *Vm) anyerror!void {
    const container = self.value_stack.pop();
    if (container.ty != .list and container.ty != .map) {
        const location = Location.getLocation(self.filename, self.src, container.offset);
        std.log.err("Subscript assign not allowed on {s}; {}", .{ @tagName(container.ty), location });
        return error.InvalidSet;
    }

    switch (container.ty) {
        .list => try self.evalListSet(container),
        .map => try self.evalMapSet(container),
        else => unreachable,
    }

    self.ip.* += 2;
}
fn evalListSet(self: *Vm, container: Value) anyerror!void {
    const index = self.value_stack.pop();
    if (index.ty != .uint) {
        const location = Location.getLocation(self.filename, self.src, index.offset);
        std.log.err("List subscript assign index cannot be {s}; {}", .{ @tagName(index.ty), location });
        return error.InvalidSet;
    }

    if (index.ty.uint >= container.ty.list.items.len) {
        const location = Location.getLocation(self.filename, self.src, index.offset);
        std.log.err("Subscript assign index out of bounds; {}", .{location});
        return error.InvalidSet;
    }

    const combo = @intToEnum(Node.Combo, self.instructions[self.ip.* + 1]);

    if (combo == .none) {
        // Free old value.
        container.ty.list.items[index.ty.uint].deinit(container.ty.list.allocator);

        const rvalue = self.value_stack.pop();
        container.ty.list.items[index.ty.uint] = try rvalue.copy(container.ty.list.allocator);
        try self.value_stack.append(rvalue);
    } else {
        const old_value = container.ty.list.items[index.ty.uint];
        const rvalue = self.value_stack.pop();
        const new_value = switch (combo) {
            .none => unreachable,
            .add => try old_value.add(rvalue),
            .sub => try old_value.sub(rvalue),
            .mul => try old_value.mul(rvalue),
            .div => try old_value.div(rvalue),
            .mod => try old_value.mod(rvalue),
        };

        // Free old value.
        container.ty.list.items[index.ty.uint].deinit(container.ty.list.allocator);

        container.ty.list.items[index.ty.uint] = try new_value.copy(container.ty.list.allocator);
        try self.value_stack.append(new_value);
    }
}
fn evalMapSet(self: *Vm, container: Value) anyerror!void {
    const key = self.value_stack.pop();
    if (key.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, key.offset);
        std.log.err("Map subscript assign key cannot be {s}; {}", .{ @tagName(key.ty), location });
        return error.InvalidSet;
    }

    const combo = @intToEnum(Node.Combo, self.instructions[self.ip.* + 1]);
    const rvalue = self.value_stack.pop();
    const key_copy = try container.ty.map.allocator.dupe(u8, key.ty.string);

    if (combo == .none) {
        if (container.ty.map.fetchRemove(key.ty.string)) |old_kv| {
            // Free old entry
            container.ty.map.allocator.free(old_kv.key);
            old_kv.value.deinit(container.ty.map.allocator);
        }

        const value_copy = try rvalue.copy(container.ty.map.allocator);
        try container.ty.map.put(key_copy, value_copy);
        try self.value_stack.append(rvalue);
    } else {
        const old_kv = container.ty.map.fetchRemove(key.ty.string) orelse {
            //TODO: Handle set of new entry.
            const location = Location.getLocation(self.filename, self.src, key.offset);
            std.log.err("{s} not found in map; {}", .{ key.ty.string, location });
            return error.KeyNotFound;
        };

        const new_value = switch (combo) {
            .none => unreachable,
            .add => try old_kv.value.add(rvalue),
            .sub => try old_kv.value.sub(rvalue),
            .mul => try old_kv.value.mul(rvalue),
            .div => try old_kv.value.div(rvalue),
            .mod => try old_kv.value.mod(rvalue),
        };

        // Free old entry
        container.ty.map.allocator.free(old_kv.key);
        old_kv.value.deinit(container.ty.map.allocator);

        const value_copy = try new_value.copy(container.ty.map.allocator);
        try container.ty.map.put(key_copy, value_copy);
        try self.value_stack.append(new_value);
    }
}

fn evalSubscript(self: *Vm) anyerror!void {
    const container = self.value_stack.pop();
    if (container.ty != .list and container.ty != .map) {
        const location = Location.getLocation(self.filename, self.src, container.offset);
        std.log.err("Subscript op not allowed on {s}; {}", .{ @tagName(container.ty), location });
        return error.InvalidSubscript;
    }

    switch (container.ty) {
        .list => try self.evalListSubscript(container),
        .map => try self.evalMapSubscript(container),
        else => unreachable,
    }

    self.ip.* += 1;
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
    const index = std.mem.bytesAsSlice(u16, self.instructions[self.ip.* + 1 .. self.ip.* + 3])[0];
    self.ip.* = index;
}

const Frame = struct {
    instructions: []const u8,
    ip: u16 = 0,
};

fn pushFrame(self: *Vm, instructions: []const u8) anyerror!void {
    try self.call_stack.append(.{ .instructions = instructions });
    self.instructions = instructions;
    self.ip = &self.call_stack.items[self.call_stack.items.len - 1].ip;
}

fn popFrame(self: *Vm) void {
    _ = self.call_stack.pop();
    self.instructions = self.call_stack.items[self.call_stack.items.len - 1].instructions;
    self.ip = &self.call_stack.items[self.call_stack.items.len - 1].ip;
}

fn pushScope(self: *Vm, scope: Scope) anyerror!void {
    try self.scope_stack.append(scope);
    self.scope = &self.scope_stack.items[self.scope_stack.items.len - 1];
}

fn popScope(self: *Vm) Scope {
    std.debug.assert(self.scope_stack.items.len > 1);
    self.scope = &self.scope_stack.items[self.scope_stack.items.len - 2];
    return self.scope_stack.pop();
}

// Builtins
fn atan2(self: *Vm, offset: u16) anyerror!void {
    // Get args count.
    self.ip.* += 1;
    const num_args = self.instructions[self.ip.*];
    if (num_args != 2) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("atan2 requires 2 arguments.; {}", .{location});
        return error.InvalidAtan2;
    }
    const y_val = self.value_stack.pop();
    const y = y_val.asFloat() orelse {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("atan2 y not convertible to float; {}", .{location});
        return error.InvalidAtan2;
    };

    const x_val = self.value_stack.pop();
    const x = x_val.asFloat() orelse {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("atan2 x not convertible to float; {}", .{location});
        return error.InvalidAtan2;
    };

    const result = Value.new(.{ .float = std.math.atan2(f64, y.ty.float, x.ty.float) }, offset);
    try self.value_stack.append(result);

    self.ip.* += 1;
}
fn strChars(self: *Vm, offset: u16) anyerror!void {
    const str = self.value_stack.pop();
    if (str.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("chars method on {s}; {}", .{ @tagName(str.ty), location });
        return error.InvalidCharsCall;
    }

    var list_ptr = try self.allocator.create(std.ArrayList(Value));
    list_ptr.* = std.ArrayList(Value).init(self.allocator);

    var giter = GraphemeIterator.init(str.ty.string) catch {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("Invalid UTF-8; {}", .{location});
        return error.InvalidString;
    };
    while (giter.next()) |grapheme| try list_ptr.append(Value.new(.{ .string = grapheme.bytes }, offset));

    try self.value_stack.append(Value.new(.{ .list = list_ptr }, offset));
    self.ip.* += 2;
}
fn print(self: *Vm, offset: u16, writer: anytype) anyerror!void {
    // Get args count.
    self.ip.* += 1;
    const num_args = self.instructions[self.ip.*];

    var i: usize = 0;
    const ofs = if (self.scope.load("@ofs")) |s| s.ty.string else ",";
    while (i < num_args) : (i += 1) {
        if (i != 0) try writer.writeAll(ofs);
        _ = try writer.print("{}", .{self.value_stack.pop()});
    }

    try self.value_stack.append(Value.new(.nil, offset));
    self.ip.* += 1;
}
fn oneArgMath(self: *Vm, builtin: Value) anyerror!void {
    // Get args count.
    self.ip.* += 1;
    const num_args = self.instructions[self.ip.*];
    if (num_args != 1) {
        const location = Location.getLocation(self.filename, self.src, builtin.offset);
        std.log.err("Builtin call requires one argument; {}", .{location});
        return error.InvalidBuiltinCall;
    }

    const x_val = self.value_stack.pop();
    const x = x_val.asFloat() orelse {
        const location = Location.getLocation(self.filename, self.src, builtin.offset);
        std.log.err("Cannot convert {s} to float; {}", .{ @tagName(x_val.ty), location });
        return error.InvalidArg;
    };

    const result = switch (builtin.ty.builtin) {
        .cos => Value.new(.{ .float = @cos(x.ty.float) }, builtin.offset),
        .exp => Value.new(.{ .float = std.math.exp(x.ty.float) }, builtin.offset),
        .int => Value.new(.{ .int = @floatToInt(isize, @trunc(x.ty.float)) }, builtin.offset),
        .log => Value.new(.{ .float = @log(x.ty.float) }, builtin.offset),
        .rand => Value.new(.{ .uint = std.rand.DefaultPrng.init(@intCast(usize, std.time.timestamp())).random().uintAtMost(usize, x.ty.uint) }, builtin.offset),
        .sin => Value.new(.{ .float = @sin(x.ty.float) }, builtin.offset),
        .sqrt => Value.new(.{ .float = @sqrt(x.ty.float) }, builtin.offset),
        else => unreachable,
    };
    try self.value_stack.append(result);
    self.ip.* += 1;
}
fn contains(self: *Vm, offset: u16) anyerror!void {
    const haystack = self.value_stack.pop();
    const needle = self.value_stack.pop();
    if (haystack.ty != .list and
        haystack.ty != .map and
        haystack.ty != .string)
    {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("contains not allowed on {s}; {}", .{ @tagName(haystack.ty), location });
        return error.InvalidContains;
    }

    const result = switch (haystack.ty) {
        .list => |l| for (l.items) |item| {
            if (needle.eql(item)) break Value.new(.{ .boolean = true }, offset);
        } else Value.new(.{ .boolean = false }, offset),
        .map => |m| mp: {
            var iter = m.valueIterator();
            break :mp while (iter.next()) |value_ptr| {
                if (needle.eql(value_ptr.*)) break Value.new(.{ .boolean = true }, offset);
            } else Value.new(.{ .boolean = false }, offset);
        },
        .string => |s| str: {
            if (needle.ty != .string) {
                const location = Location.getLocation(self.filename, self.src, needle.offset);
                std.log.err("contains needle on strings must be a string; {}", .{location});
                return error.InvalidContains;
            }

            break :str Value.new(.{ .boolean = std.mem.containsAtLeast(u8, s, 1, needle.ty.string) }, offset);
        },
        else => unreachable,
    };

    try self.value_stack.append(result);
    self.ip.* += 2;
}
fn indexOf(self: *Vm, offset: u16) anyerror!void {
    const haystack = self.value_stack.pop();
    const needle = self.value_stack.pop();
    if (haystack.ty != .list and haystack.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("indexOf not allowed on {s}; {}", .{ @tagName(haystack.ty), location });
        return error.InvalidIndexOf;
    }

    const result = switch (haystack.ty) {
        .list => |l| for (l.items) |item, i| {
            if (needle.eql(item)) break Value.new(.{ .uint = i }, offset);
        } else Value.new(.nil, offset),
        .string => |s| str: {
            if (needle.ty != .string) {
                const location = Location.getLocation(self.filename, self.src, offset);
                std.log.err("indexOf on string needle must be string; {}", .{location});
                return error.InvalidIndexOf;
            }
            var giter = try GraphemeIterator.init(s);
            var i: usize = 0;
            break :str while (giter.next()) |grapheme| : (i += 1) {
                if (std.mem.eql(u8, needle.ty.string, grapheme.bytes)) break Value.new(.{ .uint = i }, offset);
            } else Value.new(.nil, offset);
        },
        else => unreachable,
    };

    try self.value_stack.append(result);
    self.ip.* += 2;
}

fn lastIndexOf(self: *Vm, offset: u16) anyerror!void {
    const haystack = self.value_stack.pop();
    const needle = self.value_stack.pop();
    if (haystack.ty != .list and haystack.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("lastIndexOf not allowed on {s}; {}", .{ @tagName(haystack.ty), location });
        return error.InvalidLastIndexOf;
    }

    const result = switch (haystack.ty) {
        .list => |l| lst: {
            var i: usize = 0;
            const len = l.items.len;
            break :lst while (i <= len) : (i += 1) {
                if (needle.eql(l.items[len - i])) break Value.new(.{ .uint = i }, offset);
            } else Value.new(.nil, offset);
        },
        .string => |s| str: {
            if (needle.ty != .string) {
                const location = Location.getLocation(self.filename, self.src, offset);
                std.log.err("lastIndexOf needle on string must be string; {}", .{location});
                return error.InvalidLastIndexOf;
            }
            var giter = try GraphemeIterator.init(s);
            var i: usize = 0;
            var index = Value.new(.nil, offset);
            while (giter.next()) |grapheme| : (i += 1) {
                if (std.mem.eql(u8, needle.ty.string, grapheme.bytes)) index = Value.new(.{ .uint = i }, offset);
            }

            break :str index;
        },
        else => unreachable,
    };

    try self.value_stack.append(result);
    self.ip.* += 2;
}
fn length(self: *Vm, offset: u16) anyerror!void {
    const value = self.value_stack.pop();
    if (value.ty != .list and
        value.ty != .map and
        value.ty != .string)
    {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("len not allowed on {s}; {}", .{ @tagName(value.ty), location });
        return error.InvalidLen;
    }

    const result = switch (value.ty) {
        .list => |l| Value.new(.{ .uint = l.items.len }, offset),
        .map => |m| Value.new(.{ .uint = m.count() }, offset),
        .string => |s| Value.new(.{ .uint = s.len }, offset),
        else => unreachable,
    };
    try self.value_stack.append(result);
    self.ip.* += 2;
}
fn mapKeys(self: *Vm, offset: u16) anyerror!void {
    const m = self.value_stack.pop();
    if (m.ty != .map) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("keys not allowed on {s}; {}", .{ @tagName(m.ty), location });
        return error.InvalidKeys;
    }

    const keys_ptr = try self.allocator.create(std.ArrayList(Value));
    keys_ptr.* = if (m.ty.map.count() == 0)
        std.ArrayList(Value).init(self.allocator)
    else
        try std.ArrayList(Value).initCapacity(self.allocator, m.ty.map.count());
    var key_iter = m.ty.map.keyIterator();
    while (key_iter.next()) |key| keys_ptr.appendAssumeCapacity(Value.new(.{ .string = key.* }, offset));

    try self.value_stack.append(Value.new(.{ .list = keys_ptr }, offset));
    self.ip.* += 2;
}
fn mapValues(self: *Vm, offset: u16) anyerror!void {
    const m = self.value_stack.pop();
    if (m.ty != .map) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("values not allowed on {s}; {}", .{ @tagName(m.ty), location });
        return error.InvalidValues;
    }

    const values_ptr = try self.allocator.create(std.ArrayList(Value));
    values_ptr.* = if (m.ty.map.count() == 0)
        std.ArrayList(Value).init(self.allocator)
    else
        try std.ArrayList(Value).initCapacity(self.allocator, m.ty.map.count());
    var value_iter = m.ty.map.valueIterator();
    while (value_iter.next()) |value| values_ptr.appendAssumeCapacity(value.*);

    try self.value_stack.append(Value.new(.{ .list = values_ptr }, offset));
    self.ip.* += 2;
}
fn listMeanHelper(list: std.ArrayList(Value)) f64 {
    var sum: f64 = 0;
    var count: f64 = 0;
    for (list.items) |item| {
        if (item.asFloat()) |f| {
            sum += f.ty.float;
            count += 1;
        }
    }

    return sum / count;
}
fn listMean(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("mean not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidMean;
    }

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.{ .float = 0 }, offset));
        self.ip.* += 2;
        return;
    }

    try self.value_stack.append(Value.new(.{ .float = listMeanHelper(l.ty.list.*) }, offset));
    self.ip.* += 2;
}
fn listMedian(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("median not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidMedian;
    }

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.{ .float = 0 }, offset));
        self.ip.* += 2;
        return;
    }

    var list_copy = try std.ArrayList(f64).initCapacity(self.allocator, l.ty.list.items.len);

    for (l.ty.list.items) |item| {
        if (item.asFloat()) |f| list_copy.appendAssumeCapacity(f.ty.float);
    }
    std.sort.sort(f64, list_copy.items, {}, comptime std.sort.asc(f64));

    var median: f64 = @intToFloat(f64, list_copy.items.len) + 1 / 2 - 1;
    if (list_copy.items.len % 2 == 0) {
        const mid = list_copy.items.len / 2 - 1;
        median = (list_copy.items[mid] + list_copy.items[mid + 1]) / 2;
    }

    try self.value_stack.append(Value.new(.{ .float = median }, offset));
    self.ip.* += 2;
}
fn listMode(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("mode not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidMode;
    }

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil, offset));
        self.ip.* += 2;
        return;
    }

    var counts = std.StringHashMap(usize).init(self.allocator);
    var key_buf: [4096]u8 = undefined;

    for (l.ty.list.items) |item| {
        if (item.asFloat()) |f| {
            const key_str = try std.fmt.bufPrint(&key_buf, "{}", .{f});
            var entry = try counts.getOrPut(try self.allocator.dupe(u8, key_str));
            if (entry.found_existing) entry.value_ptr.* += 1 else entry.value_ptr.* = 1;
        }
    }

    var iter = counts.iterator();
    var highest: usize = 0;
    while (iter.next()) |entry| {
        if (entry.value_ptr.* > highest) highest = entry.value_ptr.*;
    }

    iter = counts.iterator();
    var mode_ptr = try self.allocator.create(std.ArrayList(Value));
    mode_ptr.* = std.ArrayList(Value).init(self.allocator);
    while (iter.next()) |entry| {
        if (entry.value_ptr.* == highest) try mode_ptr.append(Value.new(.{ .float = std.fmt.parseFloat(f64, entry.key_ptr.*) catch unreachable }, offset));
    }
    std.sort.sort(Value, mode_ptr.items, {}, Value.lessThan);

    const result = if (mode_ptr.items.len == counts.count()) Value.new(.nil, offset) else Value.new(.{ .list = mode_ptr }, offset);
    try self.value_stack.append(result);
    self.ip.* += 2;
}
fn listStdev(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("stdev not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidStdev;
    }

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.{ .float = 0 }, offset));
        self.ip.* += 2;
        return;
    }

    const mean = listMeanHelper(l.ty.list.*);

    var sum_of_squares: f64 = 0;
    var count: f64 = 0;
    for (l.ty.list.items) |item| {
        if (item.asFloat()) |f| {
            const diff = f.ty.float - mean;
            const square = diff * diff;
            sum_of_squares += square;
            count += 1;
        }
    }

    const sos_by_count = sum_of_squares / count;

    try self.value_stack.append(Value.new(.{ .float = @sqrt(sos_by_count) }, offset));
    self.ip.* += 2;
}
fn listMin(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("min not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidMin;
    }

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil, offset));
        self.ip.* += 2;
        return;
    }

    var min = l.ty.list.items[0];
    for (l.ty.list.items) |item| {
        const comparison = try min.cmp(item);
        if (comparison == .gt) min = item;
    }

    try self.value_stack.append(min);
    self.ip.* += 2;
}
fn listMax(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("max not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidMin;
    }

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil, offset));
        self.ip.* += 2;
        return;
    }

    var max = l.ty.list.items[0];
    for (l.ty.list.items) |item| {
        const comparison = try max.cmp(item);
        if (comparison == .lt) max = item;
    }

    try self.value_stack.append(max);
    self.ip.* += 2;
}
fn listSort(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("sort not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidSort;
    }

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(l);
        self.ip.* += 2;
        return;
    }

    std.sort.sort(Value, l.ty.list.items, {}, Value.lessThan);
    try self.value_stack.append(l);
    self.ip.* += 2;
}
fn listReverse(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("reverse not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidReverse;
    }

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(l);
        self.ip.* += 2;
        return;
    }

    std.mem.reverse(Value, l.ty.list.items);
    try self.value_stack.append(l);
    self.ip.* += 2;
}
fn strSplit(self: *Vm, offset: u16) anyerror!void {
    const value = self.value_stack.pop();
    if (value.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("split not allowed on {s}; {}", .{ @tagName(value.ty), location });
        return error.InvalidSplit;
    }

    const delim = self.value_stack.pop();
    if (delim.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, delim.offset);
        std.log.err("split delimiter must be a string; {}", .{location});
        return error.InvalidSplit;
    }

    var list_ptr = try self.allocator.create(std.ArrayList(Value));
    list_ptr.* = std.ArrayList(Value).init(self.allocator);
    var iter = std.mem.split(u8, value.ty.string, delim.ty.string);
    while (iter.next()) |sub| try list_ptr.append(Value.new(.{ .string = sub }, 0));

    try self.value_stack.append(Value.new(.{ .list = list_ptr }, offset));
    self.ip.* += 2;
}
fn listJoin(self: *Vm, offset: u16) anyerror!void {
    const value = self.value_stack.pop();
    if (value.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("join not allowed on {s}; {}", .{ @tagName(value.ty), location });
        return error.InvalidJoin;
    }

    const delim = self.value_stack.pop();
    if (delim.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, delim.offset);
        std.log.err("join delimiter must be a string; {}", .{location});
        return error.InvalidJoin;
    }

    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();

    for (value.ty.list.items) |item, i| {
        if (i != 0 and delim.ty.string.len > 0) try buf.appendSlice(delim.ty.string);
        _ = try writer.print("{}", .{item});
    }

    try self.value_stack.append(Value.new(.{ .string = buf.items }, offset));
    self.ip.* += 2;
}
fn strEndsWith(self: *Vm, offset: u16) anyerror!void {
    const str = self.value_stack.pop();
    const ending = self.value_stack.pop();
    if (str.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("endsWith not allowed on {s}; {}", .{ @tagName(str.ty), location });
        return error.InvalidEndsWith;
    }
    if (ending.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("endsWith argument must be string; {}", .{location});
        return error.InvalidEndsWith;
    }

    const result = Value.new(.{ .boolean = std.mem.endsWith(u8, str.ty.string, ending.ty.string) }, str.offset);

    try self.value_stack.append(result);
    self.ip.* += 2;
}
fn strStartsWith(self: *Vm, offset: u16) anyerror!void {
    const str = self.value_stack.pop();
    const beginning = self.value_stack.pop();
    if (str.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("startsWith not allowed on {s}; {}", .{ @tagName(str.ty), location });
        return error.InvalidStartsWith;
    }
    if (beginning.ty != .string) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("startsWith argument must be string; {}", .{location});
        return error.InvalidStartsWith;
    }

    const result = Value.new(.{ .boolean = std.mem.startsWith(u8, str.ty.string, beginning.ty.string) }, str.offset);
    try self.value_stack.append(result);

    self.ip.* += 2;
}
fn listMap(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("map not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidMapCall;
    }

    const f = self.value_stack.pop();
    if (f.ty != .func) {
        const location = Location.getLocation(self.filename, self.src, f.offset);
        std.log.err("map requires function argument; {}", .{location});
        return error.InvalidMapCall;
    }

    var list_ptr = try self.allocator.create(std.ArrayList(Value));
    list_ptr.* = try std.ArrayList(Value).initCapacity(self.allocator, l.ty.list.items.len);

    for (l.ty.list.items) |item, i| {
        const v = try self.evalListPredicate(f, item, i);
        list_ptr.appendAssumeCapacity(v);
    }

    try self.value_stack.append(Value.new(.{ .list = list_ptr }, offset));
    self.ip.* += 2;
}
fn listFilter(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("filter not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidFilter;
    }

    const f = self.value_stack.pop();
    if (f.ty != .func) {
        const location = Location.getLocation(self.filename, self.src, f.offset);
        std.log.err("filter requires function argument; {}", .{location});
        return error.InvalidFilter;
    }

    var list_ptr = try self.allocator.create(std.ArrayList(Value));
    list_ptr.* = std.ArrayList(Value).init(self.allocator);

    for (l.ty.list.items) |item, i| {
        const v = try self.evalListPredicate(f, item, i);
        if (isTruthy(v)) try list_ptr.append(item);
    }

    try self.value_stack.append(Value.new(.{ .list = list_ptr }, offset));
    self.ip.* += 2;
}
fn listEach(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("each not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidEach;
    }

    const f = self.value_stack.pop();
    if (f.ty != .func) {
        const location = Location.getLocation(self.filename, self.src, f.offset);
        std.log.err("each requires function argument; {}", .{location});
        return error.InvalidEach;
    }

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(l);
        self.ip.* += 2;
        return;
    }

    for (l.ty.list.items) |item, i| _ = try self.evalListPredicate(f, item, i);

    try self.value_stack.append(l);
    self.ip.* += 2;
}
fn listReduce(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("reduce not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidReduce;
    }

    var acc = self.value_stack.pop();
    const f = self.value_stack.pop();
    if (f.ty != .func) {
        const location = Location.getLocation(self.filename, self.src, f.offset);
        std.log.err("reduce requires function argument; {}", .{location});
        return error.InvalidMapCall;
    }

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil, offset));
        self.ip.* += 2;
        return;
    }

    // Set up sub-VM arena.
    var vm_arena = std.heap.ArenaAllocator.init(self.allocator);
    defer vm_arena.deinit();
    const vm_allocator = vm_arena.allocator();

    for (l.ty.list.items) |item, i| {
        // Set up function scope.
        var func_scope = Scope.init(vm_allocator, .function, self.scope);

        // Assign args as locals in function scope.
        try func_scope.store("acc", acc);
        try func_scope.store("it", item);
        try func_scope.store("@0", item);
        if (f.ty.func.params.len > 0) try func_scope.store(f.ty.func.params[0], acc);
        if (f.ty.func.params.len > 1) try func_scope.store(f.ty.func.params[1], item);
        try func_scope.store("index", Value.new(.{ .uint = i }, 0));

        var vm = try init(
            vm_allocator,
            self.filename,
            self.src,
            self.constants,
            f.ty.func.instructions,
            func_scope,
            self.output,
        );
        try vm.run();

        acc = vm.last_popped;
    }

    try self.value_stack.append(acc);
    self.ip.* += 2;
}
fn rand(self: *Vm, offset: u16) anyerror!void {
    // Get args count.
    self.ip.* += 1;
    const num_args = self.instructions[self.ip.*];
    if (num_args != 1) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("rand requres one argument; {}", .{location});
        return error.InvalidRand;
    }

    const x_val = self.value_stack.pop();
    if (x_val.ty != .uint) {
        const location = Location.getLocation(self.filename, self.src, x_val.offset);
        std.log.err("rand argument cannot be converted to float; {}", .{location});
        return error.InvalidRand;
    }

    const result = Value.new(.{ .uint = std.rand.DefaultPrng.init(@intCast(usize, std.time.timestamp())).random().uintAtMost(usize, x_val.ty.uint) }, offset);
    try self.value_stack.append(result);
    self.ip.* += 1;
}
fn listPush(self: *Vm, offset: u16) anyerror!void {
    const l = self.value_stack.pop();
    if (l.ty != .list) {
        const location = Location.getLocation(self.filename, self.src, offset);
        std.log.err("push not allowed on {s}; {}", .{ @tagName(l.ty), location });
        return error.InvalidPush;
    }

    var item = self.value_stack.pop();
    try l.ty.list.append(try item.copy(l.ty.list.allocator));
    try self.value_stack.append(item);
    self.ip.* += 2;
}

fn evalListPredicate(self: Vm, func: Value, item: Value, index: usize) anyerror!Value {
    // Set up Sub-VM arena.
    var vm_arena = std.heap.ArenaAllocator.init(self.allocator);
    defer vm_arena.deinit();
    const vm_allocator = vm_arena.allocator();

    // Set up function scope.
    var func_scope = Scope.init(vm_allocator, .function, self.scope);

    // Assign args as locals in function scope.
    try func_scope.store("it", item);
    try func_scope.store("@0", item);
    if (func.ty.func.params.len > 0) try func_scope.store(func.ty.func.params[0], item);
    try func_scope.store("index", Value.new(.{ .uint = index }, 0));

    var vm = try init(
        vm_allocator,
        self.filename,
        self.src,
        self.constants,
        func.ty.func.instructions,
        func_scope,
        self.output,
    );
    try vm.run();

    return vm.last_popped;
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
        const ins = self.instructions[ins_index];

        if (Bytecode.Opcode.fromInt(ins)) |def| {
            if (def.bytes == 3) {
                const operand = std.mem.bytesAsSlice(u16, self.instructions[ins_index + 1 .. ins_index + 3])[0];
                std.debug.print("\t{}: {} -> {}\n", .{ ins_index, def.opcode, operand });
            } else if (def.bytes == 2) {
                std.debug.print("\t{}: {} -> {}\n", .{ ins_index, def.opcode, self.instructions[ins_index + 1] });
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
    var compiler_arena = std.heap.ArenaAllocator.init(allocator);
    errdefer compiler_arena.deinit();
    const compiler_allocator = compiler_arena.allocator();

    var lexer = Lexer{
        .allocator = compiler_allocator,
        .filename = "inline",
        .src = input,
    };
    var tokens = try lexer.lex();
    var parser = Parser{
        .allocator = compiler_allocator,
        .filename = "inline",
        .src = input,
        .tokens = tokens,
    };

    var vm_arena = std.heap.ArenaAllocator.init(allocator);
    errdefer vm_arena.deinit();
    const vm_allocator = vm_arena.allocator();

    const program = try parser.parse();
    var compiler = try Compiler.init(compiler_allocator);
    const compiled = try compiler.compileProgram(vm_allocator, program);

    var scope = Scope.init(allocator, .function, null);
    defer scope.deinit();
    try addBuiltins(&scope);

    const constants = compiled.rules.constants;
    const instructions = compiled.rules.instructions;

    compiler_arena.deinit();

    var output = std.ArrayList(u8).init(vm_allocator);

    var vm = try init(
        vm_allocator,
        "inline",
        input,
        constants,
        instructions,
        scope,
        &output,
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

        .builtin,
        .func,
        .list,
        .map,
        .range,
        => try std.testing.expect(expected.eql(last_popped)),
    }

    vm_arena.deinit();

    if (debug_dumps) scope.dump();
}
fn testLastValueWithOutput(input: []const u8, expected: Value, expected_output: []const u8) !void {
    const Lexer = @import("Lexer.zig");
    const Parser = @import("Parser.zig");
    const Compiler = @import("Compiler.zig");

    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
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
    var compiler = try Compiler.init(arena.allocator());
    const compiled = try compiler.compileProgram(arena.allocator(), program);

    var scope = Scope.init(allocator, .function, null);
    defer scope.deinit();
    try addBuiltins(&scope);

    var output = std.ArrayList(u8).init(arena.allocator());

    var vm = try init(
        arena.allocator(),
        "inline",
        input,
        compiled.rules.constants,
        compiled.rules.instructions,
        scope,
        &output,
    );
    if (debug_dumps) vm.dump();
    try vm.run();

    try std.testing.expectEqualStrings(expected_output, vm.output.items);

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

        .builtin,
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

test "Vm subscript" {
    try testLastValue("[1, 2112, 3][1]", Value.new(.{ .uint = 2112 }, 0));
    try testLastValue(
        \\["a": 1, "b": 2112, "c": 3]["b"]
    , Value.new(.{ .uint = 2112 }, 0));
    try testLastValue(
        \\["a": 1, "b": 2112, "c": 3]["d"]
    , Value.new(.nil, 0));
    try testLastValue("[1, 2112, 3][1..<3][0]", Value.new(.{ .uint = 2112 }, 0));
}

test "Vm subscript assign" {
    try testLastValue("l := [1, 2, 3]; l[1] = 2112; l[1]", Value.new(.{ .uint = 2112 }, 0));
    try testLastValue(
        \\m := ["a": 1, "b": 2, "c": 3]; m["b"] = 2112; m["b"]
    , Value.new(.{ .uint = 2112 }, 0));
}

test "Vm combo assign" {
    try testLastValue("i := 0; i += 1; i", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("i := 1; i -= 1; i", Value.new(.{ .uint = 0 }, 0));
    try testLastValue("i := 1; i *= 1; i", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("i := 1; i /= 1; i", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("i := 1; i %= 1; i", Value.new(.{ .uint = 0 }, 0));

    try testLastValue("l := [0,1,2]; l[0] += 1; l[0]", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("l := [1,2,3]; l[0] -= 1; l[0]", Value.new(.{ .uint = 0 }, 0));
    try testLastValue("l := [1,2,3]; l[0] *= 1; l[0]", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("l := [1,2,3]; l[0] /= 1; l[0]", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("l := [1,2,3]; l[0] %= 1; l[0]", Value.new(.{ .uint = 0 }, 0));

    try testLastValue(
        \\m := ["a": 1, "b": 2]; m["a"] += 1; m["a"]
    , Value.new(.{ .uint = 2 }, 0));
    try testLastValue(
        \\m := ["a": 1, "b": 2]; m["a"] -= 1; m["a"]
    , Value.new(.{ .uint = 0 }, 0));
    try testLastValue(
        \\m := ["a": 1, "b": 2]; m["a"] *= 1; m["a"]
    , Value.new(.{ .uint = 1 }, 0));
    try testLastValue(
        \\m := ["a": 1, "b": 2]; m["a"] /= 1; m["a"]
    , Value.new(.{ .uint = 1 }, 0));
    try testLastValue(
        \\m := ["a": 1, "b": 2]; m["a"] %= 1; m["a"]
    , Value.new(.{ .uint = 0 }, 0));
}

test "Vm ternary" {
    try testLastValue("true ? 1 : 0", Value.new(.{ .uint = 1 }, 6));
    try testLastValue("false ? 1 : 0", Value.new(.{ .uint = 0 }, 6));
}

test "Vm Elvis" {
    try testLastValue("foo := 3; bar := foo ?: 2", Value.new(.{ .uint = 3 }, 6));
    try testLastValue("foo := 0; bar := foo ?: 2", Value.new(.{ .uint = 2 }, 6));
}

test "Vm Elvis assign" {
    try testLastValue("foo := 0; foo ?= 2; foo", Value.new(.{ .uint = 2 }, 6));
    try testLastValue("foo := 2; foo ?= 3; foo", Value.new(.{ .uint = 2 }, 6));
}

test "Vm grouped" {
    try testLastValue("(1 + 2) * 3", Value.new(.{ .uint = 9 }, 6));
}

test "Vm math builtins" {
    try testLastValue("atan2(0, -1)", Value.new(.{ .float = 3.141592653589793e+00 }, 0));
    try testLastValue("cos(-1)", Value.new(.{ .float = 5.403023058681398e-01 }, 0));
    try testLastValue("exp(5)", Value.new(.{ .float = 1.484131591025766e+02 }, 0));
    try testLastValue("int(-3.9)", Value.new(.{ .int = -3 }, 0));
    try testLastValue("int(3.9)", Value.new(.{ .int = 3 }, 0));
    try testLastValue("log(3.14)", Value.new(.{ .float = 1.144222799920162e+00 }, 0));
    //try testLastValue("rand(10)", Value.new(.{ .uint = 10 }, 0));
    try testLastValue("sin(3.14)", Value.new(.{ .float = 1.5926529164868282e-03 }, 0));
    try testLastValue("sqrt(49)", Value.new(.{ .float = 7 }, 0));
}

test "Vm method builtins" {
    const mean_input =
        \\l := [1, 2, "foo", 3, nil]
        \\l.mean()
    ;
    const median_input =
        \\l := [1, 2, "foo", 3, nil, 4]
        \\l.median()
    ;
    const mode_input =
        \\l := [1, 1, "foo", 2, nil, 2, 3, 4]
        \\l.mode().len()
    ;
    const stdev_input =
        \\l := [1, 1, "foo", 2, nil, 2, 3, 4]
        \\l.stdev()
    ;

    try testLastValue("[1, 2, 3].len()", Value.new(.{ .uint = 3 }, 0));
    try testLastValue(
        \\["a": 1, "b": 2, "c": 3].len()
    , Value.new(.{ .uint = 3 }, 0));
    try testLastValue(
        \\"foo".len()
    , Value.new(.{ .uint = 3 }, 0));
    try testLastValue(
        \\["a": 1, "b": 2, "c": 3].keys().len()
    , Value.new(.{ .uint = 3 }, 0));
    try testLastValue(
        \\["a": 1, "b": 2, "c": 3].values().len()
    , Value.new(.{ .uint = 3 }, 0));
    try testLastValue(mean_input, Value.new(.{ .float = 2 }, 0));
    try testLastValue(median_input, Value.new(.{ .float = 2.5 }, 0));
    try testLastValue(mode_input, Value.new(.{ .uint = 2 }, 0));
    try testLastValue(stdev_input, Value.new(.{ .float = 1.0671873729054748 }, 0));
    try testLastValue("[1, 2, 3].min()", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("[1, 2, 3].max()", Value.new(.{ .uint = 3 }, 0));
    try testLastValue(
        \\["a", "z", "B"].min()
    , Value.new(.{ .string = "B" }, 0));
    try testLastValue("[2, 3, 1].sort()[0]", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("[2, 3, 1].reverse()[0]", Value.new(.{ .uint = 1 }, 0));
    try testLastValue("[2, 3, 1].contains(3)", Value.new(.{ .boolean = true }, 0));
    try testLastValue("[2, 3, 1].contains(4)", Value.new(.{ .boolean = false }, 0));
    try testLastValue(
        \\"foo".contains("oo")
    , Value.new(.{ .boolean = true }, 0));
    try testLastValue(
        \\["a": 2, "b": 3].contains(3)
    , Value.new(.{ .boolean = true }, 0));
    try testLastValue("[2, 3, 1].indexOf(1)", Value.new(.{ .uint = 2 }, 0));
    try testLastValue("[2, 3, 1].indexOf(4)", Value.new(.nil, 0));
    try testLastValue(
        \\"H\u65\u301llo".indexOf("l")
    , Value.new(.{ .uint = 2 }, 0));
    try testLastValue(
        \\"H\u65\u301llo".lastIndexOf("l")
    , Value.new(.{ .uint = 3 }, 0));
    try testLastValue(
        \\"foo,bar,baz".split(",")[1]
    , Value.new(.{ .string = "bar" }, 0));
    try testLastValue(
        \\["foo", 1, 2.3, nil].join(",")
    , Value.new(.{ .string = "foo,1,2.3," }, 0));
    try testLastValue(
        \\f := { a => a * 2 + index }
        \\[1, 2, 3].map(f)[1]
    , Value.new(.{ .uint = 5 }, 0));
    try testLastValue(
        \\[1, 2, 3].filter() { it > 1 }[1]
    , Value.new(.{ .uint = 3 }, 0));
    try testLastValue(
        \\total := 0
        \\[1, 2, 3].each() { total = total + it }
        \\total
    , Value.new(.{ .uint = 6 }, 0));
    try testLastValue(
        \\[1, 2, 3].reduce(1) { acc * it }
    , Value.new(.{ .uint = 6 }, 0));
    try testLastValueWithOutput(
        \\print("foo", 1, 2, 3.14)
    , Value.new(.nil, 0), "foo,1,2,3.14");
    try testLastValueWithOutput(
        \\print("foo", 1, 2, 3.14, "foo {1}")
    , Value.new(.nil, 0), "foo,1,2,3.14,foo 1");
    try testLastValueWithOutput(
        \\print("foo", 1, 2, 3.14, "{#d:0>3# 1}")
    , Value.new(.nil, 0), "foo,1,2,3.14,001");
    try testLastValue(
        \\"H\u65\u301llo".chars()[1]
    , Value.new(.{ .string = "\u{65}\u{301}" }, 0));
    try testLastValue(
        \\"Hello".startsWith("Hell")
    , Value.new(.{ .boolean = true }, 0));
    try testLastValue(
        \\"Hello".endsWith("llo")
    , Value.new(.{ .boolean = true }, 0));
}
