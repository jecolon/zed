const std = @import("std");

const Compiler = @import("Compiler.zig");
const Context = @import("Context.zig");
const Node = @import("Node.zig");
const Scope = @import("Scope.zig");
const ScopeStack = @import("ScopeStack.zig");
const Value = @import("Value.zig");
const runtimePrint = @import("fmt.zig").runtimePrint;

allocator: std.mem.Allocator,
ctx: Context,
last_popped: Value = Value.new(.nil, 0),
output: *std.ArrayList(u8),

instructions: []const u8 = undefined,
ip: *u16 = undefined,

scope_stack: ScopeStack,
frame_stack: std.ArrayList(Frame),
value_stack: std.ArrayList(Value),

const Vm = @This();

pub fn init(
    allocator: std.mem.Allocator,
    instructions: []const u8,
    scope_stack: ScopeStack,
    ctx: Context,
    output: *std.ArrayList(u8),
) !Vm {
    var self = Vm{
        .allocator = allocator,
        .ctx = ctx,
        .output = output,
        .frame_stack = std.ArrayList(Frame).init(allocator),
        .scope_stack = scope_stack,
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
            .jump => self.execJump(),
            .jump_false => self.execJumpFalse(),
            .jump_true => self.execJumpTrue(),
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
            .call => try self.execCall(),
            .func => try self.execFunc(),
            .func_return => {
                if (self.frame_stack.items.len == 1) {
                    // Top-level return
                    self.last_popped = self.value_stack.pop();
                    self.ip.* += 1;
                    break; // VM exit
                }

                self.execReturn();
            },
            // Variables
            .define => try self.execDefine(),
            .load => try self.execLoad(),
            .store => try self.execStore(),
            // Infix
            .add => try self.execAdd(),
            .sub => try self.execSub(),
            .mul => try self.execMul(),
            .div => try self.execDiv(),
            .mod => try self.execMod(),
            // Comparison
            .lt,
            .lte,
            .gt,
            .gte,
            => try self.execComparison(opcode),
            .eq, .neq => try self.execEqNeq(opcode),
            // Prefix
            .neg => try self.execNeg(),
            .not => try self.execNot(),
            // Data structures
            .list => try self.execList(),
            .map => try self.execMap(),
            .range => try self.execRange(),
            .subscript => try self.execSubscript(),
            .set => try self.execSet(),
            // Record Ranges
            .rec_range => try self.execRecRange(),

            else => {
                std.log.err("{s}", .{@tagName(opcode)});
                unreachable;
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
    self.ip.* += 1;
    try self.value_stack.append(Value.new(.{ .boolean = false }, self.getOffset()));
    self.ip.* += 2;
}
fn execBoolTrue(self: *Vm) !void {
    self.ip.* += 1;
    try self.value_stack.append(Value.new(.{ .boolean = true }, self.getOffset()));
    self.ip.* += 2;
}
fn execNil(self: *Vm) !void {
    self.ip.* += 1;
    try self.value_stack.append(Value.new(.nil, self.getOffset()));
    self.ip.* += 2;
}

fn execFloat(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const f = self.getNumber(f64, self.ip.*, 8);
    self.ip.* += 8;
    try self.value_stack.append(Value.new(.{ .float = f }, offset));
}
fn execFloatRef(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const start = self.getU16(self.ip.*);
    self.ip.* += 2;
    const f = self.getNumber(f64, start, 8);
    try self.value_stack.append(Value.new(.{ .float = f }, offset));
}
fn execInt(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const i = self.getNumber(i64, self.ip.*, 8);
    self.ip.* += 8;
    try self.value_stack.append(Value.new(.{ .int = i }, offset));
}
fn execIntRef(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const start = self.getU16(self.ip.*);
    self.ip.* += 2;
    const i = self.getNumber(i64, start, 8);
    try self.value_stack.append(Value.new(.{ .int = i }, offset));
}
fn execUint(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const u = self.getNumber(u64, self.ip.*, 8);
    self.ip.* += 8;
    try self.value_stack.append(Value.new(.{ .uint = u }, offset));
}
fn execUintRef(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const start = self.getU16(self.ip.*);
    self.ip.* += 2;
    const u = self.getNumber(u64, start, 8);
    try self.value_stack.append(Value.new(.{ .uint = u }, offset));
}

fn execFormat(self: *Vm) !void {
    self.ip.* += 1;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;
    const spec = self.getString(self.ip.*, len);
    self.ip.* += len;

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
}
fn execPlain(self: *Vm) !void {
    self.ip.* += 1;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;
    const s = self.getString(self.ip.*, len);
    self.ip.* += len;
    try self.value_stack.append(Value.new(.{ .string = s }, 0));
}
fn execPlainRef(self: *Vm) !void {
    self.ip.* += 1;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;
    const start = self.getU16(self.ip.*);
    self.ip.* += 2;
    const s = self.getString(start, len);
    try self.value_stack.append(Value.new(.{ .string = s }, 0));
}
fn execString(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;

    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();
    var i: usize = 0;
    while (i < len) : (i += 1) _ = try writer.print("{}", .{self.value_stack.pop()});
    try self.value_stack.append(Value.new(.{ .string = buf.items }, offset));
}

fn execFunc(self: *Vm) !void {
    // Src offset
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

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

fn execCall(self: *Vm) anyerror!void {
    // Get the function.
    const callee = self.value_stack.pop();

    //TODO: Builtins
    //if (callee.ty == .builtin) {
    //    return switch (callee.ty.builtin) {
    //        .atan2 => try self.atan2(callee.offset),
    //        .chars => try self.strChars(callee.offset),
    //        .contains => try self.contains(callee.offset),
    //        .cos => try self.oneArgMath(callee),
    //        .each => try self.each(callee.offset),
    //        .endsWith => try self.strEndsWith(callee.offset),
    //        .exp => try self.oneArgMath(callee),
    //        .filter => try self.listFilter(callee.offset),
    //        .join => try self.listJoin(callee.offset),
    //        .indexOf => try self.indexOf(callee.offset),
    //        .int => try self.oneArgMath(callee),
    //        .keys => try self.mapKeys(callee.offset),
    //        .lastIndexOf => try self.lastIndexOf(callee.offset),
    //        .len => try self.length(callee.offset),
    //        .log => try self.oneArgMath(callee),
    //        .map => try self.listMap(callee.offset),
    //        .max => try self.listMax(callee.offset),
    //        .mean => try self.listMean(callee.offset),
    //        .median => try self.listMedian(callee.offset),
    //        .min => try self.listMin(callee.offset),
    //        .mode => try self.listMode(callee.offset),
    //        .print => try self.print(callee.offset, self.output.writer()),
    //        .pop => try self.listPop(callee.offset),
    //        .push => try self.listPush(callee.offset),
    //        .rand => try self.rand(callee.offset),
    //        .reduce => try self.listReduce(callee.offset),
    //        .reverse => try self.listReverse(callee.offset),
    //        .sin => try self.oneArgMath(callee),
    //        .sort => try self.listSort(callee.offset),
    //        .split => try self.strSplit(callee.offset),
    //        .sqrt => try self.oneArgMath(callee),
    //        .startsWith => try self.strStartsWith(callee.offset),
    //        .stdev => try self.listStdev(callee.offset),
    //        .values => try self.mapValues(callee.offset),
    //    };
    //}

    if (callee.ty != .func) return self.ctx.err("{s} is not callable.", .{@tagName(callee.ty)}, error.InvalidCall, callee.offset);

    // Prepare the child scope.
    var func_scope = Scope.init(self.allocator, .function);

    // Self-references
    if (callee.ty.func.name.len != 0) try func_scope.store(callee.ty.func.name, callee);

    // Process args
    self.ip.* += 1;
    const num_args = self.instructions[self.ip.*];

    var i: usize = 0;
    while (i < num_args) : (i += 1) {
        const arg = self.value_stack.pop();
        if (i == 0) try func_scope.store("it", arg); // it
        //TODO: Auto func arg names
        //var buf: [4]u8 = undefined;
        //const auto_arg_name = try std.fmt.bufPrint(&buf, "@{}", .{i});
        //try func_scope.store(try func_scope.allocator.dupe(u8, auto_arg_name), arg); // @0, @1, ...
        if (i < callee.ty.func.params.len) try func_scope.store(callee.ty.func.params[i], arg);
    }

    // Push the function's frame.
    try self.pushScope(func_scope);
    try self.pushFrame(callee.ty.func.instructions);
}

fn execDefine(self: *Vm) !void {
    // Offset
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    // Name
    const name = self.getName();
    // Is it already defined?
    if (self.scope_stack.isDefined(name)) return self.ctx.err("{s} already defined.", .{name}, error.NameAlreadyDefined, offset);
    // Value
    const rvalue = self.value_stack.pop();
    // Define
    try self.scope_stack.store(name, rvalue);
    try self.value_stack.append(rvalue);
}
fn execLoad(self: *Vm) !void {
    // Offset
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    // Name
    const name = self.getName();
    // Is the name defined?
    if (!self.scope_stack.isDefined(name)) return self.ctx.err("{s} is undefined.", .{name}, error.NameUndefined, offset);
    // Load
    try self.value_stack.append(self.scope_stack.load(name).?);
}
fn execStore(self: *Vm) !void {
    // Offset
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    // Combo assign
    const combo = @intToEnum(Node.Combo, self.instructions[self.ip.*]);
    self.ip.* += 1;
    // Name
    const name = self.getName();
    // Is the name defined?
    if (!self.scope_stack.isDefined(name)) return self.ctx.err("{s} is undefined.", .{name}, error.NameUndefined, offset);
    // Value
    const rvalue = self.value_stack.pop();
    // Store
    if (combo == .none) {
        try self.scope_stack.update(name, rvalue);
        try self.value_stack.append(rvalue);
    } else {
        const old_value = self.scope_stack.load(name).?;

        const new_value = switch (combo) {
            .none => unreachable,
            .add => try old_value.add(rvalue),
            .sub => try old_value.sub(rvalue),
            .mul => try old_value.mul(rvalue),
            .div => try old_value.div(rvalue),
            .mod => try old_value.mod(rvalue),
        };

        try self.scope_stack.update(name, new_value);
        try self.value_stack.append(new_value);
    }
}

// Arithmetic
fn execAdd(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (left.add(right)) |sum| {
        try self.value_stack.append(sum);
    } else |err| return self.ctx.err("{s} + {s} ?", .{ @tagName(left.ty), @tagName(right.ty) }, err, offset);
}
fn execSub(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (left.sub(right)) |diff| {
        try self.value_stack.append(diff);
    } else |err| return self.ctx.err("{s} - {s} ?", .{ @tagName(left.ty), @tagName(right.ty) }, err, offset);
}
fn execMul(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (left.mul(right)) |product| {
        try self.value_stack.append(product);
    } else |err| return self.ctx.err("{s} * {s} ?", .{ @tagName(left.ty), @tagName(right.ty) }, err, offset);
}
fn execDiv(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (left.div(right)) |quotient| {
        try self.value_stack.append(quotient);
    } else |err| return self.ctx.err("{s} / {s} ?", .{ @tagName(left.ty), @tagName(right.ty) }, err, offset);
}
fn execMod(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (left.mod(right)) |remainder| {
        try self.value_stack.append(remainder);
    } else |err| return self.ctx.err("{s} % {s} ?", .{ @tagName(left.ty), @tagName(right.ty) }, err, offset);
}
fn execComparison(self: *Vm, opcode: Compiler.Opcode) !void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const comparison = left.cmp(right) catch |err| return self.ctx.err("{s} {s} {s} ?", .{
        @tagName(left.ty),
        @tagName(opcode),
        @tagName(right.ty),
    }, err, offset);

    const result = switch (opcode) {
        .lt => Value.new(.{ .boolean = comparison == .lt }, offset),
        .lte => Value.new(.{ .boolean = comparison == .lt or comparison == .eq }, offset),
        .gt => Value.new(.{ .boolean = comparison == .gt }, offset),
        .gte => Value.new(.{ .boolean = comparison == .gt or comparison == .eq }, offset),

        else => unreachable,
    };

    try self.value_stack.append(result);
}
fn execEqNeq(self: *Vm, opcode: Compiler.Opcode) !void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    var comparison = left.eql(right);
    if (opcode == .neq) comparison = !comparison;
    try self.value_stack.append(Value.new(.{ .boolean = comparison }, offset));
}

fn execNot(self: *Vm) !void {
    const value = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    if (value.ty != .boolean) return self.ctx.err("!{s} ?", .{@tagName(value.ty)}, error.InvalidNot, offset);
    try self.value_stack.append(Value.new(.{ .boolean = !value.ty.boolean }, offset));
}
fn execNeg(self: *Vm) !void {
    const value = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    switch (value.ty) {
        .float => |f| try self.value_stack.append(Value.new(.{ .float = -f }, offset)),
        .int => |i| try self.value_stack.append(Value.new(.{ .int = -i }, offset)),
        .uint => |u| try self.value_stack.append(Value.new(.{ .int = -@intCast(isize, u) }, offset)),
        else => return self.ctx.err("-{s} ?", .{@tagName(value.ty)}, error.InvalidNeg, offset),
    }
}

fn execList(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;

    const list_ptr = try self.allocator.create(std.ArrayList(Value));
    list_ptr.* = try std.ArrayList(Value).initCapacity(self.allocator, len);
    var i: usize = 0;
    while (i < len) : (i += 1) list_ptr.appendAssumeCapacity(self.value_stack.pop());

    try self.value_stack.append(Value.new(.{ .list = list_ptr }, offset));
}
fn execMap(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;

    const map_ptr = try self.allocator.create(std.StringHashMap(Value));
    map_ptr.* = std.StringHashMap(Value).init(self.allocator);
    try map_ptr.ensureTotalCapacity(len);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        const value = self.value_stack.pop();
        const key = self.value_stack.pop();
        map_ptr.putAssumeCapacity(key.ty.string, value);
    }

    try self.value_stack.append(Value.new(.{ .map = map_ptr }, offset));
}
fn execSubscript(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const container = self.value_stack.pop();

    if (container.ty != .list and container.ty != .map)
        return self.ctx.err("{s}[] ?", .{@tagName(container.ty)}, error.InvalidSubscript, offset);

    return if (container.ty == .list)
        try self.execSubscriptList(container, offset)
    else
        try self.execSubscriptMap(container, offset);
}
fn execSubscriptList(self: *Vm, list: Value, offset: u16) !void {
    const index = self.value_stack.pop();
    if (index.ty != .uint and index.ty != .range) return self.ctx.err("list[{s}] ?", .{@tagName(index.ty)}, error.InvalidSubscript, offset);

    if (index.ty == .uint) {
        if (index.ty.uint >= list.ty.list.items.len) return self.ctx.err("Index out of bounds.", .{}, error.InvalidSubscript, offset);
        try self.value_stack.append(list.ty.list.items[index.ty.uint]);
    } else {
        if (index.ty.range[1] > list.ty.list.items.len) return self.ctx.err("Index out of bounds.", .{}, error.InvalidSubscript, offset);

        var new_list_ptr = try self.allocator.create(std.ArrayList(Value));
        new_list_ptr.* = try std.ArrayList(Value).initCapacity(self.allocator, index.ty.range[1] - index.ty.range[0]);
        for (list.ty.list.items[index.ty.range[0]..index.ty.range[1]]) |item|
            new_list_ptr.appendAssumeCapacity(item); //TODO: Copy here?

        try self.value_stack.append(Value.new(.{ .list = new_list_ptr }, offset));
    }
}
fn execSubscriptMap(self: *Vm, map: Value, offset: u16) !void {
    const key = self.value_stack.pop();
    if (key.ty != .string) return self.ctx.err("map[{s}] ?", .{@tagName(key.ty)}, error.InvalidSubscript, offset);
    const value = if (map.ty.map.get(key.ty.string)) |v| v else Value.new(.nil, offset);
    try self.value_stack.append(value);
}
fn execSet(self: *Vm) !void {
    const container = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    // Combo assign
    const combo = @intToEnum(Node.Combo, self.instructions[self.ip.*]);
    self.ip.* += 1;

    if (container.ty != .list and container.ty != .map)
        return self.ctx.err("{s}[]= ?", .{@tagName(container.ty)}, error.InvalidSubscript, offset);

    return if (container.ty == .list)
        try self.execSetList(container, offset, combo)
    else
        try self.execSetMap(container, offset, combo);
}
fn execSetList(self: *Vm, list: Value, offset: u16, combo: Node.Combo) !void {
    const index = self.value_stack.pop();
    if (index.ty != .uint) return self.ctx.err("list[{s}]= ?", .{@tagName(index.ty)}, error.InvalidSubscript, offset);
    if (index.ty.uint >= list.ty.list.items.len) return self.ctx.err("Index out of bounds.", .{}, error.InvalidSubscript, offset);
    const rvalue = self.value_stack.pop();

    // Store
    if (combo == .none) {
        list.ty.list.items[index.ty.uint] = rvalue; //TODO: Deinit old value?
        try self.value_stack.append(rvalue);
    } else {
        const old_value = list.ty.list.items[index.ty.uint];

        const new_value = switch (combo) {
            .none => unreachable,
            .add => try old_value.add(rvalue),
            .sub => try old_value.sub(rvalue),
            .mul => try old_value.mul(rvalue),
            .div => try old_value.div(rvalue),
            .mod => try old_value.mod(rvalue),
        };

        list.ty.list.items[index.ty.uint] = new_value;
        try self.value_stack.append(new_value);
    }
}
fn execSetMap(self: *Vm, map: Value, offset: u16, combo: Node.Combo) !void {
    const key = self.value_stack.pop();
    if (key.ty != .string) return self.ctx.err("map[{s}]= ?", .{@tagName(key.ty)}, error.InvalidSubscript, offset);
    const rvalue = self.value_stack.pop();

    // Store
    if (combo == .none) {
        try map.ty.map.put(key.ty.string, rvalue); //TODO: Deinit old value?
        try self.value_stack.append(rvalue);
    } else {
        const old_value = map.ty.map.get(key.ty.string) orelse Value.new(.{ .uint = 0 }, offset);

        const new_value = switch (combo) {
            .none => unreachable,
            .add => try old_value.add(rvalue),
            .sub => try old_value.sub(rvalue),
            .mul => try old_value.mul(rvalue),
            .div => try old_value.div(rvalue),
            .mod => try old_value.mod(rvalue),
        };

        try map.ty.map.put(key.ty.string, new_value);
        try self.value_stack.append(new_value);
    }
}

fn execJump(self: *Vm) void {
    self.ip.* += 1;
    const index = self.getU16(self.ip.*);
    self.ip.* = index;
}
fn execJumpFalse(self: *Vm) void {
    const condition = self.value_stack.pop();
    if (!isTruthy(condition)) self.execJump() else self.ip.* += 3;
}
fn execJumpTrue(self: *Vm) void {
    const condition = self.value_stack.pop();
    if (isTruthy(condition)) self.execJump() else self.ip.* += 3;
}

fn execRange(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const inclusive = self.instructions[self.ip.*] == 1;
    self.ip.* += 1;

    const to = self.value_stack.pop();
    const from = self.value_stack.pop();

    if (from.ty != .uint or to.ty != .uint) return self.ctx.err("Invalid range.", .{}, error.InvalidRange, offset);

    const from_uint = from.ty.uint;
    const to_uint = if (inclusive) to.ty.uint + 1 else to.ty.uint;

    try self.value_stack.append(Value.new(.{ .range = [2]usize{ from_uint, to_uint } }, offset));
}

fn execRecRange(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const range_id = self.instructions[self.ip.*];
    self.ip.* += 1;
    const exclusive = self.instructions[self.ip.*] == 1;
    self.ip.* += 1;

    const len = self.getU16(self.ip.*);
    self.ip.* += 2;
    const action_instructions: []const u8 = if (len != 0) self.instructions[self.ip.* .. self.ip.* + len] else "";
    self.ip.* += len;

    const has_from = self.instructions[self.ip.*] == 1;
    self.ip.* += 1;
    const has_to = self.instructions[self.ip.*] == 1;
    self.ip.* += 1;

    const nil_value = Value.new(.nil, offset);
    const from = if (has_from) self.value_stack.pop() else nil_value;
    const to = if (has_to) self.value_stack.pop() else nil_value;

    var result = nil_value;
    var eval_action = false;

    if (self.scope_stack.stack.items[0].rec_ranges.contains(range_id)) {
        // In range
        eval_action = true;

        if (isTruthy(to)) {
            // Range end.
            _ = self.scope_stack.stack.items[0].rec_ranges.remove(range_id);
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
            const rnum = self.scope_stack.stack.items[0].map.get("@rnum").?;
            start_range = rnum.ty.uint == 1;
        }

        if (start_range) {
            // We start a new range.
            try self.scope_stack.stack.items[0].rec_ranges.put(range_id, {});
            eval_action = true;
        }
    }

    if (eval_action) {
        if (len != 0) {
            var vm = try init(
                self.allocator,
                action_instructions,
                self.scope_stack,
                self.ctx,
                self.output,
            );
            try vm.run();

            result = vm.last_popped;
        } else {
            // Default action
            var writer = self.output.writer();
            _ = try writer.print("{s}", .{self.scope_stack.stack.items[0].record});
        }
    }

    try self.value_stack.append(result);
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
    try self.scope_stack.push(scope);
}

fn popScope(self: *Vm) Scope {
    std.debug.assert(self.scope_stack.stack.items.len > 1);
    var old_scope = self.scope_stack.pop();
    return old_scope;
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

fn getName(self: Vm) []const u8 {
    var name: []const u8 = undefined;
    if (@intToEnum(Compiler.Opcode, self.instructions[self.ip.*]) == .ident) {
        self.ip.* += 1;
        const name_len = self.getU16(self.ip.*);
        self.ip.* += 2;
        name = self.getString(self.ip.*, name_len);
        self.ip.* += name_len;
    } else {
        self.ip.* += 1;
        const start = self.getU16(self.ip.*);
        self.ip.* += 2;
        const name_len = self.getU16(self.ip.*);
        self.ip.* += 2;
        name = self.getString(start, name_len);
    }

    return name;
}

fn getNumber(self: Vm, comptime T: type, start: usize, n: usize) T {
    return std.mem.bytesAsSlice(T, self.instructions[start .. start + n])[0];
}

fn getOffset(self: Vm) u16 {
    return self.getU16(self.ip.*);
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

    const ctx = Context{ .filename = "inline", .src = input };

    var lexer = Lexer{ .allocator = allocator, .ctx = ctx };
    var tokens = try lexer.lex();

    var parser = Parser{
        .allocator = allocator,
        .ctx = ctx,
        .tokens = tokens,
    };
    const program = try parser.parse();

    var compiler = try Compiler.init(allocator);
    for (program.rules) |n| try compiler.compile(n);

    var scope_stack = ScopeStack.init(allocator);
    _ = try scope_stack.push(Scope.init(allocator, .block));

    var output = std.ArrayList(u8).init(allocator);

    var vm = try init(
        allocator,
        compiler.instructions.items,
        scope_stack,
        ctx,
        &output,
    );
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

test "Vm function literal" {
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

test "Vm function call / define, store, load" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\f := { a := it; return a }
        \\r := f(42)
        \\r
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 42), got.ty.uint);
}

test "Vm combo assign" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\f := 1
        \\f += 2
        \\f
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 3), got.ty.uint);
}

test "Vm infix" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "1 + 2 * 3 / 2 % 2");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 2), got.ty.uint);

    got = try testVmValue(allocator, "1 + 2 * 3 / 2 % 2 == 2");
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(true, got.ty.boolean);

    got = try testVmValue(allocator, "(1 + 2) * 3 / 2 % 2 > 2");
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(false, got.ty.boolean);
}

test "Vm prefix" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "foo := 42; -foo");
    try std.testing.expectEqual(Value.Tag.int, got.ty);
    try std.testing.expectEqual(@as(i64, -42), got.ty.int);

    got = try testVmValue(allocator, "!true");
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(false, got.ty.boolean);
}

test "Vm list literal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "[1, 2, 3]");
    try std.testing.expectEqual(Value.Tag.list, got.ty);
    try std.testing.expectEqual(@as(usize, 3), got.ty.list.items.len);
    try std.testing.expectEqual(Value.Tag.uint, got.ty.list.items[0].ty);
    try std.testing.expectEqual(@as(u64, 1), got.ty.list.items[0].ty.uint);
}

test "Vm map literal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\["a": 1, "b": 2]
    );
    try std.testing.expectEqual(Value.Tag.map, got.ty);
    try std.testing.expectEqual(@as(usize, 2), got.ty.map.count());
    try std.testing.expectEqual(Value.Tag.uint, got.ty.map.get("a").?.ty);
    try std.testing.expectEqual(@as(u64, 1), got.ty.map.get("a").?.ty.uint);
}

test "Vm subscripts" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\["a": 1, "b": 2]["b"] + [1, 2, 3][1]
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 4), got.ty.uint);
    try std.testing.expectEqual(@as(u16, 14), got.offset);
}

test "Vm subscript assign" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\m := ["a": 1, "b": 2]
        \\m["b"] += 2
        \\m["c"] += m["b"]
        \\l := [1, 2, 3]
        \\l[1] = 3
        \\m["c"] + l[1]
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 7), got.ty.uint);
    try std.testing.expectEqual(@as(u16, 41), got.offset);
}

test "Vm conditionals" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\a := if (false) 1 else 0
        \\a ?= 1
        \\b := a ?: 3
        \\c := b ? 4 : 5
        \\a + b + c
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 6), got.ty.uint);
}

test "Vm while loop" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\i := 0
        \\while (i < 9) {
        \\  i += 1
        \\}
        \\i
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 9), got.ty.uint);
}

test "Vm do while loop" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\i := 10
        \\do {
        \\  i += 1
        \\} while (i < 10)
        \\i
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 11), got.ty.uint);
}

test "Vm loop break" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\i := 0
        \\while (i < 9) {
        \\  i += 1
        \\  if (i == 4) break
        \\}
        \\i
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 4), got.ty.uint);
}

test "Vm loop continue" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\i := 0
        \\total := 0
        \\while (i < 4) {
        \\  i += 1
        \\  if (i == 2) continue
        \\  total += i
        \\}
        \\total
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 8), got.ty.uint);
}

test "Vm fibonacci" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\fib := {
        \\  a := 0
        \\  b := 1
        \\  i := 0
        \\
        \\  while (i < it) {
        \\      tmp := a
        \\      a = b
        \\      b = tmp + a
        \\      i += 1
        \\  }
        \\
        \\  return a
        \\}
        \\fib(7)
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 13), got.ty.uint);
}

test "Vm recursive fibonacci" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\fib := {
        \\  if (it < 2) return it
        \\  return fib(it - 1) + fib(it - 2)
        \\}
        \\fib(7)
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 13), got.ty.uint);
}

test "Vm list range subscript" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\[1, 2, 3, 4, 5][2..<5][1]
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 4), got.ty.uint);
}
