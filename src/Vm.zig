const std = @import("std");

const Compiler = @import("Compiler.zig");
const Context = @import("Context.zig");
const GraphemeIterator = @import("ziglyph").GraphemeIterator;
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

scope_stack: *ScopeStack,
frame_stack: std.ArrayList(Frame),
value_stack: std.ArrayList(Value),

const Vm = @This();

pub fn init(
    allocator: std.mem.Allocator,
    instructions: []const u8,
    scope_stack: *ScopeStack,
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
            .concat => try self.execConcat(),
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
            // Output redirection
            .redir => try self.execRedir(),
            // Printing
            .sprint => try self.execSprint(),

            else => {
                std.log.err("{s}", .{@tagName(opcode)}); //TODO: Remove this.
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

    if (callee.ty == .builtin) {
        return switch (callee.ty.builtin) {
            .atan2 => try self.atan2(),
            .chars => try self.strChars(),
            .contains => try self.contains(),
            .cos => try self.oneArgMath(callee),
            .each => try self.each(),
            .endsWith => try self.strEndsWith(),
            .exp => try self.oneArgMath(callee),
            .filter => try self.listFilter(),
            .join => try self.listJoin(),
            .indexOf => try self.indexOf(),
            .int => try self.oneArgMath(callee),
            .keys => try self.mapKeys(),
            .lastIndexOf => try self.lastIndexOf(),
            .len => try self.length(),
            .log => try self.oneArgMath(callee),
            .map => try self.listMap(),
            .max => try self.listMax(),
            .mean => try self.listMean(),
            .median => try self.listMedian(),
            .min => try self.listMin(),
            .mode => try self.listMode(),
            .print => try self.execPrint(),
            .pop => try self.listPop(),
            .push => try self.listPush(),
            .rand => try self.rand(),
            .reduce => try self.listReduce(),
            .reverse => try self.listReverse(),
            .sin => try self.oneArgMath(callee),
            .sort => try self.listSort(),
            .split => try self.strSplit(),
            .sqrt => try self.oneArgMath(callee),
            .startsWith => try self.strStartsWith(),
            .stdev => try self.listStdev(),
            .values => try self.mapValues(),
        };
    }

    if (callee.ty != .func) return self.ctx.err(
        "{s} is not callable.",
        .{@tagName(callee.ty)},
        error.InvalidCall,
        callee.offset,
    );

    // Prepare the child scope.
    var func_scope = Scope.init(self.allocator, .function);

    // Self-references
    if (callee.ty.func.name.len != 0) try func_scope.map.put(callee.ty.func.name, callee);

    // Process args
    self.ip.* += 3;
    const num_args = self.instructions[self.ip.*];

    var i: usize = 0;
    while (i < num_args) : (i += 1) {
        const arg = self.value_stack.pop();
        if (i == 0) try func_scope.map.put("it", arg); // it
        var buf: [4]u8 = undefined;
        const auto_arg_name = try std.fmt.bufPrint(&buf, "@{}", .{i});
        try func_scope.map.put(try self.allocator.dupe(u8, auto_arg_name), arg); // @0, @1, ...
        if (i < callee.ty.func.params.len) try func_scope.map.put(callee.ty.func.params[i], arg);
    }

    // Push the function's frame.
    try self.pushScope(func_scope);
    try self.pushFrame(callee.ty.func.instructions);

    // NOTE: Final self.ip.* += 1 is done on return.
}

fn execDefine(self: *Vm) !void {
    // Offset
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    // Name
    const name = self.getName();
    // Is it already defined?
    if (self.scope_stack.isDefined(name)) return self.ctx.err(
        "{s} already defined.",
        .{name},
        error.NameAlreadyDefined,
        offset,
    );
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
    if (!self.scope_stack.isDefined(name)) return self.ctx.err(
        "{s} is undefined.",
        .{name},
        error.NameUndefined,
        offset,
    );
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
    if (!self.scope_stack.isDefined(name)) return self.ctx.err(
        "{s} is undefined.",
        .{name},
        error.NameUndefined,
        offset,
    );
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
fn execConcat(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (left.ty != .string or right.ty != .string) return self.ctx.err(
        "{s} ++ {s} ?",
        .{ @tagName(left.ty), @tagName(right.ty) },
        error.InvalidConcat,
        offset,
    );

    var buf = try self.allocator.alloc(u8, left.ty.string.len + right.ty.string.len);
    std.mem.copy(u8, buf, left.ty.string);
    std.mem.copy(u8, buf[left.ty.string.len..], right.ty.string);
    try self.value_stack.append(Value.new(.{ .string = buf }, offset));
}

fn execNot(self: *Vm) !void {
    const value = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    if (value.ty != .boolean) return self.ctx.err(
        "!{s} ?",
        .{@tagName(value.ty)},
        error.InvalidNot,
        offset,
    );
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
        else => return self.ctx.err(
            "-{s} ?",
            .{@tagName(value.ty)},
            error.InvalidNeg,
            offset,
        ),
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
        return self.ctx.err(
            "{s}[] ?",
            .{@tagName(container.ty)},
            error.InvalidSubscript,
            offset,
        );

    return if (container.ty == .list)
        try self.execSubscriptList(container, offset)
    else
        try self.execSubscriptMap(container, offset);
}
fn execSubscriptList(self: *Vm, list: Value, offset: u16) !void {
    const index = self.value_stack.pop();
    if (index.ty != .uint and index.ty != .range) return self.ctx.err(
        "list[{s}] ?",
        .{@tagName(index.ty)},
        error.InvalidSubscript,
        offset,
    );

    if (index.ty == .uint) {
        if (index.ty.uint >= list.ty.list.items.len) return self.ctx.err(
            "Index out of bounds.",
            .{},
            error.InvalidSubscript,
            offset,
        );
        try self.value_stack.append(list.ty.list.items[index.ty.uint]);
    } else {
        if (index.ty.range[1] > list.ty.list.items.len) return self.ctx.err(
            "Index out of bounds.",
            .{},
            error.InvalidSubscript,
            offset,
        );

        var new_list_ptr = try self.allocator.create(std.ArrayList(Value));
        new_list_ptr.* = try std.ArrayList(Value).initCapacity(self.allocator, index.ty.range[1] - index.ty.range[0]);
        for (list.ty.list.items[index.ty.range[0]..index.ty.range[1]]) |item|
            new_list_ptr.appendAssumeCapacity(item); //TODO: Copy here?

        try self.value_stack.append(Value.new(.{ .list = new_list_ptr }, offset));
    }
}
fn execSubscriptMap(self: *Vm, map: Value, offset: u16) !void {
    const key = self.value_stack.pop();
    if (key.ty != .string) return self.ctx.err(
        "map[{s}] ?",
        .{@tagName(key.ty)},
        error.InvalidSubscript,
        offset,
    );
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
        return self.ctx.err(
            "{s}[]= ?",
            .{@tagName(container.ty)},
            error.InvalidSubscript,
            offset,
        );

    return if (container.ty == .list)
        try self.execSetList(container, offset, combo)
    else
        try self.execSetMap(container, offset, combo);
}
fn execSetList(self: *Vm, list: Value, offset: u16, combo: Node.Combo) !void {
    const index = self.value_stack.pop();
    if (index.ty != .uint) return self.ctx.err(
        "list[{s}]= ?",
        .{@tagName(index.ty)},
        error.InvalidSubscript,
        offset,
    );
    if (index.ty.uint >= list.ty.list.items.len) return self.ctx.err(
        "Index out of bounds.",
        .{},
        error.InvalidSubscript,
        offset,
    );
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
    if (key.ty != .string) return self.ctx.err(
        "map[{s}]= ?",
        .{@tagName(key.ty)},
        error.InvalidSubscript,
        offset,
    );
    const rvalue = self.value_stack.pop();
    const key_copy = try map.ty.map.allocator.dupe(u8, key.ty.string);

    // Store
    if (combo == .none) {
        try map.ty.map.put(key_copy, try rvalue.copy(map.ty.map.allocator)); //TODO: Deinit old value?
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

        try map.ty.map.put(key_copy, try new_value.copy(map.ty.map.allocator));
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

    if (from.ty != .uint or to.ty != .uint) return self.ctx.err(
        "Invalid range.",
        .{},
        error.InvalidRange,
        offset,
    );

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

    if (self.scope_stack.rec_ranges.contains(range_id)) {
        // In range
        eval_action = true;

        if (isTruthy(to)) {
            // Range end.
            _ = self.scope_stack.rec_ranges.remove(range_id);
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
            const rnum = self.scope_stack.rnum;
            start_range = rnum == 1;
        }

        if (start_range) {
            // We start a new range.
            try self.scope_stack.rec_ranges.put(range_id, {});
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
            _ = try writer.print("{s}", .{self.scope_stack.record});
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

// Builtins

fn atan2(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    // Get args count.
    const num_args = self.instructions[self.ip.*];
    self.ip.* += 1;

    if (num_args != 2) return self.ctx.err(
        "atan2 requires 2 arguments.",
        .{},
        error.InvalidAtan2,
        offset,
    );

    const y_val = self.value_stack.pop();
    const y = y_val.asFloat() orelse return self.ctx.err(
        "atan2 y not convertible to float.",
        .{},
        error.InvalidAtan2,
        y_val.offset,
    );

    const x_val = self.value_stack.pop();
    const x = x_val.asFloat() orelse return self.ctx.err(
        "atan2 x not convertible to float.",
        .{},
        error.InvalidAtan2,
        x_val.offset,
    );

    const result = Value.new(.{ .float = std.math.atan2(f64, y.ty.float, x.ty.float) }, offset);
    try self.value_stack.append(result);
}
fn strChars(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const str = self.value_stack.pop();
    if (str.ty != .string) return self.ctx.err(
        "{s}.chars() ?",
        .{@tagName(str.ty)},
        error.InvalidCharsCall,
        str.offset,
    );

    var list_ptr = try self.allocator.create(std.ArrayList(Value));
    list_ptr.* = std.ArrayList(Value).init(self.allocator);

    var giter = GraphemeIterator.init(str.ty.string) catch |err| return self.ctx.err(
        "Unicode error.",
        .{},
        err,
        str.offset,
    );
    while (giter.next()) |grapheme| try list_ptr.append(Value.new(.{ .string = grapheme.bytes }, offset));

    try self.value_stack.append(Value.new(.{ .list = list_ptr }, offset));
    self.ip.* += 1;
}
fn execPrint(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    // Get args count.
    const num_args = self.instructions[self.ip.*];
    self.ip.* += 1;

    var writer = self.output.writer();
    var i: usize = 0;
    while (i < num_args) : (i += 1) {
        if (i != 0) try writer.writeAll(self.scope_stack.ofs);
        _ = try writer.print("{}", .{self.value_stack.pop()});
    }

    try self.value_stack.append(Value.new(.nil, offset));
}
fn execSprint(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    // Get args count.
    const num_args = self.instructions[self.ip.*];
    self.ip.* += 1;

    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();

    var i: usize = 0;
    while (i < num_args) : (i += 1) {
        if (i != 0) try writer.writeAll(self.scope_stack.ofs);
        _ = try writer.print("{}", .{self.value_stack.pop()});
    }

    try self.value_stack.append(Value.new(.{ .string = buf.items }, offset));
}
fn oneArgMath(self: *Vm, builtin: Value) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    // Get args count.
    const num_args = self.instructions[self.ip.*];
    self.ip.* += 1;

    if (num_args != 1) return self.ctx.err(
        "Math builtin call requires one argument.",
        .{},
        error.InvalidMathBuiltin,
        offset,
    );

    const x_val = self.value_stack.pop();
    const x = x_val.asFloat() orelse return self.ctx.err(
        "Arg not convertible to float.",
        .{},
        error.InvalidArg,
        x_val.offset,
    );

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
}
fn contains(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const haystack = self.value_stack.pop();
    const needle = self.value_stack.pop();
    if (haystack.ty != .list and
        haystack.ty != .map and
        haystack.ty != .string)
        return self.ctx.err(
            "contains not allowed on {s}.",
            .{@tagName(haystack.ty)},
            error.InvalidContains,
            offset,
        );

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
            if (needle.ty != .string) return self.ctx.err(
                "contains arg on strings must be a string.",
                .{},
                error.InvalidContains,
                offset,
            );

            break :str Value.new(.{ .boolean = std.mem.containsAtLeast(u8, s, 1, needle.ty.string) }, offset);
        },
        else => unreachable,
    };

    try self.value_stack.append(result);
    self.ip.* += 1;
}
fn indexOf(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const haystack = self.value_stack.pop();
    const needle = self.value_stack.pop();
    if (haystack.ty != .list and haystack.ty != .string) return self.ctx.err(
        "indexOf not allowed on {s}.",
        .{@tagName(haystack.ty)},
        error.InvalidIndexOf,
        offset,
    );

    const result = switch (haystack.ty) {
        .list => |l| for (l.items) |item, i| {
            if (needle.eql(item)) break Value.new(.{ .uint = i }, offset);
        } else Value.new(.nil, offset),
        .string => |s| str: {
            if (needle.ty != .string) return self.ctx.err(
                "indexOf arg on strings must be a string.",
                .{},
                error.InvalidIndexOf,
                offset,
            );

            var giter = try GraphemeIterator.init(s);
            var i: usize = 0;
            break :str while (giter.next()) |grapheme| : (i += 1) {
                if (std.mem.eql(u8, needle.ty.string, grapheme.bytes)) break Value.new(.{ .uint = i }, offset);
            } else Value.new(.nil, offset);
        },
        else => unreachable,
    };

    try self.value_stack.append(result);
    self.ip.* += 1;
}
fn lastIndexOf(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const haystack = self.value_stack.pop();
    const needle = self.value_stack.pop();
    if (haystack.ty != .list and haystack.ty != .string) return self.ctx.err(
        "lastIndexOf not allowed on {s}.",
        .{@tagName(haystack.ty)},
        error.InvalidLastIndexOf,
        offset,
    );

    const result = switch (haystack.ty) {
        .list => |l| lst: {
            var i: usize = 0;
            const len = l.items.len;
            break :lst while (i <= len) : (i += 1) {
                if (needle.eql(l.items[len - i])) break Value.new(.{ .uint = i }, offset);
            } else Value.new(.nil, offset);
        },
        .string => |s| str: {
            if (needle.ty != .string) return self.ctx.err(
                "lastIndexOf arg on strings must be a string.",
                .{},
                error.InvalidLastIndexOf,
                offset,
            );

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
    self.ip.* += 1;
}
fn length(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const value = self.value_stack.pop();
    if (value.ty != .list and
        value.ty != .map and
        value.ty != .string)
        return self.ctx.err(
            "len not allowed on {s}.",
            .{@tagName(value.ty)},
            error.InvalidLen,
            offset,
        );

    const result = switch (value.ty) {
        .list => |l| Value.new(.{ .uint = l.items.len }, offset),
        .map => |m| Value.new(.{ .uint = m.count() }, offset),
        .string => |s| Value.new(.{ .uint = s.len }, offset),
        else => unreachable,
    };

    try self.value_stack.append(result);
    self.ip.* += 1;
}
fn mapKeys(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const m = self.value_stack.pop();
    if (m.ty != .map) return self.ctx.err(
        "keys not allowed on {s}.",
        .{@tagName(m.ty)},
        error.InvalidKeys,
        offset,
    );

    const keys_ptr = try self.allocator.create(std.ArrayList(Value));
    keys_ptr.* = if (m.ty.map.count() == 0)
        std.ArrayList(Value).init(self.allocator)
    else
        try std.ArrayList(Value).initCapacity(self.allocator, m.ty.map.count());
    var key_iter = m.ty.map.keyIterator();
    while (key_iter.next()) |key| keys_ptr.appendAssumeCapacity(Value.new(.{ .string = key.* }, offset));

    try self.value_stack.append(Value.new(.{ .list = keys_ptr }, offset));
    self.ip.* += 1;
}
fn mapValues(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const m = self.value_stack.pop();
    if (m.ty != .map) return self.ctx.err(
        "values not allowed on {s}.",
        .{@tagName(m.ty)},
        error.InvalidValues,
        offset,
    );

    const values_ptr = try self.allocator.create(std.ArrayList(Value));
    values_ptr.* = if (m.ty.map.count() == 0)
        std.ArrayList(Value).init(self.allocator)
    else
        try std.ArrayList(Value).initCapacity(self.allocator, m.ty.map.count());
    var value_iter = m.ty.map.valueIterator();
    while (value_iter.next()) |value| values_ptr.appendAssumeCapacity(value.*);

    try self.value_stack.append(Value.new(.{ .list = values_ptr }, offset));
    self.ip.* += 1;
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
fn listMean(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "mean not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMean,
        offset,
    );

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.{ .float = 0 }, offset));
        self.ip.* += 1;
        return;
    }

    try self.value_stack.append(Value.new(.{ .float = listMeanHelper(l.ty.list.*) }, offset));
    self.ip.* += 1;
}
fn listMedian(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "median not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMedian,
        offset,
    );

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.{ .float = 0 }, offset));
        self.ip.* += 1;
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
    self.ip.* += 1;
}
fn listMode(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "mode not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMode,
        offset,
    );

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil, offset));
        self.ip.* += 1;
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
    self.ip.* += 1;
}
fn listStdev(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "stdev not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidStdev,
        offset,
    );

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.{ .float = 0 }, offset));
        self.ip.* += 1;
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
    self.ip.* += 1;
}
fn listMin(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "min not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMin,
        offset,
    );

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil, offset));
        self.ip.* += 1;
        return;
    }

    var min = l.ty.list.items[0];
    for (l.ty.list.items) |item| {
        const comparison = try min.cmp(item);
        if (comparison == .gt) min = item;
    }

    try self.value_stack.append(min);
    self.ip.* += 1;
}
fn listMax(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "max not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMax,
        offset,
    );

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil, offset));
        self.ip.* += 1;
        return;
    }

    var max = l.ty.list.items[0];
    for (l.ty.list.items) |item| {
        const comparison = try max.cmp(item);
        if (comparison == .lt) max = item;
    }

    try self.value_stack.append(max);
    self.ip.* += 1;
}
fn listSort(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "sort not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidSort,
        offset,
    );

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(l);
        self.ip.* += 1;
        return;
    }

    std.sort.sort(Value, l.ty.list.items, {}, Value.lessThan);
    try self.value_stack.append(l);
    self.ip.* += 1;
}
fn listReverse(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "reverse not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidReverse,
        offset,
    );

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(l);
        self.ip.* += 1;
        return;
    }

    std.mem.reverse(Value, l.ty.list.items);
    try self.value_stack.append(l);
    self.ip.* += 1;
}
fn strSplit(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const str = self.value_stack.pop();
    if (str.ty != .string) return self.ctx.err(
        "split not allowed on {s}.",
        .{@tagName(str.ty)},
        error.InvalidSplit,
        offset,
    );

    const delim = self.value_stack.pop();
    if (delim.ty != .string) return self.ctx.err(
        "split delimiter must be a string",
        .{},
        error.InvalidSplit,
        offset,
    );

    var list_ptr = try self.allocator.create(std.ArrayList(Value));
    list_ptr.* = std.ArrayList(Value).init(self.allocator);
    var iter = std.mem.split(u8, str.ty.string, delim.ty.string);
    while (iter.next()) |sub| try list_ptr.append(Value.new(.{ .string = sub }, 0));

    try self.value_stack.append(Value.new(.{ .list = list_ptr }, offset));
    self.ip.* += 1;
}
fn listJoin(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "join not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidJoin,
        offset,
    );

    const delim = self.value_stack.pop();
    if (delim.ty != .string) return self.ctx.err(
        "join delimiter must be a string",
        .{},
        error.InvalidJoin,
        offset,
    );

    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();

    for (l.ty.list.items) |item, i| {
        if (i != 0 and delim.ty.string.len > 0) try buf.appendSlice(delim.ty.string);
        _ = try writer.print("{}", .{item});
    }

    try self.value_stack.append(Value.new(.{ .string = buf.items }, offset));
    self.ip.* += 1;
}
fn strEndsWith(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const str = self.value_stack.pop();
    const ending = self.value_stack.pop();
    if (str.ty != .string or ending.ty != .string) return self.ctx.err(
        "endsWith callee and arg must be strings.",
        .{},
        error.InvalidEndsWith,
        offset,
    );

    const result = Value.new(.{ .boolean = std.mem.endsWith(u8, str.ty.string, ending.ty.string) }, offset);

    try self.value_stack.append(result);
    self.ip.* += 1;
}
fn strStartsWith(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const str = self.value_stack.pop();
    const start = self.value_stack.pop();
    if (str.ty != .string or start.ty != .string) return self.ctx.err(
        "startsWith callee and arg must be strings.",
        .{},
        error.InvalidStartsWith,
        offset,
    );

    const result = Value.new(.{ .boolean = std.mem.startsWith(u8, str.ty.string, start.ty.string) }, offset);
    try self.value_stack.append(result);

    self.ip.* += 1;
}
fn listMap(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "map not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMap,
        offset,
    );

    const f = self.value_stack.pop();
    if (f.ty != .func) return self.ctx.err(
        "map requres function argument.",
        .{},
        error.InvalidMap,
        offset,
    );

    var list_ptr = try self.allocator.create(std.ArrayList(Value));
    list_ptr.* = try std.ArrayList(Value).initCapacity(self.allocator, l.ty.list.items.len);

    for (l.ty.list.items) |item, i| {
        const v = try self.evalListPredicate(f, item, i);
        list_ptr.appendAssumeCapacity(v);
    }

    try self.value_stack.append(Value.new(.{ .list = list_ptr }, offset));
    self.ip.* += 1;
}
fn listFilter(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "filter not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidFilter,
        offset,
    );

    const f = self.value_stack.pop();
    if (f.ty != .func) return self.ctx.err(
        "filter requires function argument",
        .{},
        error.InvalidFilter,
        offset,
    );

    var list_ptr = try self.allocator.create(std.ArrayList(Value));
    list_ptr.* = std.ArrayList(Value).init(self.allocator);

    for (l.ty.list.items) |item, i| {
        const v = try self.evalListPredicate(f, item, i);
        if (isTruthy(v)) try list_ptr.append(item);
    }

    try self.value_stack.append(Value.new(.{ .list = list_ptr }, offset));
    self.ip.* += 1;
}
fn each(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const container = self.value_stack.pop();
    if (container.ty != .list and container.ty != .map) return self.ctx.err(
        "each not allowed on {s}.",
        .{@tagName(container.ty)},
        error.InvalidEach,
        offset,
    );

    const f = self.value_stack.pop();
    if (f.ty != .func) return self.ctx.err(
        "each requres function argument.",
        .{},
        error.InvalidEach,
        offset,
    );

    const container_len = switch (container.ty) {
        .list => |l| l.items.len,
        .map => |m| m.count(),
        else => unreachable,
    };

    if (container_len == 0) {
        try self.value_stack.append(container);
        self.ip.* += 1;
        return;
    }

    if (container.ty == .list) {
        for (container.ty.list.items) |item, i| _ = try self.evalListPredicate(f, item, i);
    } else {
        var iter = container.ty.map.iterator();
        var i: usize = 0;
        while (iter.next()) |entry| : (i += 1) _ = try self.evalMapPredicate(f, entry.key_ptr.*, entry.value_ptr.*, i);
    }

    try self.value_stack.append(container);
    self.ip.* += 1;
}
fn listReduce(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "reduce not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidReduce,
        offset,
    );

    var acc = self.value_stack.pop();
    const f = self.value_stack.pop();
    if (f.ty != .func) return self.ctx.err(
        "reduce requires function argument.",
        .{},
        error.InvalidReduce,
        offset,
    );

    if (l.ty.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil, offset));
        self.ip.* += 1;
        return;
    }

    // Set up sub-VM arena.
    var vm_arena = std.heap.ArenaAllocator.init(self.allocator);
    defer vm_arena.deinit();
    const vm_allocator = vm_arena.allocator();

    for (l.ty.list.items) |item, i| {
        // Set up function scope.
        var func_scope = Scope.init(vm_allocator, .function);

        // Assign args as locals in function scope.
        try func_scope.map.put("acc", acc);
        try func_scope.map.put("it", item);
        try func_scope.map.put("@0", item);
        if (f.ty.func.params.len > 0) try func_scope.map.put(f.ty.func.params[0], acc);
        if (f.ty.func.params.len > 1) try func_scope.map.put(f.ty.func.params[1], item);
        try func_scope.map.put("index", Value.new(.{ .uint = i }, 0));

        _ = try self.pushScope(func_scope);

        var vm = try init(
            vm_allocator,
            f.ty.func.instructions,
            self.scope_stack,
            self.ctx,
            self.output,
        );
        try vm.run();

        _ = self.popScope();

        acc = vm.last_popped;
    }

    try self.value_stack.append(acc);
    self.ip.* += 1;
}
fn rand(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    // Get args count.
    const num_args = self.instructions[self.ip.*];
    self.ip.* += 1;
    if (num_args != 1) return self.ctx.err(
        "rand requires a single argument.",
        .{},
        error.InvalidRand,
        offset,
    );

    const x_val = self.value_stack.pop();
    if (x_val.ty != .uint) return self.ctx.err(
        "rand argument must be unsigned integer.",
        .{},
        error.InvalidRand,
        offset,
    );

    const result = Value.new(.{ .uint = std.rand.DefaultPrng.init(@intCast(usize, std.time.timestamp())).random().uintAtMost(usize, x_val.ty.uint) }, offset);
    try self.value_stack.append(result);
}
fn listPush(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "push not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidPush,
        offset,
    );

    var item = self.value_stack.pop();
    try l.ty.list.append(try item.copy(l.ty.list.allocator));
    try self.value_stack.append(l);

    self.ip.* += 1;
}
fn listPop(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .list) return self.ctx.err(
        "pop not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidPop,
        offset,
    );

    try self.value_stack.append(l.ty.list.pop()); //TODO: Deinit popped value?
    self.ip.* += 1;
}

fn evalListPredicate(self: *Vm, func: Value, item: Value, index: usize) anyerror!Value {
    // Assign args as locals in function scope.
    var func_scope = Scope.init(self.allocator, .function);

    const index_val = Value.new(.{ .uint = index }, 0);

    try func_scope.map.put("it", item);
    try func_scope.map.put("index", index_val);

    if (func.ty.func.params.len > 0) try func_scope.map.put(func.ty.func.params[0], item);
    if (func.ty.func.params.len > 1) try func_scope.map.put(func.ty.func.params[1], index_val);

    return self.evalPredicate(func.ty.func.instructions, func_scope);
}

fn evalMapPredicate(self: *Vm, func: Value, key: []const u8, item: Value, index: usize) anyerror!Value {
    // Assign args as locals in function scope.
    var func_scope = Scope.init(self.allocator, .function);

    const key_val = Value.new(.{ .string = key }, 0);
    const index_val = Value.new(.{ .uint = index }, 0);

    try func_scope.map.put("key", key_val);
    try func_scope.map.put("value", item);
    try func_scope.map.put("index", Value.new(.{ .uint = index }, 0));

    if (func.ty.func.params.len > 0) try func_scope.map.put(func.ty.func.params[0], key_val);
    if (func.ty.func.params.len > 1) try func_scope.map.put(func.ty.func.params[1], item);
    if (func.ty.func.params.len > 2) try func_scope.map.put(func.ty.func.params[2], index_val);

    return self.evalPredicate(func.ty.func.instructions, func_scope);
}

fn evalPredicate(self: *Vm, instructions: []const u8, func_scope: Scope) anyerror!Value {
    // Set up Sub-VM arena.
    var vm_arena = std.heap.ArenaAllocator.init(self.allocator);
    defer vm_arena.deinit();
    const vm_allocator = vm_arena.allocator();

    _ = try self.pushScope(func_scope);

    var vm = try init(
        vm_allocator,
        instructions,
        self.scope_stack,
        self.ctx,
        self.output,
    );
    try vm.run();

    _ = self.popScope();

    return vm.last_popped;
}

fn execRedir(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const clobber = self.instructions[self.ip.*] == 1;
    self.ip.* += 1;

    // Get filename
    const filename = self.value_stack.pop();
    if (filename.ty != .string) return self.ctx.err(
        "Redirection filename must evaluate to a string",
        .{},
        error.InvalidRedirect,
        offset,
    );

    // Open file
    var create_flags: std.fs.File.CreateFlags = .{};
    if (!clobber) create_flags.truncate = false;
    var file = try std.fs.cwd().createFile(filename.ty.string, create_flags);
    defer file.close();
    if (!clobber) try file.seekFromEnd(0);

    // Buffering
    var file_buf = std.io.bufferedWriter(file.writer());
    var writer = file_buf.writer();

    // Write
    const value = self.value_stack.pop();
    _ = try writer.print("{}", .{value});
    try file_buf.flush();

    try self.value_stack.append(value);
}

// Scopes

fn pushScope(self: *Vm, scope: Scope) !void {
    try self.scope_stack.push(scope);
}

fn popScope(self: *Vm) Scope {
    return self.scope_stack.pop();
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
    var output = std.ArrayList(u8).init(allocator);

    var vm = try init(
        allocator,
        compiler.instructions.items,
        &scope_stack,
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

    got = try testVmValue(allocator,
        \\"foo" ++ "bar"
    );
    try std.testing.expectEqual(Value.Tag.string, got.ty);
    try std.testing.expectEqualStrings("foobar", got.ty.string);
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

test "Vm math builtins" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "atan2(0, -1)");
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 3.141592653589793e+00), got.ty.float);
    got = try testVmValue(allocator, "cos(-1)");
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 5.403023058681398e-01), got.ty.float);
    got = try testVmValue(allocator, "exp(5)");
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 1.484131591025766e+02), got.ty.float);
    got = try testVmValue(allocator, "int(-3.9)");
    try std.testing.expectEqual(Value.Tag.int, got.ty);
    try std.testing.expectEqual(@as(i64, -3), got.ty.int);
    got = try testVmValue(allocator, "int(3.9)");
    try std.testing.expectEqual(Value.Tag.int, got.ty);
    try std.testing.expectEqual(@as(i64, 3), got.ty.int);
    got = try testVmValue(allocator, "log(3.14)");
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 1.144222799920162e+00), got.ty.float);
    //try testLastValue("rand(10)", Value.new(.{ .uint = 10 }, 0));
    got = try testVmValue(allocator, "sin(3.14)");
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 1.5926529164868282e-03), got.ty.float);
    got = try testVmValue(allocator, "sqrt(49)");
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 7), got.ty.float);
}

test "Vm method builtins" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

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

    var got = try testVmValue(allocator, "[1, 2, 3].len()");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 3), got.ty.uint);
    got = try testVmValue(allocator,
        \\["a": 1, "b": 2, "c": 3].len()
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 3), got.ty.uint);
    got = try testVmValue(allocator,
        \\"foo".len()
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 3), got.ty.uint);
    got = try testVmValue(allocator,
        \\["a": 1, "b": 2, "c": 3].keys().len()
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 3), got.ty.uint);
    got = try testVmValue(allocator,
        \\["a": 1, "b": 2, "c": 3].values().len()
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 3), got.ty.uint);
    got = try testVmValue(allocator, mean_input);
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 2), got.ty.float);
    got = try testVmValue(allocator, median_input);
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 2.5), got.ty.float);
    got = try testVmValue(allocator, mode_input);
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 2), got.ty.uint);
    got = try testVmValue(allocator, stdev_input);
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 1.0671873729054748), got.ty.float);
    got = try testVmValue(allocator, "[1, 2, 3].min()");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 1), got.ty.uint);
    got = try testVmValue(allocator, "[1, 2, 3].max()");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 3), got.ty.uint);
    got = try testVmValue(allocator,
        \\["a", "z", "B"].min()
    );
    try std.testing.expectEqual(Value.Tag.string, got.ty);
    try std.testing.expectEqualStrings("B", got.ty.string);
    got = try testVmValue(allocator, "[2, 3, 1].sort()[0]");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 1), got.ty.uint);
    got = try testVmValue(allocator, "[2, 3, 1].reverse()[0]");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 1), got.ty.uint);
    got = try testVmValue(allocator, "[2, 3, 1].contains(3)");
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(true, got.ty.boolean);
    got = try testVmValue(allocator, "[2, 3, 1].contains(4)");
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(false, got.ty.boolean);
    got = try testVmValue(allocator,
        \\"foo".contains("oo")
    );
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(true, got.ty.boolean);
    got = try testVmValue(allocator,
        \\["a": 2, "b": 3].contains(3)
    );
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(true, got.ty.boolean);
    got = try testVmValue(allocator, "[2, 3, 1].indexOf(1)");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 2), got.ty.uint);
    got = try testVmValue(allocator, "[2, 3, 1].indexOf(4)");
    try std.testing.expectEqual(Value.Tag.nil, got.ty);
    got = try testVmValue(allocator,
        \\"H\u65\u301llo".indexOf("l")
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 2), got.ty.uint);
    got = try testVmValue(allocator,
        \\"H\u65\u301llo".lastIndexOf("l")
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 3), got.ty.uint);
    //try testLastValue(
    //, Value.new(.{ .string = "bar" }, 0));
    got = try testVmValue(allocator,
        \\"foo,bar,baz".split(",")[1]
    );
    try std.testing.expectEqual(Value.Tag.string, got.ty);
    try std.testing.expectEqualStrings("bar", got.ty.string);
    got = try testVmValue(allocator,
        \\["foo", 1, 2.3, nil].join(",")
    );
    try std.testing.expectEqual(Value.Tag.string, got.ty);
    try std.testing.expectEqualStrings("foo,1,2.3,", got.ty.string);
    got = try testVmValue(allocator,
        \\f := { a => a * 2 + index }
        \\[1, 2, 3].map(f)[1]
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 5), got.ty.uint);
    got = try testVmValue(allocator,
        \\[1, 2, 3].filter() { it > 1 }[1]
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 3), got.ty.uint);
    got = try testVmValue(allocator,
        \\total := 0
        \\[1, 2, 3].each() { total = total + it }
        \\total
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 6), got.ty.uint);
    got = try testVmValue(allocator,
        \\[1, 2, 3].reduce(1) { acc * it }
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 6), got.ty.uint);
    //try testLastValueWithOutput(
    //    \\print("foo", 1, 2, 3.14)
    //, Value.new(.nil, 0), "foo,1,2,3.14");
    //try testLastValueWithOutput(
    //    \\print("foo", 1, 2, 3.14, "foo {1}")
    //, Value.new(.nil, 0), "foo,1,2,3.14,foo 1");
    //try testLastValueWithOutput(
    //    \\print("foo", 1, 2, 3.14, "{#d:0>3# 1}")
    //, Value.new(.nil, 0), "foo,1,2,3.14,001");
    //try testLastValue(
    //, Value.new(.{ .string = "\u{65}\u{301}" }, 0));
    got = try testVmValue(allocator,
        \\"H\u65\u301llo".chars()[1]
    );
    try std.testing.expectEqual(Value.Tag.string, got.ty);
    try std.testing.expectEqualStrings("\u{65}\u{301}", got.ty.string);
    got = try testVmValue(allocator,
        \\"Hello".startsWith("Hell")
    );
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(true, got.ty.boolean);
    got = try testVmValue(allocator,
        \\"Hello".endsWith("llo")
    );
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(true, got.ty.boolean);
    got = try testVmValue(allocator,
        \\l := [1]
        \\l.push(2)
        \\l[1]
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 2), got.ty.uint);
    got = try testVmValue(allocator,
        \\l := [1]
        \\l.pop()
    );
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 1), got.ty.uint);
}
