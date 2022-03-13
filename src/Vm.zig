const std = @import("std");

const Compiler = @import("Compiler.zig");
const Context = @import("Context.zig");
const GraphemeIterator = @import("ziglyph").GraphemeIterator;
const Node = @import("Node.zig");
const Scope = @import("Scope.zig");
const ScopeStack = @import("ScopeStack.zig");
const Token = @import("Token.zig");
const Value = @import("Value.zig");
const runtimePrint = @import("fmt.zig").runtimePrint;
const ziglyph = @import("ziglyph");

const ObjectTag = std.meta.Tag(Value.Object);

allocator: std.mem.Allocator,
ctx: Context,
last_popped: Value = Value{ .ty = .nil },
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
            .int => try self.execInt(),
            .uint => try self.execUint(),
            // Strings
            .format => try self.execFormat(),
            .plain => try self.execPlain(),
            .string => try self.execString(),
            // functions
            .builtin => try self.execBuiltin(),
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
            .global => try self.execGlobal(),
            .gstore => try self.execGlobalStore(),
            // Infix
            .add => try self.execAdd(),
            .sub => try self.execSub(),
            .mul => try self.execMul(),
            .div => try self.execDiv(),
            .mod => try self.execMod(),
            .concat => try self.execConcat(),
            .repeat => try self.execRepeat(),
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
    try self.value_stack.append(Value.new(.{ .boolean = false }));
    self.ip.* += 2;
}
fn execBoolTrue(self: *Vm) !void {
    self.ip.* += 1;
    try self.value_stack.append(Value.new(.{ .boolean = true }));
    self.ip.* += 2;
}
fn execNil(self: *Vm) !void {
    self.ip.* += 1;
    try self.value_stack.append(Value.new(.nil));
    self.ip.* += 2;
}

fn execFloat(self: *Vm) !void {
    self.ip.* += 3;
    const f = self.getNumber(f64, self.ip.*, 8);
    self.ip.* += 8;
    try self.value_stack.append(Value.new(.{ .float = f }));
}
fn execInt(self: *Vm) !void {
    self.ip.* += 3;
    const i = self.getNumber(i64, self.ip.*, 8);
    self.ip.* += 8;
    try self.value_stack.append(Value.new(.{ .int = i }));
}
fn execUint(self: *Vm) !void {
    self.ip.* += 3;
    const u = self.getNumber(u64, self.ip.*, 8);
    self.ip.* += 8;
    try self.value_stack.append(Value.new(.{ .uint = u }));
}

fn execFormat(self: *Vm) !void {
    self.ip.* += 1;
    const spec = std.mem.sliceTo(self.instructions[self.ip.*..], 0);
    self.ip.* += @intCast(u16, spec.len) + 1;

    const value = self.value_stack.pop();
    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();
    try runtimePrint(
        self.allocator,
        spec,
        value,
        writer,
    );
    try buf.append(0);

    try self.value_stack.append(try Value.newStringZ(self.allocator, buf.items));
}
fn execPlain(self: *Vm) !void {
    self.ip.* += 1;
    const s = std.mem.sliceTo(self.instructions[self.ip.*..], 0);
    self.ip.* += @intCast(u16, s.len) + 1;

    try self.value_stack.append(try Value.newStringZ(self.allocator, s));
}
fn execString(self: *Vm) !void {
    self.ip.* += 3;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;

    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();
    var i: usize = 0;
    while (i < len) : (i += 1) _ = try writer.print("{}", .{self.value_stack.pop()});
    try buf.append(0);

    try self.value_stack.append(try Value.newStringZ(self.allocator, buf.items));
}

fn execFunc(self: *Vm) !void {
    self.ip.* += 1;

    // Function name
    const func_name = std.mem.sliceTo(self.instructions[self.ip.*..], 0);
    self.ip.* += @intCast(u16, func_name.len) + 1;

    // function params
    const params_len = self.getU16(self.ip.*);
    self.ip.* += 2;

    var params: [][]const u8 = &[0][]const u8{};
    if (params_len != 0) {
        var params_list = std.ArrayList([]const u8).init(self.allocator);
        var param_index: usize = 0;

        while (param_index < params_len) : (param_index += 1) {
            const param = std.mem.sliceTo(self.instructions[self.ip.*..], 0);
            try params_list.append(param);
            self.ip.* += @intCast(u16, param.len) + 1;
        }

        params = params_list.items;
    }

    // Function instructions
    const instructions_len = self.getU16(self.ip.*);
    self.ip.* += 2;
    const func_instructions: []const u8 = if (instructions_len == 0) "" else self.instructions[self.ip.* .. self.ip.* + instructions_len];
    self.ip.* += instructions_len;

    const func = Value.Function{
        .name = func_name,
        .params = params,
        .instructions = func_instructions,
    };
    try self.value_stack.append(try Value.newFunc(self.allocator, func));
}

fn execReturn(self: *Vm) void {
    self.popFrame();
    // Unwind scopes up to the function's scope.
    while (true) if (self.popScope().ty == .function) break;
    self.ip.* += 1;
}

fn execBuiltin(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const builtin = @intToEnum(Token.Tag, self.instructions[self.ip.*]);

    return switch (builtin) {
        .pd_atan2 => try self.atan2(),
        .pd_chars => try self.strChars(),
        .pd_contains => try self.contains(),
        .pd_cos => try self.oneArgMath(builtin),
        .pd_each => try self.each(),
        .pd_endsWith => try self.strEndsWith(),
        .pd_exp => try self.oneArgMath(builtin),
        .pd_filter => try self.listFilter(),
        .pd_join => try self.listJoin(),
        .pd_indexOf => try self.indexOf(),
        .pd_int => try self.oneArgMath(builtin),
        .pd_keys => try self.mapKeys(),
        .pd_keysByValueAsc => try self.mapKeysByValueAsc(),
        .pd_keysByValueDesc => try self.mapKeysByValueDesc(),
        .pd_lastIndexOf => try self.lastIndexOf(),
        .pd_len => try self.length(),
        .pd_log => try self.oneArgMath(builtin),
        .pd_map => try self.listMap(),
        .pd_max => try self.listMax(),
        .pd_mean => try self.listMean(),
        .pd_median => try self.listMedian(),
        .pd_min => try self.listMin(),
        .pd_mode => try self.listMode(),
        .pd_print => try self.execPrint(),
        .pd_pop => try self.listPop(),
        .pd_push => try self.listPush(),
        .pd_rand => try self.rand(),
        .pd_reduce => try self.listReduce(),
        .pd_reverse => try self.listReverse(),
        .pd_sin => try self.oneArgMath(builtin),
        .pd_sortAsc => try self.listSortAsc(),
        .pd_sortDesc => try self.listSortDesc(),
        .pd_split => try self.strSplit(),
        .pd_sqrt => try self.oneArgMath(builtin),
        .pd_startsWith => try self.strStartsWith(),
        .pd_stdev => try self.listStdev(),
        .pd_toLower => try self.strToLower(),
        .pd_toUpper => try self.strToUpper(),
        .pd_values => try self.mapValues(),
        else => unreachable,
    };
}

fn execCall(self: *Vm) anyerror!void {
    // Get the offset.
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    // Get the function.
    const callee = self.value_stack.pop();
    if (!inObjectTypes(callee, &[_]ObjectTag{.func})) return self.ctx.err(
        "{s} is not callable.",
        .{@tagName(callee.ty)},
        error.InvalidCall,
        offset,
    );

    // Prepare the child scope.
    var func_scope = Scope.init(self.allocator, .function);

    // Self-references
    if (callee.ty.obj.func.name.len != 0) try func_scope.map.put(callee.ty.obj.func.name, callee);

    // Process args
    const num_args = self.instructions[self.ip.*];

    var i: usize = 0;
    while (i < num_args) : (i += 1) {
        const arg = self.value_stack.pop();
        if (i == 0) try func_scope.map.put("it", arg); // it
        var buf: [4]u8 = undefined;
        const auto_arg_name = try std.fmt.bufPrint(&buf, "@{}", .{i});
        try func_scope.map.put(try self.allocator.dupe(u8, auto_arg_name), arg); // @0, @1, ...
        if (i < callee.ty.obj.func.params.len) try func_scope.map.put(callee.ty.obj.func.params[i], arg);
    }

    // Push the function's frame.
    try self.pushScope(func_scope);
    try self.pushFrame(callee.ty.obj.func.instructions);

    // NOTE: Final self.ip.* += 1 is done on return.
}

fn execDefine(self: *Vm) !void {
    // Offset
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    // Name
    const name = std.mem.sliceTo(self.instructions[self.ip.*..], 0);
    self.ip.* += @intCast(u16, name.len) + 1;
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
    const name = std.mem.sliceTo(self.instructions[self.ip.*..], 0);
    self.ip.* += @intCast(u16, name.len) + 1;
    // Is the name defined?
    if (!self.scope_stack.isDefined(name)) return self.ctx.err(
        "{s} is undefined.",
        .{name},
        error.NameUndefined,
        offset,
    );
    // Load
    try self.value_stack.append((try self.scope_stack.load(name)).?);
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
    const name = std.mem.sliceTo(self.instructions[self.ip.*..], 0);
    self.ip.* += @intCast(u16, name.len) + 1;
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
        const old_value = (try self.scope_stack.load(name)).?;

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
fn execGlobal(self: *Vm) !void {
    self.ip.* += 1;
    const global = @intToEnum(Token.Tag, self.instructions[self.ip.*]);
    self.ip.* += 1;

    const result = switch (global) {
        .at_cols => try Value.newList(self.allocator, self.scope_stack.columns),
        .at_file => try Value.newStringP(self.allocator, self.scope_stack.file),
        .at_frnum => Value{ .ty = .{ .uint = @intCast(u64, self.scope_stack.frnum) } },
        .at_ifs => try Value.newStringP(self.allocator, self.scope_stack.ifs),
        .at_irs => try Value.newStringP(self.allocator, self.scope_stack.irs),
        .at_ofs => try Value.newStringP(self.allocator, self.scope_stack.ofs),
        .at_ors => try Value.newStringP(self.allocator, self.scope_stack.ors),
        .at_rec => try Value.newStringP(self.allocator, self.scope_stack.record),
        .at_rnum => Value{ .ty = .{ .uint = @intCast(u64, self.scope_stack.frnum) } },

        else => unreachable,
    };

    try self.value_stack.append(result);
}
fn execGlobalStore(self: *Vm) !void {
    // Get the offset.
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    // Get the global type.
    const global = @intToEnum(Token.Tag, self.instructions[self.ip.*]);
    self.ip.* += 1;
    // Get the value to assign.
    const rvalue = self.value_stack.pop();

    switch (global) {
        .at_ifs => {
            if (!inObjectTypes(rvalue, &[_]ObjectTag{.string})) return self.ctx.err(
                "@ifs must be a string.",
                .{},
                error.InvalidIfs,
                offset,
            );
            self.scope_stack.ifs = try self.scope_stack.allocator.dupeZ(u8, std.mem.sliceTo(rvalue.ty.obj.string, 0));
        },
        .at_irs => {
            if (!inObjectTypes(rvalue, &[_]ObjectTag{.string})) return self.ctx.err(
                "@irs must be a string.",
                .{},
                error.InvalidIrs,
                offset,
            );
            self.scope_stack.irs = try self.scope_stack.allocator.dupeZ(u8, std.mem.sliceTo(rvalue.ty.obj.string, 0));
        },
        .at_ofs => {
            if (!inObjectTypes(rvalue, &[_]ObjectTag{.string})) return self.ctx.err(
                "@ofs must be a string.",
                .{},
                error.InvalidOfs,
                offset,
            );
            self.scope_stack.ofs = try self.scope_stack.allocator.dupeZ(u8, std.mem.sliceTo(rvalue.ty.obj.string, 0));
        },
        .at_ors => {
            if (!inObjectTypes(rvalue, &[_]ObjectTag{.string})) return self.ctx.err(
                "@ors must be a string.",
                .{},
                error.InvalidOrs,
                offset,
            );
            self.scope_stack.ors = try self.scope_stack.allocator.dupeZ(u8, std.mem.sliceTo(rvalue.ty.obj.string, 0));
        },
        .at_rec => {
            if (!inObjectTypes(rvalue, &[_]ObjectTag{.string})) return self.ctx.err(
                "@rec must be a string.",
                .{},
                error.InvalidRec,
                offset,
            );
            self.scope_stack.record = try self.allocator.dupeZ(u8, std.mem.sliceTo(rvalue.ty.obj.string, 0));
        },
        .at_cols => {
            if (!inObjectTypes(rvalue, &[_]ObjectTag{.list})) return self.ctx.err(
                "@cols must be a list.",
                .{},
                error.InvalidCols,
                offset,
            );
            self.scope_stack.columns = (try rvalue.copy(self.allocator)).ty.obj.list;
        },
        else => unreachable,
    }

    try self.value_stack.append(rvalue);
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
        .lt => Value.new(.{ .boolean = comparison == .lt }),
        .lte => Value.new(.{ .boolean = comparison == .lt or comparison == .eq }),
        .gt => Value.new(.{ .boolean = comparison == .gt }),
        .gte => Value.new(.{ .boolean = comparison == .gt or comparison == .eq }),

        else => unreachable,
    };

    try self.value_stack.append(result);
}
fn execEqNeq(self: *Vm, opcode: Compiler.Opcode) !void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 3;

    var comparison = left.eql(right);
    if (opcode == .neq) comparison = !comparison;
    try self.value_stack.append(Value.new(.{ .boolean = comparison }));
}
fn execConcat(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (!inObjectTypes(left, &[_]ObjectTag{.string}) or
        !inObjectTypes(right, &[_]ObjectTag{.string})) return self.ctx.err(
        "Invlid concatenation.",
        .{},
        error.InvalidConcat,
        offset,
    );

    const str_left = std.mem.sliceTo(left.ty.obj.string, 0);
    const str_right = std.mem.sliceTo(right.ty.obj.string, 0);

    var buf = try self.allocator.alloc(u8, str_left.len + str_right.len + 1);
    std.mem.copy(u8, buf, str_left);
    std.mem.copy(u8, buf[str_left.len..], str_right);
    buf[buf.len - 1] = 0;

    try self.value_stack.append(try Value.newStringZ(self.allocator, buf));
}
fn execRepeat(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (!inObjectTypes(left, &[_]ObjectTag{.string}) or right.ty != .uint) return self.ctx.err(
        "Invalid string repeat.",
        .{},
        error.InvalidRepeat,
        offset,
    );

    const str_left = std.mem.sliceTo(left.ty.obj.string, 0);

    var buf = try self.allocator.alloc(u8, str_left.len * right.ty.uint + 1);
    var i: usize = 0;
    while (i < right.ty.uint) : (i += 1) std.mem.copy(u8, buf[str_left.len * i ..], str_left);
    buf[buf.len - 1] = 0;

    try self.value_stack.append(try Value.newStringZ(self.allocator, buf));
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
    try self.value_stack.append(Value.new(.{ .boolean = !value.ty.boolean }));
}
fn execNeg(self: *Vm) !void {
    const value = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    switch (value.ty) {
        .float => |f| try self.value_stack.append(Value.new(.{ .float = -f })),
        .int => |i| try self.value_stack.append(Value.new(.{ .int = -i })),
        .uint => |u| try self.value_stack.append(Value.new(.{ .int = -@intCast(isize, u) })),
        else => return self.ctx.err(
            "-{s} ?",
            .{@tagName(value.ty)},
            error.InvalidNeg,
            offset,
        ),
    }
}

fn execList(self: *Vm) !void {
    self.ip.* += 3;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;

    var list = try std.ArrayList(Value).initCapacity(self.allocator, len);
    var i: usize = 0;
    while (i < len) : (i += 1) list.appendAssumeCapacity(self.value_stack.pop());

    try self.value_stack.append(try Value.newList(self.allocator, list));
}
fn execMap(self: *Vm) !void {
    self.ip.* += 3;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;

    var map = std.StringHashMap(Value).init(self.allocator);
    try map.ensureTotalCapacity(len);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        const value = self.value_stack.pop();
        const key = self.value_stack.pop();
        map.putAssumeCapacity(std.mem.sliceTo(key.ty.obj.string, 0), value);
    }

    try self.value_stack.append(try Value.newMap(self.allocator, map));
}

fn execSubscript(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const container = self.value_stack.pop();

    if (!inObjectTypes(container, &[_]ObjectTag{ .list, .map }))
        return self.ctx.err(
            "{s}[] ?",
            .{@tagName(container.ty)},
            error.InvalidSubscript,
            offset,
        );

    return if (container.ty.obj.* == .list)
        try self.execSubscriptList(container, offset)
    else
        try self.execSubscriptMap(container, offset);
}
fn execSubscriptList(self: *Vm, list: Value, offset: u16) !void {
    const index = self.value_stack.pop();
    if (index.ty != .uint and !inObjectTypes(index, &[_]ObjectTag{.range})) return self.ctx.err(
        "list[{s}] ?",
        .{@tagName(index.ty)},
        error.InvalidSubscript,
        offset,
    );

    if (index.ty == .uint) {
        if (index.ty.uint >= list.ty.obj.list.items.len) return self.ctx.err(
            "Index out of bounds.",
            .{},
            error.InvalidSubscript,
            offset,
        );
        try self.value_stack.append(list.ty.obj.list.items[index.ty.uint]);
    } else {
        if (index.ty.obj.range[1] > list.ty.obj.list.items.len) return self.ctx.err(
            "Index out of bounds.",
            .{},
            error.InvalidSubscript,
            offset,
        );

        var new_list = try std.ArrayList(Value).initCapacity(self.allocator, index.ty.obj.range[1] - index.ty.obj.range[0]);
        for (list.ty.obj.list.items[index.ty.obj.range[0]..index.ty.obj.range[1]]) |item|
            new_list.appendAssumeCapacity(item); //TODO: Copy here?

        try self.value_stack.append(try Value.newList(self.allocator, new_list));
    }
}
fn execSubscriptMap(self: *Vm, map: Value, offset: u16) !void {
    const key = self.value_stack.pop();
    if (!inObjectTypes(key, &[_]ObjectTag{.string})) return self.ctx.err(
        "map[{s}] ?",
        .{@tagName(key.ty)},
        error.InvalidSubscript,
        offset,
    );
    const value = if (map.ty.obj.map.get(std.mem.sliceTo(key.ty.obj.string, 0))) |v| v else Value.new(.nil);
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

    if (!inObjectTypes(container, &[_]ObjectTag{ .list, .map })) return self.ctx.err(
        "{s}[]= ?",
        .{@tagName(container.ty)},
        error.InvalidSubscript,
        offset,
    );

    return if (container.ty.obj.* == .list)
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
    if (index.ty.uint >= list.ty.obj.list.items.len) return self.ctx.err(
        "Index out of bounds.",
        .{},
        error.InvalidSubscript,
        offset,
    );
    const rvalue = self.value_stack.pop();

    // Store
    if (combo == .none) {
        list.ty.obj.list.items[index.ty.uint] = rvalue; //TODO: Deinit old value?
        try self.value_stack.append(rvalue);
    } else {
        const old_value = list.ty.obj.list.items[index.ty.uint];

        const new_value = switch (combo) {
            .none => unreachable,
            .add => try old_value.add(rvalue),
            .sub => try old_value.sub(rvalue),
            .mul => try old_value.mul(rvalue),
            .div => try old_value.div(rvalue),
            .mod => try old_value.mod(rvalue),
        };

        list.ty.obj.list.items[index.ty.uint] = new_value;
        try self.value_stack.append(new_value);
    }
}
fn execSetMap(self: *Vm, map: Value, offset: u16, combo: Node.Combo) !void {
    const key = self.value_stack.pop();
    if (!inObjectTypes(key, &[_]ObjectTag{.string})) return self.ctx.err(
        "map[{s}]= ?",
        .{@tagName(key.ty)},
        error.InvalidSubscript,
        offset,
    );
    const rvalue = self.value_stack.pop();
    const key_copy = try map.ty.obj.map.allocator.dupe(u8, std.mem.sliceTo(key.ty.obj.string, 0));

    // Store
    if (combo == .none) {
        try map.ty.obj.map.put(key_copy, try rvalue.copy(map.ty.obj.map.allocator)); //TODO: Deinit old value?
        try self.value_stack.append(rvalue);
    } else {
        const old_value = map.ty.obj.map.get(std.mem.sliceTo(key.ty.obj.string, 0)) orelse Value.new(.{ .uint = 0 });

        const new_value = switch (combo) {
            .none => unreachable,
            .add => try old_value.add(rvalue),
            .sub => try old_value.sub(rvalue),
            .mul => try old_value.mul(rvalue),
            .div => try old_value.div(rvalue),
            .mod => try old_value.mod(rvalue),
        };

        try map.ty.obj.map.put(key_copy, try new_value.copy(map.ty.obj.map.allocator));
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

    const range = try Value.newRange(self.allocator, [2]usize{ from_uint, to_uint });
    try self.value_stack.append(range);
}

fn execRecRange(self: *Vm) anyerror!void {
    self.ip.* += 3;
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

    const nil_value = Value.new(.nil);
    const from = if (has_from) self.value_stack.pop() else nil_value;
    const to = if (has_to) self.value_stack.pop() else nil_value;

    var result = nil_value;
    var exec_action = false;

    if (self.scope_stack.rec_ranges.contains(range_id)) {
        // In range
        exec_action = true;

        if (isTruthy(to)) {
            // Range end.
            _ = self.scope_stack.rec_ranges.remove(range_id);
            if (exclusive) exec_action = false;
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
            exec_action = true;
        }
    }

    if (exec_action) {
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
        offset,
    );

    const x_val = self.value_stack.pop();
    const x = x_val.asFloat() orelse return self.ctx.err(
        "atan2 x not convertible to float.",
        .{},
        error.InvalidAtan2,
        offset,
    );

    const result = Value.new(.{ .float = std.math.atan2(f64, y.ty.float, x.ty.float) });
    try self.value_stack.append(result);
}
fn strChars(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const str = self.value_stack.pop();
    if (str.ty != .obj or str.ty.obj.* != .string) return self.ctx.err(
        "{s}.chars() ?",
        .{@tagName(str.ty)},
        error.InvalidCharsCall,
        offset,
    );

    var list = std.ArrayList(Value).init(self.allocator);

    var giter = GraphemeIterator.init(std.mem.sliceTo(str.ty.obj.string, 0)) catch |err| return self.ctx.err(
        "Unicode error.",
        .{},
        err,
        offset,
    );
    while (giter.next()) |grapheme| {
        try list.append(try Value.newString(self.allocator, grapheme.bytes));
    }

    try self.value_stack.append(try Value.newList(self.allocator, list));
    self.ip.* += 1;
}
fn execPrint(self: *Vm) anyerror!void {
    self.ip.* += 3;

    // Get args count.
    const num_args = self.instructions[self.ip.*];
    self.ip.* += 1;

    var writer = self.output.writer();
    var i: usize = 0;
    while (i < num_args) : (i += 1) {
        if (i != 0) try writer.writeAll(std.mem.sliceTo(self.scope_stack.ofs, 0));
        _ = try writer.print("{}", .{self.value_stack.pop()});
    }

    try self.value_stack.append(Value.new(.nil));
}
fn execSprint(self: *Vm) anyerror!void {
    self.ip.* += 3;

    // Get args count.
    const num_args = self.instructions[self.ip.*];
    self.ip.* += 1;

    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();

    var i: usize = 0;
    while (i < num_args) : (i += 1) {
        if (i != 0) try writer.writeAll(std.mem.sliceTo(self.scope_stack.ofs, 0));
        _ = try writer.print("{}", .{self.value_stack.pop()});
    }
    try buf.append(0);

    try self.value_stack.append(try Value.newStringZ(self.allocator, buf.items));
}
fn oneArgMath(self: *Vm, builtin: Token.Tag) anyerror!void {
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
        offset,
    );

    const result = switch (builtin) {
        .pd_cos => Value.new(.{ .float = @cos(x.ty.float) }),
        .pd_exp => Value.new(.{ .float = std.math.exp(x.ty.float) }),
        .pd_int => Value.new(.{ .int = @floatToInt(isize, @trunc(x.ty.float)) }),
        .pd_log => Value.new(.{ .float = @log(x.ty.float) }),
        .pd_rand => Value.new(.{ .uint = std.rand.DefaultPrng.init(@intCast(usize, std.time.timestamp())).random().uintAtMost(usize, x.ty.uint) }),
        .pd_sin => Value.new(.{ .float = @sin(x.ty.float) }),
        .pd_sqrt => Value.new(.{ .float = @sqrt(x.ty.float) }),
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
    if (haystack.ty != .obj or
        (haystack.ty.obj.* != .list and
        haystack.ty.obj.* != .map and
        haystack.ty.obj.* != .string))
        return self.ctx.err(
            "contains not allowed on {s}.",
            .{@tagName(haystack.ty)},
            error.InvalidContains,
            offset,
        );

    const result = switch (haystack.ty.obj.*) {
        .list => |l| for (l.items) |item| {
            if (needle.eql(item)) break Value.new(.{ .boolean = true });
        } else Value.new(.{ .boolean = false }),
        .map => |m| mp: {
            var iter = m.valueIterator();
            break :mp while (iter.next()) |value_ptr| {
                if (needle.eql(value_ptr.*)) break Value.new(.{ .boolean = true });
            } else Value.new(.{ .boolean = false });
        },
        .string => |s| str: {
            if (needle.ty != .obj or needle.ty.obj.* != .string) return self.ctx.err(
                "contains arg on strings must be a string.",
                .{},
                error.InvalidContains,
                offset,
            );

            break :str Value.new(.{ .boolean = std.mem.containsAtLeast(u8, std.mem.sliceTo(s, 0), 1, std.mem.sliceTo(needle.ty.obj.string, 0)) });
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
    if (haystack.ty != .obj or
        (haystack.ty.obj.* != .list and
        haystack.ty.obj.* != .string)) return self.ctx.err(
        "indexOf not allowed on {s}.",
        .{@tagName(haystack.ty)},
        error.InvalidIndexOf,
        offset,
    );

    const result = switch (haystack.ty.obj.*) {
        .list => |l| for (l.items) |item, i| {
            if (needle.eql(item)) break Value.new(.{ .uint = i });
        } else Value.new(.nil),
        .string => |s| str: {
            if (needle.ty != .obj or needle.ty.obj.* != .string) return self.ctx.err(
                "indexOf arg on strings must be a string.",
                .{},
                error.InvalidIndexOf,
                offset,
            );

            var giter = try GraphemeIterator.init(std.mem.sliceTo(s, 0));
            var i: usize = 0;
            break :str while (giter.next()) |grapheme| : (i += 1) {
                if (std.mem.eql(u8, std.mem.sliceTo(needle.ty.obj.string, 0), grapheme.bytes)) break Value.new(.{ .uint = i });
            } else Value.new(.nil);
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
    if (!inObjectTypes(haystack, &[_]ObjectTag{ .list, .string })) return self.ctx.err(
        "lastIndexOf not allowed on {s}.",
        .{@tagName(haystack.ty)},
        error.InvalidLastIndexOf,
        offset,
    );

    const result = switch (haystack.ty.obj.*) {
        .list => |l| lst: {
            var i: usize = 0;
            const len = l.items.len;
            break :lst while (i <= len) : (i += 1) {
                if (needle.eql(l.items[len - i])) break Value.new(.{ .uint = i });
            } else Value.new(.nil);
        },
        .string => |s| str: {
            if (needle.ty != .obj or needle.ty.obj.* != .string) return self.ctx.err(
                "lastIndexOf arg on strings must be a string.",
                .{},
                error.InvalidLastIndexOf,
                offset,
            );

            var giter = try GraphemeIterator.init(std.mem.sliceTo(s, 0));
            var i: usize = 0;
            var index = Value.new(.nil);
            while (giter.next()) |grapheme| : (i += 1) {
                if (std.mem.eql(u8, std.mem.sliceTo(needle.ty.obj.string, 0), grapheme.bytes)) index = Value.new(.{ .uint = i });
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
    if (value.ty != .obj or
        (value.ty.obj.* != .list and
        value.ty.obj.* != .map and
        value.ty.obj.* != .string))
        return self.ctx.err(
            "len not allowed on {s}.",
            .{@tagName(value.ty)},
            error.InvalidLen,
            offset,
        );

    const result = switch (value.ty.obj.*) {
        .list => |l| Value.new(.{ .uint = l.items.len }),
        .map => |m| Value.new(.{ .uint = m.count() }),
        .string => |s| Value.new(.{ .uint = std.mem.sliceTo(s, 0).len }),
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
    if (m.ty != .obj or m.ty.obj.* != .map) return self.ctx.err(
        "keys not allowed on {s}.",
        .{@tagName(m.ty)},
        error.InvalidKeys,
        offset,
    );

    var list = if (m.ty.obj.map.count() == 0)
        std.ArrayList(Value).init(self.allocator)
    else
        try std.ArrayList(Value).initCapacity(self.allocator, m.ty.obj.map.count());

    var key_iter = m.ty.obj.map.keyIterator();
    while (key_iter.next()) |key| {
        list.appendAssumeCapacity(try Value.newString(self.allocator, key.*));
    }

    try self.value_stack.append(try Value.newList(self.allocator, list));
    self.ip.* += 1;
}

fn entryAsc(_: void, a: std.StringHashMap(Value).Entry, b: std.StringHashMap(Value).Entry) bool {
    return a.value_ptr.cmp(b.value_ptr.*) catch unreachable == .lt;
}
fn entryDesc(_: void, a: std.StringHashMap(Value).Entry, b: std.StringHashMap(Value).Entry) bool {
    return a.value_ptr.cmp(b.value_ptr.*) catch unreachable == .gt;
}

fn mapKeysByValueAsc(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const m = self.value_stack.pop();
    if (m.ty != .obj or m.ty.obj.* != .map) return self.ctx.err(
        "keysByValueAsc not allowed on {s}.",
        .{@tagName(m.ty)},
        error.InvalidKeysByValueAsc,
        offset,
    );

    if (m.ty.obj.map.count() == 0) {
        var list = std.ArrayList(Value).init(self.allocator);
        try self.value_stack.append(try Value.newList(self.allocator, list));
        self.ip.* += 1;
        return;
    }

    var list = try std.ArrayList(Value).initCapacity(self.allocator, m.ty.obj.map.count());

    var entries = try std.ArrayList(std.StringHashMap(Value).Entry).initCapacity(self.allocator, m.ty.obj.map.count());
    defer entries.deinit();

    var iter = m.ty.obj.map.iterator();
    while (iter.next()) |entry| entries.appendAssumeCapacity(entry);

    std.sort.sort(std.StringHashMap(Value).Entry, entries.items, {}, entryAsc);

    for (entries.items) |entry| {
        list.appendAssumeCapacity(try Value.newString(self.allocator, entry.key_ptr.*));
    }

    try self.value_stack.append(try Value.newList(self.allocator, list));
    self.ip.* += 1; // num_args
}
fn mapKeysByValueDesc(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const m = self.value_stack.pop();
    if (m.ty != .obj or m.ty.obj.* != .map) return self.ctx.err(
        "keysByValueDesc not allowed on {s}.",
        .{@tagName(m.ty)},
        error.InvalidKeysByValueDesc,
        offset,
    );

    if (m.ty.obj.map.count() == 0) {
        var list = std.ArrayList(Value).init(self.allocator);
        try self.value_stack.append(try Value.newList(self.allocator, list));
        self.ip.* += 1;
        return;
    }

    var list = try std.ArrayList(Value).initCapacity(self.allocator, m.ty.obj.map.count());

    var entries = try std.ArrayList(std.StringHashMap(Value).Entry).initCapacity(self.allocator, m.ty.obj.map.count());
    defer entries.deinit();

    var iter = m.ty.obj.map.iterator();
    while (iter.next()) |entry| entries.appendAssumeCapacity(entry);

    std.sort.sort(std.StringHashMap(Value).Entry, entries.items, {}, entryDesc);

    for (entries.items) |entry| {
        list.appendAssumeCapacity(try Value.newString(self.allocator, entry.key_ptr.*));
    }

    try self.value_stack.append(try Value.newList(self.allocator, list));
    self.ip.* += 1; // num_args
}

fn mapValues(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const m = self.value_stack.pop();
    if (m.ty != .obj or m.ty.obj.* != .map) return self.ctx.err(
        "values not allowed on {s}.",
        .{@tagName(m.ty)},
        error.InvalidValues,
        offset,
    );

    var list = if (m.ty.obj.map.count() == 0)
        std.ArrayList(Value).init(self.allocator)
    else
        try std.ArrayList(Value).initCapacity(self.allocator, m.ty.obj.map.count());

    var value_iter = m.ty.obj.map.valueIterator();
    while (value_iter.next()) |value| list.appendAssumeCapacity(value.*);

    try self.value_stack.append(try Value.newList(self.allocator, list));
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
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "mean not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMean,
        offset,
    );

    if (l.ty.obj.list.items.len == 0) {
        try self.value_stack.append(Value.new(.{ .float = 0 }));
        self.ip.* += 1;
        return;
    }

    try self.value_stack.append(Value.new(.{ .float = listMeanHelper(l.ty.obj.list) }));
    self.ip.* += 1;
}
fn listMedian(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "median not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMedian,
        offset,
    );

    if (l.ty.obj.list.items.len == 0) {
        try self.value_stack.append(Value.new(.{ .float = 0 }));
        self.ip.* += 1;
        return;
    }

    var list_copy = try std.ArrayList(f64).initCapacity(self.allocator, l.ty.obj.list.items.len);

    for (l.ty.obj.list.items) |item| {
        if (item.asFloat()) |f| list_copy.appendAssumeCapacity(f.ty.float);
    }
    std.sort.sort(f64, list_copy.items, {}, comptime std.sort.asc(f64));

    var median: f64 = @intToFloat(f64, list_copy.items.len) + 1 / 2 - 1;
    if (list_copy.items.len % 2 == 0) {
        const mid = list_copy.items.len / 2 - 1;
        median = (list_copy.items[mid] + list_copy.items[mid + 1]) / 2;
    }

    try self.value_stack.append(Value.new(.{ .float = median }));
    self.ip.* += 1;
}
fn listMode(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "mode not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMode,
        offset,
    );

    if (l.ty.obj.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil));
        self.ip.* += 1;
        return;
    }

    var counts = std.StringHashMap(usize).init(self.allocator);
    var key_buf: [4096]u8 = undefined;

    for (l.ty.obj.list.items) |item| {
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
    var list = std.ArrayList(Value).init(self.allocator);
    while (iter.next()) |entry| {
        if (entry.value_ptr.* == highest) try list.append(Value.new(.{ .float = std.fmt.parseFloat(f64, entry.key_ptr.*) catch unreachable }));
    }
    std.sort.sort(Value, list.items, {}, Value.lessThan);

    const result = if (list.items.len == counts.count()) Value.new(.nil) else try Value.newList(self.allocator, list);
    try self.value_stack.append(result);
    self.ip.* += 1;
}
fn listStdev(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "stdev not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidStdev,
        offset,
    );

    if (l.ty.obj.list.items.len == 0) {
        try self.value_stack.append(Value.new(.{ .float = 0 }));
        self.ip.* += 1;
        return;
    }

    const mean = listMeanHelper(l.ty.obj.list);

    var sum_of_squares: f64 = 0;
    var count: f64 = 0;
    for (l.ty.obj.list.items) |item| {
        if (item.asFloat()) |f| {
            const diff = f.ty.float - mean;
            const square = diff * diff;
            sum_of_squares += square;
            count += 1;
        }
    }

    const sos_by_count = sum_of_squares / count;

    try self.value_stack.append(Value.new(.{ .float = @sqrt(sos_by_count) }));
    self.ip.* += 1;
}
fn listMin(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (!inObjectTypes(l, &[_]ObjectTag{.list})) return self.ctx.err(
        "min not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMin,
        offset,
    );

    if (l.ty.obj.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil));
        self.ip.* += 1;
        return;
    }

    var min = l.ty.obj.list.items[0];
    for (l.ty.obj.list.items) |item| {
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
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "max not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMax,
        offset,
    );

    if (l.ty.obj.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil));
        self.ip.* += 1;
        return;
    }

    var max = l.ty.obj.list.items[0];
    for (l.ty.obj.list.items) |item| {
        const comparison = try max.cmp(item);
        if (comparison == .lt) max = item;
    }

    try self.value_stack.append(max);
    self.ip.* += 1;
}
fn listSortAsc(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "sortAsc not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidSortAsc,
        offset,
    );

    if (l.ty.obj.list.items.len == 0) {
        try self.value_stack.append(l);
        self.ip.* += 1;
        return;
    }

    std.sort.sort(Value, l.ty.obj.list.items, {}, Value.lessThan);
    try self.value_stack.append(l);
    self.ip.* += 1;
}
fn listSortDesc(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "sortDesc not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidSortDesc,
        offset,
    );

    if (l.ty.obj.list.items.len == 0) {
        try self.value_stack.append(l);
        self.ip.* += 1;
        return;
    }

    std.sort.sort(Value, l.ty.obj.list.items, {}, Value.greaterThan);
    try self.value_stack.append(l);
    self.ip.* += 1;
}
fn listReverse(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "reverse not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidReverse,
        offset,
    );

    if (l.ty.obj.list.items.len == 0) {
        try self.value_stack.append(l);
        self.ip.* += 1;
        return;
    }

    std.mem.reverse(Value, l.ty.obj.list.items);
    try self.value_stack.append(l);
    self.ip.* += 1;
}
fn strSplit(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const str = self.value_stack.pop();
    if (str.ty != .obj or str.ty.obj.* != .string) return self.ctx.err(
        "split not allowed on {s}.",
        .{@tagName(str.ty)},
        error.InvalidSplit,
        offset,
    );

    const delim = self.value_stack.pop();
    if (delim.ty != .obj or delim.ty.obj.* != .string) return self.ctx.err(
        "split delimiter must be a string",
        .{},
        error.InvalidSplit,
        offset,
    );

    const str_str = std.mem.sliceTo(str.ty.obj.string, 0);
    const delim_str = std.mem.sliceTo(delim.ty.obj.string, 0);

    var list = std.ArrayList(Value).init(self.allocator);
    var iter = std.mem.split(u8, str_str, delim_str);
    while (iter.next()) |sub| {
        try list.append(try Value.newString(self.allocator, sub));
    }

    try self.value_stack.append(try Value.newList(self.allocator, list));
    self.ip.* += 1;
}
fn listJoin(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "join not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidJoin,
        offset,
    );

    const delim = self.value_stack.pop();
    if (delim.ty != .obj or delim.ty.obj.* != .string) return self.ctx.err(
        "join delimiter must be a string",
        .{},
        error.InvalidJoin,
        offset,
    );

    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();
    const str_delim = std.mem.sliceTo(delim.ty.obj.string, 0);

    for (l.ty.obj.list.items) |item, i| {
        if (i != 0 and str_delim.len > 0) try buf.appendSlice(str_delim);
        _ = try writer.print("{}", .{item});
    }
    try buf.append(0);

    try self.value_stack.append(try Value.newStringZ(self.allocator, buf.items));
    self.ip.* += 1;
}
fn strEndsWith(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const str = self.value_stack.pop();
    const ending = self.value_stack.pop();
    if (str.ty != .obj or
        str.ty.obj.* != .string or
        ending.ty != .obj or
        ending.ty.obj.* != .string) return self.ctx.err(
        "endsWith callee and arg must be strings.",
        .{},
        error.InvalidEndsWith,
        offset,
    );

    const result = Value.new(.{ .boolean = std.mem.endsWith(u8, std.mem.sliceTo(str.ty.obj.string, 0), std.mem.sliceTo(ending.ty.obj.string, 0)) });

    try self.value_stack.append(result);
    self.ip.* += 1;
}
fn strStartsWith(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const str = self.value_stack.pop();
    const start = self.value_stack.pop();
    if (str.ty != .obj or
        str.ty.obj.* != .string or
        start.ty != .obj or
        start.ty.obj.* != .string) return self.ctx.err(
        "startsWith callee and arg must be strings.",
        .{},
        error.InvalidStartsWith,
        offset,
    );

    const str_str = std.mem.sliceTo(str.ty.obj.string, 0);
    const start_str = std.mem.sliceTo(start.ty.obj.string, 0);

    const result = Value.new(.{ .boolean = std.mem.startsWith(u8, str_str, start_str) });
    try self.value_stack.append(result);

    self.ip.* += 1;
}
fn listMap(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "map not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidMap,
        offset,
    );

    const f = self.value_stack.pop();
    if (f.ty != .obj or f.ty.obj.* != .func) return self.ctx.err(
        "map requres function argument.",
        .{},
        error.InvalidMap,
        offset,
    );

    var list = try std.ArrayList(Value).initCapacity(self.allocator, l.ty.obj.list.items.len);

    for (l.ty.obj.list.items) |item, i| {
        const v = try self.execListPredicate(f, item, i);
        list.appendAssumeCapacity(v);
    }

    try self.value_stack.append(try Value.newList(self.allocator, list));
    self.ip.* += 1;
}
fn listFilter(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "filter not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidFilter,
        offset,
    );

    const f = self.value_stack.pop();
    if (f.ty != .obj or f.ty.obj.* != .func) return self.ctx.err(
        "filter requires function argument",
        .{},
        error.InvalidFilter,
        offset,
    );

    var list = std.ArrayList(Value).init(self.allocator);

    for (l.ty.obj.list.items) |item, i| {
        const v = try self.execListPredicate(f, item, i);
        if (isTruthy(v)) try list.append(item);
    }

    try self.value_stack.append(try Value.newList(self.allocator, list));
    self.ip.* += 1;
}
fn each(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const container = self.value_stack.pop();
    if (container.ty != .obj or
        container.ty.obj.* != .list and
        container.ty.obj.* != .map) return self.ctx.err(
        "each not allowed on {s}.",
        .{@tagName(container.ty)},
        error.InvalidEach,
        offset,
    );

    const f = self.value_stack.pop();
    if (f.ty != .obj or f.ty.obj.* != .func) return self.ctx.err(
        "each requres function argument.",
        .{},
        error.InvalidEach,
        offset,
    );

    const container_len = switch (container.ty.obj.*) {
        .list => |l| l.items.len,
        .map => |m| m.count(),
        else => unreachable,
    };

    if (container_len == 0) {
        try self.value_stack.append(container);
        self.ip.* += 1;
        return;
    }

    if (container.ty.obj.* == .list) {
        for (container.ty.obj.list.items) |item, i| _ = try self.execListPredicate(f, item, i);
    } else {
        var iter = container.ty.obj.map.iterator();
        var i: usize = 0;
        while (iter.next()) |entry| : (i += 1) _ = try self.execMapPredicate(f, entry.key_ptr.*, entry.value_ptr.*, i);
    }

    try self.value_stack.append(container);
    self.ip.* += 1;
}
fn listReduce(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "reduce not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidReduce,
        offset,
    );

    var acc = self.value_stack.pop();
    const f = self.value_stack.pop();
    if (f.ty != .obj or f.ty.obj.* != .func) return self.ctx.err(
        "reduce requires function argument.",
        .{},
        error.InvalidReduce,
        offset,
    );

    if (l.ty.obj.list.items.len == 0) {
        try self.value_stack.append(Value.new(.nil));
        self.ip.* += 1;
        return;
    }

    // Set up sub-VM arena.
    var vm_arena = std.heap.ArenaAllocator.init(self.allocator);
    defer vm_arena.deinit();
    const vm_allocator = vm_arena.allocator();

    for (l.ty.obj.list.items) |item, i| {
        // Set up function scope.
        var func_scope = Scope.init(vm_allocator, .function);

        // Assign args as locals in function scope.
        try func_scope.map.put("acc", acc);
        try func_scope.map.put("it", item);
        try func_scope.map.put("@0", item);
        if (f.ty.obj.func.params.len > 0) try func_scope.map.put(f.ty.obj.func.params[0], acc);
        if (f.ty.obj.func.params.len > 1) try func_scope.map.put(f.ty.obj.func.params[1], item);
        try func_scope.map.put("index", Value.new(.{ .uint = i }));

        _ = try self.pushScope(func_scope);

        var vm = try init(
            vm_allocator,
            f.ty.obj.func.instructions,
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

    const result = Value.new(.{ .uint = std.rand.DefaultPrng.init(@intCast(usize, std.time.timestamp())).random().uintAtMost(usize, x_val.ty.uint) });
    try self.value_stack.append(result);
}
fn listPush(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "push not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidPush,
        offset,
    );

    var item = self.value_stack.pop();
    try l.ty.obj.list.append(try item.copy(l.ty.obj.list.allocator));
    try self.value_stack.append(l);

    self.ip.* += 1;
}
fn listPop(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    if (l.ty != .obj or l.ty.obj.* != .list) return self.ctx.err(
        "pop not allowed on {s}.",
        .{@tagName(l.ty)},
        error.InvalidPop,
        offset,
    );

    try self.value_stack.append(l.ty.obj.list.pop()); //TODO: Deinit popped value?
    self.ip.* += 1;
}

fn execListPredicate(self: *Vm, func: Value, item: Value, index: usize) anyerror!Value {
    // Assign args as locals in function scope.
    var func_scope = Scope.init(self.allocator, .function);

    const index_val = Value.new(.{ .uint = index });

    try func_scope.map.put("it", item);
    try func_scope.map.put("index", index_val);

    if (func.ty.obj.func.params.len > 0) try func_scope.map.put(func.ty.obj.func.params[0], item);
    if (func.ty.obj.func.params.len > 1) try func_scope.map.put(func.ty.obj.func.params[1], index_val);

    return self.execPredicate(func.ty.obj.func.instructions, func_scope);
}

fn execMapPredicate(self: *Vm, func: Value, key: []const u8, item: Value, index: usize) anyerror!Value {
    // Assign args as locals in function scope.
    var func_scope = Scope.init(self.allocator, .function);

    const key_val = try Value.newString(self.allocator, key);
    const index_val = Value.new(.{ .uint = index });

    try func_scope.map.put("key", key_val);
    try func_scope.map.put("value", item);
    try func_scope.map.put("index", Value.new(.{ .uint = index }));

    if (func.ty.obj.func.params.len > 0) try func_scope.map.put(func.ty.obj.func.params[0], key_val);
    if (func.ty.obj.func.params.len > 1) try func_scope.map.put(func.ty.obj.func.params[1], item);
    if (func.ty.obj.func.params.len > 2) try func_scope.map.put(func.ty.obj.func.params[2], index_val);

    return self.execPredicate(func.ty.obj.func.instructions, func_scope);
}

fn execPredicate(self: *Vm, instructions: []const u8, func_scope: Scope) anyerror!Value {
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
    if (filename.ty != .obj or filename.ty.obj.* != .string) return self.ctx.err(
        "Redirection filename must evaluate to a string",
        .{},
        error.InvalidRedirect,
        offset,
    );

    // Open file
    var create_flags: std.fs.File.CreateFlags = .{};
    if (!clobber) create_flags.truncate = false;
    var file = try std.fs.cwd().createFile(std.mem.sliceTo(filename.ty.obj.string, 0), create_flags);
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

fn strToLower(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const s = self.value_stack.pop();
    if (s.ty != .obj or s.ty.obj.* != .string) return self.ctx.err(
        "toLower not allowed on {s}.",
        .{@tagName(s.ty)},
        error.InvalidtoLower,
        offset,
    );

    if (std.mem.sliceTo(s.ty.obj.string, 0).len == 0) {
        try self.value_stack.append(s);
        self.ip.* += 1;
        return;
    }

    const lower_str = try ziglyph.toLowerStr(self.allocator, std.mem.sliceTo(s.ty.obj.string, 0));
    const lower_val = try Value.newString(self.allocator, lower_str);

    try self.value_stack.append(lower_val);
    self.ip.* += 1; // num_args
}
fn strToUpper(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const s = self.value_stack.pop();
    if (s.ty != .obj or s.ty.obj.* != .string) return self.ctx.err(
        "toUpper not allowed on {s}.",
        .{@tagName(s.ty)},
        error.InvalidtoUpper,
        offset,
    );

    if (std.mem.sliceTo(s.ty.obj.string, 0).len == 0) {
        try self.value_stack.append(s);
        self.ip.* += 1;
        return;
    }

    const upper_str = try ziglyph.toUpperStr(self.allocator, std.mem.sliceTo(s.ty.obj.string, 0));
    const upper_val = try Value.newString(self.allocator, upper_str);

    try self.value_stack.append(upper_val);
    self.ip.* += 1; // num_args
}

// Scopes

fn pushScope(self: *Vm, scope: Scope) !void {
    try self.scope_stack.push(scope);
}

fn popScope(self: *Vm) Scope {
    return self.scope_stack.pop();
}

// Helpers
fn inObjectTypes(value: Value, types: []const std.meta.Tag(Value.Object)) bool {
    if (value.ty != .obj) return false;
    return for (types) |ty| {
        if (value.ty.obj.* == ty) break true;
    } else false;
}

fn isTruthy(value: Value) bool {
    return switch (value.ty) {
        .boolean => |b| b,
        .float => |f| f != 0.0,
        .int => |i| i != 0,
        .obj => |o| obj: {
            break :obj switch (o.*) {
                .func => true,
                .list => |l| l.items.len != 0,
                .map => |m| m.count() != 0,
                .range => |r| r[1] - r[0] != 0,
                .string => |s| std.mem.sliceTo(s, 0).len != 0,
            };
        },
        .uint => |u| u != 0,

        else => false,
    };
}

fn getNumber(self: Vm, comptime T: type, start: usize, n: usize) T {
    return std.mem.bytesAsSlice(T, self.instructions[start .. start + n])[0];
}

fn getOffset(self: Vm) u16 {
    return self.getU16(self.ip.*);
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

    var compiler = try Compiler.init(allocator, ctx);
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

    got = try testVmValue(allocator, "false");
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(false, got.ty.boolean);

    got = try testVmValue(allocator, "nil");
    try std.testing.expectEqual(Value.Tag.nil, got.ty);

    got = try testVmValue(allocator, "3.1415");
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 3.1415), got.ty.float);

    got = try testVmValue(allocator, "3.1415 3.1415");
    try std.testing.expectEqual(Value.Tag.float, got.ty);
    try std.testing.expectEqual(@as(f64, 3.1415), got.ty.float);

    got = try testVmValue(allocator, "-3");
    try std.testing.expectEqual(Value.Tag.int, got.ty);
    try std.testing.expectEqual(@as(i64, -3), got.ty.int);

    got = try testVmValue(allocator, "-3 -3");
    try std.testing.expectEqual(Value.Tag.int, got.ty);
    try std.testing.expectEqual(@as(i64, -3), got.ty.int);

    got = try testVmValue(allocator, "9");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 9), got.ty.uint);

    got = try testVmValue(allocator, "9 9");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 9), got.ty.uint);
}

test "Vm strings" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "\"foobar\"");
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("foobar", std.mem.sliceTo(got.ty.obj.string, 0));

    got = try testVmValue(allocator, "\"foobar\" \"foobar\"");
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("foobar", std.mem.sliceTo(got.ty.obj.string, 0));

    got = try testVmValue(allocator,
        \\"foo {#d:0>3# 2} bar"
    );
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("foo 002 bar", std.mem.sliceTo(got.ty.obj.string, 0));
}

test "Vm function literal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\{ foo, bar => 1 }
    );
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqual(@as(usize, 2), got.ty.obj.func.params.len);
    try std.testing.expectEqualStrings("foo", got.ty.obj.func.params[0]);
    try std.testing.expectEqualStrings("bar", got.ty.obj.func.params[1]);
    try std.testing.expectEqual(@as(usize, 12), got.ty.obj.func.instructions.len);
    try std.testing.expectEqual(Compiler.Opcode.uint, @intToEnum(Compiler.Opcode, got.ty.obj.func.instructions[0]));
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
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("foobar", std.mem.sliceTo(got.ty.obj.string, 0));

    got = try testVmValue(allocator,
        \\"-" ** 3
    );
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("---", std.mem.sliceTo(got.ty.obj.string, 0));

    got = try testVmValue(allocator, "true and false");
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(false, got.ty.boolean);

    got = try testVmValue(allocator, "false or true");
    try std.testing.expectEqual(Value.Tag.boolean, got.ty);
    try std.testing.expectEqual(true, got.ty.boolean);
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
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqual(@as(usize, 3), got.ty.obj.list.items.len);
    try std.testing.expectEqual(Value.Tag.uint, got.ty.obj.list.items[0].ty);
    try std.testing.expectEqual(@as(u64, 1), got.ty.obj.list.items[0].ty.uint);
}

test "Vm map literal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\["a": 1, "b": 2]
    );
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqual(@as(usize, 2), got.ty.obj.map.count());
    try std.testing.expectEqual(Value.Tag.uint, got.ty.obj.map.get("a").?.ty);
    try std.testing.expectEqual(@as(u64, 1), got.ty.obj.map.get("a").?.ty.uint);
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
    //try testLastValue("rand(10)", Value.new(.{ .uint = 10 }));
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
        \\["a": 3, "b": 2, "c": 1].keysByValueAsc()[0]
    );
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("c", std.mem.sliceTo(got.ty.obj.string, 0));

    got = try testVmValue(allocator,
        \\["a": 3, "b": 2, "c": 1].keysByValueDesc()[0]
    );
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("a", std.mem.sliceTo(got.ty.obj.string, 0));

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
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("B", std.mem.sliceTo(got.ty.obj.string, 0));

    got = try testVmValue(allocator, "[2, 3, 1].sortAsc()[0]");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 1), got.ty.uint);

    got = try testVmValue(allocator, "[2, 3, 1].sortDesc()[0]");
    try std.testing.expectEqual(Value.Tag.uint, got.ty);
    try std.testing.expectEqual(@as(u64, 3), got.ty.uint);

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

    got = try testVmValue(allocator,
        \\"foo,bar,baz".split(",")[1]
    );
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("bar", std.mem.sliceTo(got.ty.obj.string, 0));

    got = try testVmValue(allocator,
        \\["foo", 1, 2.3, nil].join(",")
    );
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("foo,1,2.3,", std.mem.sliceTo(got.ty.obj.string, 0));

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
    //, Value.new(.nil), "foo,1,2,3.14");
    //try testLastValueWithOutput(
    //    \\print("foo", 1, 2, 3.14, "foo {1}")
    //, Value.new(.nil), "foo,1,2,3.14,foo 1");
    //try testLastValueWithOutput(
    //    \\print("foo", 1, 2, 3.14, "{#d:0>3# 1}")
    //, Value.new(.nil), "foo,1,2,3.14,001");
    //try testLastValue(
    //, Value.new(.{ .string = "\u{65}\u{301}" }));
    //
    got = try testVmValue(allocator,
        \\"H\u65\u301llo".chars()[1]
    );
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("\u{65}\u{301}", std.mem.sliceTo(got.ty.obj.string, 0));

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

    got = try testVmValue(allocator,
        \\"FOO".toLower()
    );
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("foo", std.mem.sliceTo(got.ty.obj.string, 0));

    got = try testVmValue(allocator,
        \\"foo".toUpper()
    );
    try std.testing.expectEqual(Value.Tag.obj, got.ty);
    try std.testing.expectEqualStrings("FOO", std.mem.sliceTo(got.ty.obj.string, 0));
}
