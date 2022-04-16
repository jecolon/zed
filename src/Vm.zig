const std = @import("std");

const Compiler = @import("Compiler.zig");
const Context = @import("Context.zig");
const GraphemeIterator = @import("ziglyph").GraphemeIterator;
const Node = @import("Node.zig");
const Scope = @import("Scope.zig");
const ScopeStack = @import("ScopeStack.zig");
const Token = @import("Token.zig");
const value = @import("value.zig");
const Value = value.Value;
const runtimePrint = @import("fmt.zig").runtimePrint;

const p2z = @import("pcre2zig");
const ziglyph = @import("ziglyph");

const ObjectTag = std.meta.Tag(Value.Object);

allocator: std.mem.Allocator,
ctx: Context,
last_popped: Value = value.val_nil,
output: *std.ArrayList(u8),

bytecode: []const u8 = undefined,
ip: *u16 = undefined,

scope_stack: *ScopeStack,
frame_stack: std.ArrayList(Frame),
value_stack: std.ArrayList(Value),

const Vm = @This();

pub fn init(
    allocator: std.mem.Allocator,
    bytecode: []const u8,
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
    try self.pushFrame(bytecode);
    return self;
}

pub fn run(self: *Vm) !void {
    while (self.ip.* < self.bytecode.len) {
        const opcode = @intToEnum(Compiler.Opcode, self.bytecode[self.ip.*]);

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
            .plain, .raw_str => try self.execPlain(),
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

                try self.execReturn();
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
            // Regex
            .match, .nomatch => try self.execMatch(opcode),
            .matcher => try self.execMatcher(),
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
    try self.value_stack.append(value.val_false);
    self.ip.* += 2;
}
fn execBoolTrue(self: *Vm) !void {
    self.ip.* += 1;
    try self.value_stack.append(value.val_true);
    self.ip.* += 2;
}
fn execNil(self: *Vm) !void {
    self.ip.* += 1;
    try self.value_stack.append(value.val_nil);
    self.ip.* += 2;
}

fn execFloat(self: *Vm) !void {
    self.ip.* += 1;
    const f = self.getNumber(f64, self.ip.*, 8);
    self.ip.* += 8;
    try self.value_stack.append(value.floatToValue(f));
}
fn execInt(self: *Vm) !void {
    self.ip.* += 1;
    const i = self.getNumber(i32, self.ip.*, 4);
    self.ip.* += 4;
    try self.value_stack.append(value.intToValue(i));
}
fn execUint(self: *Vm) !void {
    self.ip.* += 1;
    const u = self.getNumber(u32, self.ip.*, 4);
    self.ip.* += 4;
    try self.value_stack.append(value.uintToValue(u));
}

fn execFormat(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const spec = std.mem.sliceTo(self.bytecode[self.ip.*..], 0);
    self.ip.* += @intCast(u16, spec.len) + 1;

    const v = self.value_stack.pop();
    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();
    runtimePrint(
        self.allocator,
        spec,
        v,
        writer,
    ) catch |err| return self.ctx.err(
        "Error in string interpolation #{s}#.",
        .{spec},
        err,
        offset,
    );

    if (buf.items.len < 7) {
        try self.value_stack.append(value.strToValue(buf.items));
    } else {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .string = buf.items };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
    }
}
fn execPlain(self: *Vm) !void {
    self.ip.* += 1;
    const str = std.mem.sliceTo(self.bytecode[self.ip.*..], 0);
    self.ip.* += @intCast(u16, str.len) + 1;

    if (str.len < 7) {
        try self.value_stack.append(value.strToValue(str));
    } else {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .string = str };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
    }
}
fn execString(self: *Vm) !void {
    self.ip.* += 1;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;

    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();
    var i: usize = 0;
    while (i < len) : (i += 1) try value.print(self.value_stack.pop(), writer);

    if (buf.items.len < 7) {
        try self.value_stack.append(value.strToValue(buf.items));
    } else {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .string = buf.items };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
    }
}

fn execFunc(self: *Vm) !void {
    self.ip.* += 1;

    // Bytes to skip if cached function.
    const skip_bytes = self.getU16(self.ip.*);
    self.ip.* += 2;

    // Func string
    const func_str = try self.scope_stack.allocator.dupe(u8, std.mem.sliceTo(self.bytecode[self.ip.*..], 0));
    self.ip.* += @intCast(u16, func_str.len) + 1;

    // Check for cached function.
    if (self.scope_stack.value_cache.get(func_str)) |v| {
        try self.value_stack.append(v);
        self.ip.* += skip_bytes;
        return;
    }

    // Function name
    var func_name: ?[]const u8 = null;
    if (self.bytecode[self.ip.*] != 0) {
        func_name = try self.scope_stack.allocator.dupe(u8, std.mem.sliceTo(self.bytecode[self.ip.*..], 0));
        self.ip.* += @intCast(u16, func_name.?.len) + 1;
    } else {
        self.ip.* += 1;
    }

    // function params
    const params_len = self.getU16(self.ip.*);
    self.ip.* += 2;

    var params: ?[]const u16 = null;
    if (params_len != 0) {
        var params_slice = try self.scope_stack.allocator.alloc(u16, params_len);
        for (params_slice) |_, i| {
            const param = std.mem.sliceTo(self.bytecode[self.ip.*..], 0);
            params_slice[i] = self.ip.*;
            self.ip.* += @intCast(u16, param.len) + 1;
        }
        params = params_slice;
    }

    // Function bytecode
    const bytecode_len = self.getU16(self.ip.*);
    self.ip.* += 2;
    const func_bytecode: ?[]const u8 = if (bytecode_len == 0) null else self.bytecode[self.ip.* .. self.ip.* + bytecode_len];
    self.ip.* += bytecode_len;

    const func = value.Function{
        .str = func_str,
        .name = func_name,
        .params = params,
        .bytecode = func_bytecode,
    };

    const obj_ptr = try self.scope_stack.allocator.create(value.Object);
    obj_ptr.* = .{ .func = func };
    const obj_addr = @ptrToInt(obj_ptr);

    const obj_val = value.addrToValue(obj_addr);
    // Cache for further re-use.
    const func_str_copy = try self.scope_stack.allocator.dupe(u8, func_str);
    try self.scope_stack.value_cache.put(func_str_copy, obj_val);

    try self.value_stack.append(obj_val);
}

fn execBuiltin(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const builtin = @intToEnum(Token.Tag, self.bytecode[self.ip.*]);

    return switch (builtin) {
        .pd_atan2 => self.execAtan2(),
        .pd_capture => self.execCapture(),
        .pd_chars => self.execChars(),
        .pd_col => self.execCol(),
        .pd_contains => self.execContains(),
        .pd_cos => self.execOneArgMath(builtin),
        .pd_each => self.execEach(),
        .pd_endsWith => self.execStrEndStart(false),
        .pd_exp => self.execOneArgMath(builtin),
        .pd_filter => self.execFilter(),
        .pd_join => self.execListJoin(),
        .pd_indexOf => self.execIndexOf(),
        .pd_int => self.execOneArgMath(builtin),
        .pd_keys => self.execMapKeys(),
        .pd_keysByValueAsc => self.execMapKeysByValue(true),
        .pd_keysByValueDesc => self.execMapKeysByValue(false),
        .pd_lastIndexOf => self.execLastIndexOf(),
        .pd_len => self.execLen(),
        .pd_log => self.execOneArgMath(builtin),
        .pd_map => self.execMapMethod(),
        .pd_max => self.execListMax(),
        .pd_mean => self.execListMean(),
        .pd_median => self.execListMedian(),
        .pd_memo => self.execMemo(),
        .pd_min => self.execListMin(),
        .pd_mode => self.execListMode(),
        .pd_next => self.execNext(),
        .pd_print => self.execPrint(),
        .pd_pop => self.execListPop(),
        .pd_push => self.execListPush(),
        .pd_rand => self.execRand(),
        .pd_reduce => self.execReduce(),
        .pd_replace => self.execReplace(),
        .pd_reset => self.execReset(),
        .pd_reverse => self.execListReverse(),
        .pd_sin => self.execOneArgMath(builtin),
        .pd_sortAsc => self.execListSort(true),
        .pd_sortDesc => self.execListSort(false),
        .pd_split => self.execStrSplit(),
        .pd_sqrt => self.execOneArgMath(builtin),
        .pd_startsWith => self.execStrEndStart(true),
        .pd_stdev => self.execListStdev(),
        .pd_toLower => self.execStrCase(true),
        .pd_toUpper => self.execStrCase(false),
        .pd_unique => self.execUnique(),
        .pd_values => self.execMapValues(),
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
    const func_obj_ptr = (value.asFunc(callee)) orelse return self.ctx.err(
        "{} is not callable.",
        .{callee},
        error.InvalidCall,
        offset,
    );

    // Empty func?
    if (func_obj_ptr.func.bytecode == null) {
        try self.value_stack.append(value.val_nil);
        self.ip.* += 1;
        return;
    }

    // Memoized function?
    if (func_obj_ptr.func.memo) return self.execCallMemo(func_obj_ptr);

    // Prepare the child scope.
    var func_scope = Scope.init(self.allocator, .function);

    // Self-references
    if (func_obj_ptr.func.name) |name| try func_scope.map.put(name, callee);

    // Process args
    const num_args = self.bytecode[self.ip.*];
    self.ip.* += 1;

    var i: usize = 0;
    while (i < num_args) : (i += 1) {
        const arg = self.value_stack.pop();
        if (i == 0) try func_scope.map.put("it", arg); // it
        var buf: [4]u8 = undefined;
        const auto_arg_name = try std.fmt.bufPrint(&buf, "@{}", .{i});
        try func_scope.map.put(try self.allocator.dupe(u8, auto_arg_name), arg); // @0, @1, ...
        if (func_obj_ptr.func.params) |params| {
            if (i < params.len) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[i]..], 0), arg);
        }
    }

    // Push the function's frame.
    try self.pushScope(func_scope);
    try self.pushFrame(func_obj_ptr.func.bytecode.?);
}
fn execCallMemo(self: *Vm, func_obj_ptr: *value.Object) !void {
    // Process args
    const num_args = self.bytecode[self.ip.*];
    self.ip.* += 1;

    // Hash and check for memoized result.
    var wh = std.hash.Wyhash.init(Context.seed);
    wh.update(func_obj_ptr.func.str);
    var args_buf: [256]Value = undefined;
    var i: usize = 0;
    while (i < num_args) : (i += 1) args_buf[i] = self.value_stack.pop();
    const args = args_buf[0..i];
    wh.update(std.mem.sliceAsBytes(args));
    const memo_hash = wh.final();

    if (self.scope_stack.func_memo.get(memo_hash)) |v| {
        try self.value_stack.append(v);
        return;
    }

    // Prepare the child scope.
    var func_scope = Scope.init(self.allocator, .function);

    // Signal need to memo.
    try func_scope.map.put("@memo", memo_hash);

    // Self-references
    if (func_obj_ptr.func.name) |name| {
        const addr = @ptrToInt(func_obj_ptr);
        try func_scope.map.put(name, value.addrToValue(addr));
    }

    for (args) |arg, j| {
        if (j == 0) try func_scope.map.put("it", arg); // it
        var buf: [4]u8 = undefined;
        const auto_arg_name = try std.fmt.bufPrint(&buf, "@{}", .{j});
        try func_scope.map.put(try self.allocator.dupe(u8, auto_arg_name), arg); // @0, @1, ...
        if (func_obj_ptr.func.params) |params| {
            if (j < params.len) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[j]..], 0), arg);
        }
    }

    // Push the function's frame.
    try self.pushScope(func_scope);
    try self.pushFrame(func_obj_ptr.func.bytecode.?);
}
fn execReturn(self: *Vm) !void {
    self.popFrame();
    // Unwind scopes up to the function's scope.
    while (true) {
        var popped_scope = self.popScope();

        // Signal to memoize?
        if (popped_scope.map.get("@memo")) |hash| {
            try self.scope_stack.func_memo.put(hash, self.value_stack.items[self.value_stack.items.len - 1]);
        }

        popped_scope.map.deinit();
        if (popped_scope.ty == .function) break;
    }
}

fn execDefine(self: *Vm) !void {
    // Offset
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    // Name
    const name = std.mem.sliceTo(self.bytecode[self.ip.*..], 0);
    self.ip.* += @intCast(u16, name.len) + 1;
    // Is it already defined?
    if (self.scope_stack.isDefined(name)) return self.ctx.err(
        "{s} is already defined.",
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
    const name = std.mem.sliceTo(self.bytecode[self.ip.*..], 0);
    self.ip.* += @intCast(u16, name.len) + 1;
    // Is the name defined?
    if (!self.scope_stack.isDefined(name)) return self.ctx.err(
        "{s} is not defined.",
        .{name},
        error.NameUndefined,
        offset,
    );
    // Load
    try self.value_stack.append((self.scope_stack.load(name)).?);
}
fn execStore(self: *Vm) !void {
    // Offset
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    // Combo assign
    const combo = @intToEnum(Node.Combo, self.bytecode[self.ip.*]);
    self.ip.* += 1;
    // Name
    const name = std.mem.sliceTo(self.bytecode[self.ip.*..], 0);
    self.ip.* += @intCast(u16, name.len) + 1;
    // Is the name defined?
    if (!self.scope_stack.isDefined(name)) return self.ctx.err(
        "{s} is not defined.",
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
        const old_value = (self.scope_stack.load(name)).?;

        const new_value = switch (combo) {
            .none => unreachable,
            .add => try value.add(old_value, rvalue),
            .sub => try value.sub(old_value, rvalue),
            .mul => try value.mul(old_value, rvalue),
            .div => try value.div(old_value, rvalue),
            .mod => try value.mod(old_value, rvalue),
        };

        try self.scope_stack.update(name, new_value);
        try self.value_stack.append(new_value);
    }
}
fn execGlobal(self: *Vm) !void {
    self.ip.* += 1;
    const global = @intToEnum(Token.Tag, self.bytecode[self.ip.*]);
    self.ip.* += 1;

    const result = switch (global) {
        .at_cols => self.scope_stack.columns,
        .at_file => self.scope_stack.file,
        .at_frnum => value.uintToValue(@intCast(u32, self.scope_stack.frnum)),
        .at_head => hd: {
            if (self.scope_stack.header_row) |hr| {
                break :hd value.uintToValue(@intCast(u32, hr));
            } else {
                break :hd value.val_nil;
            }
        },
        .at_headers => hdrs: {
            if (self.scope_stack.header_row == null) break :hdrs value.val_nil;

            var keys_list = std.ArrayList(Value).init(self.allocator);

            for (self.scope_stack.headers.keys()) |key| {
                if (key.len < 7) {
                    try keys_list.append(value.strToValue(key));
                } else {
                    const obj_ptr = try self.allocator.create(value.Object);
                    obj_ptr.* = .{ .string = key };
                    const obj_addr = @ptrToInt(obj_ptr);
                    try keys_list.append(value.addrToValue(obj_addr));
                }
            }

            const obj_ptr = try self.allocator.create(value.Object);
            obj_ptr.* = .{ .list = keys_list };
            const addr = @ptrToInt(obj_ptr);
            break :hdrs value.addrToValue(addr);
        },
        .at_ics => self.scope_stack.ics,
        .at_irs => self.scope_stack.irs,
        .at_ocs => self.scope_stack.ocs,
        .at_ors => self.scope_stack.ors,
        .at_rec => self.scope_stack.record,
        .at_rnum => value.uintToValue(@intCast(u32, self.scope_stack.rnum)),
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
    const global = @intToEnum(Token.Tag, self.bytecode[self.ip.*]);
    self.ip.* += 1;
    // Get the value to assign.
    const rvalue = self.value_stack.pop();

    switch (global) {
        .at_ics => {
            if (!value.isAnyStr(rvalue)) return self.ctx.err(
                "@ics must be a string.",
                .{},
                error.InvalidIcs,
                offset,
            );
            self.scope_stack.ics = rvalue;
        },
        .at_irs => {
            if (!value.isAnyStr(rvalue)) return self.ctx.err(
                "@irs must be a string.",
                .{},
                error.InvalidIrs,
                offset,
            );
            self.scope_stack.irs = rvalue;
        },
        .at_ocs => {
            if (!value.isAnyStr(rvalue)) return self.ctx.err(
                "@ocs must be a string.",
                .{},
                error.InvalidOcs,
                offset,
            );
            self.scope_stack.ocs = rvalue;
        },
        .at_ors => {
            if (!value.isAnyStr(rvalue)) return self.ctx.err(
                "@ors must be a string.",
                .{},
                error.InvalidOrs,
                offset,
            );
            self.scope_stack.ors = rvalue;
        },
        .at_rec => {
            if (!value.isAnyStr(rvalue)) return self.ctx.err(
                "@rec must be a string.",
                .{},
                error.InvalidRec,
                offset,
            );
            self.scope_stack.record = rvalue;
        },
        .at_cols => {
            if (value.asList(rvalue) == null) return self.ctx.err(
                "@cols must be a list.",
                .{},
                error.InvalidCols,
                offset,
            );
            self.scope_stack.columns = rvalue;
        },
        .at_head => {
            if (value.asUint(rvalue)) |u| {
                self.scope_stack.header_row = u;
            } else return self.ctx.err(
                "@head must be an unsigned integer.",
                .{},
                error.InvalidHead,
                offset,
            );
        },
        .at_headers => {
            const hlist = value.asList(rvalue) orelse return self.ctx.err(
                "@headers must be a list.",
                .{},
                error.InvalidHeaders,
                offset,
            );

            self.scope_stack.headers.clearRetainingCapacity();

            for (hlist.list.items) |v, i| {
                const h_str = if (value.unboxStr(v)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(v).?.string;
                const h_copy = try self.scope_stack.allocator.dupe(u8, h_str);
                try self.scope_stack.headers.put(h_copy, i);
            }
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

    if (value.add(left, right)) |sum| {
        try self.value_stack.append(sum);
    } else |err| return self.ctx.err(
        "Invalid addition.",
        .{},
        err,
        offset,
    );
}
fn execSub(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (value.sub(left, right)) |diff| {
        try self.value_stack.append(diff);
    } else |err| return self.ctx.err(
        "Invalid subtraction.",
        .{},
        err,
        offset,
    );
}
fn execMul(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (value.mul(left, right)) |product| {
        try self.value_stack.append(product);
    } else |err| return self.ctx.err(
        "Invalid multiplication.",
        .{},
        err,
        offset,
    );
}
fn execDiv(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (value.div(left, right)) |quotient| {
        try self.value_stack.append(quotient);
    } else |err| return self.ctx.err(
        "Invalid division.",
        .{},
        err,
        offset,
    );
}
fn execMod(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (value.mod(left, right)) |remainder| {
        try self.value_stack.append(remainder);
    } else |err| return self.ctx.err(
        "Invalid modulo.",
        .{},
        err,
        offset,
    );
}
fn execComparison(self: *Vm, opcode: Compiler.Opcode) !void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const comparison = value.cmp(left, right) catch |err| return self.ctx.err(
        "Invalie comparison.",
        .{},
        err,
        offset,
    );

    const result = switch (opcode) {
        .lt => value.boolToValue(comparison == .lt),
        .lte => value.boolToValue(comparison == .lt or comparison == .eq),
        .gt => value.boolToValue(comparison == .gt),
        .gte => value.boolToValue(comparison == .gt or comparison == .eq),
        else => unreachable,
    };

    try self.value_stack.append(result);
}
fn execEqNeq(self: *Vm, opcode: Compiler.Opcode) !void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 3;

    var comparison = value.eql(left, right);
    if (opcode == .neq) comparison = !comparison;
    try self.value_stack.append(value.boolToValue(comparison));
}
fn execConcat(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (!value.isAnyStr(left) or !value.isAnyStr(right)) return self.ctx.err(
        "Invlid concatenation.",
        .{},
        error.InvalidConcat,
        offset,
    );

    const str_left = if (value.unboxStr(left)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(left).?.string;
    const str_right = if (value.unboxStr(right)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(right).?.string;

    var buf = try self.allocator.alloc(u8, str_left.len + str_right.len);
    std.mem.copy(u8, buf, str_left);
    std.mem.copy(u8, buf[str_left.len..], str_right);

    if (buf.len < 7) {
        try self.value_stack.append(value.strToValue(buf));
    } else {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .string = buf };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
    }
}
fn execRepeat(self: *Vm) anyerror!void {
    const right = self.value_stack.pop();
    const left = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (!value.isAnyStr(left) or !value.isUint(right)) return self.ctx.err(
        "Invalid string repeat.",
        .{},
        error.InvalidRepeat,
        offset,
    );

    const str_left = if (value.unboxStr(left)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(left).?.string;

    const n = value.asUint(right) orelse return self.ctx.err(
        "`repeat` arg must be an unsigned integer.",
        .{},
        error.InvalidRepeat,
        offset,
    );

    var buf = try self.allocator.alloc(u8, str_left.len * n);
    var i: usize = 0;
    while (i < n) : (i += 1) std.mem.copy(u8, buf[str_left.len * i ..], str_left);

    if (buf.len < 7) {
        try self.value_stack.append(value.strToValue(buf));
    } else {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .string = buf };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
    }
}

fn execNot(self: *Vm) !void {
    const v = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    if (!value.isBool(v)) return self.ctx.err(
        "Logical not on non-boolean.",
        .{},
        error.InvalidNot,
        offset,
    );
    try self.value_stack.append(value.boolToValue(!value.asBool(v).?));
}
fn execNeg(self: *Vm) !void {
    const v = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    if (value.asFloat(v)) |f| return self.value_stack.append(value.floatToValue(-f));
    if (value.asInt(v)) |i| return self.value_stack.append(value.intToValue(-i));
    if (value.asUint(v)) |u| return self.value_stack.append(value.floatToValue(-@intToFloat(f64, u)));

    return self.ctx.err(
        "Negation of non-number.",
        .{},
        error.InvalidNeg,
        offset,
    );
}

fn execList(self: *Vm) !void {
    self.ip.* += 1;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;

    var list = try std.ArrayList(Value).initCapacity(self.allocator, len);

    if (len == 0) {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .list = list };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
        return;
    }

    var i: usize = 0;
    while (i < len) : (i += 1) list.appendAssumeCapacity(self.value_stack.pop());

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .list = list };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
}
fn execMap(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const len = self.getU16(self.ip.*);
    self.ip.* += 2;

    var map = std.StringHashMap(Value).init(self.allocator);

    if (len == 0) {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .map = map };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
        return;
    }

    try map.ensureTotalCapacity(len);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        const v = self.value_stack.pop();
        const key = self.value_stack.pop();

        if (!value.isAnyStr(key)) return self.ctx.err(
            "Map keys must be strings.",
            .{},
            error.InvalidMapKey,
            offset,
        );

        const key_str = if (value.unboxStr(key)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(key).?.string;
        const key_copy = try map.allocator.dupe(u8, key_str);
        map.putAssumeCapacity(key_copy, v);
    }

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .map = map };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
}

fn execSubscript(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    const container = self.value_stack.pop();

    if (value.asList(container) == null and value.asMap(container) == null) return self.ctx.err(
        "Subscript on non-container.",
        .{},
        error.InvalidSubscript,
        offset,
    );

    return if (value.asList(container) != null)
        try self.execSubscriptList(container, offset)
    else
        try self.execSubscriptMap(container, offset);
}
fn execSubscriptList(self: *Vm, list: Value, offset: u16) !void {
    const index = self.value_stack.pop();
    if (!value.isUint(index) and value.asRange(index) == null) return self.ctx.err(
        "Invalid subscript index.",
        .{},
        error.InvalidSubscript,
        offset,
    );

    const list_obj_ptr = value.asList(list).?;

    if (value.asUint(index)) |index_u| {
        if (index_u >= list_obj_ptr.list.items.len) return self.ctx.err(
            "Index out of bounds.",
            .{},
            error.InvalidSubscript,
            offset,
        );

        try self.value_stack.append(list_obj_ptr.list.items[index_u]);
    } else {
        const range_obj_ptr = value.asRange(index).?;

        if (range_obj_ptr.range[1] > list_obj_ptr.list.items.len) return self.ctx.err(
            "Range end out of bounds.",
            .{},
            error.InvalidSubscript,
            offset,
        );

        var new_list = try std.ArrayList(Value).initCapacity(self.allocator, range_obj_ptr.range[1] - range_obj_ptr.range[0]);
        for (list_obj_ptr.list.items[range_obj_ptr.range[0]..range_obj_ptr.range[1]]) |item|
            new_list.appendAssumeCapacity(item);

        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .list = new_list };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
    }
}
fn execSubscriptMap(self: *Vm, map: Value, offset: u16) !void {
    const key = self.value_stack.pop();
    if (!value.isAnyStr(key)) return self.ctx.err(
        "Map keys must be strings.",
        .{},
        error.InvalidSubscript,
        offset,
    );

    const key_str = if (value.unboxStr(key)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(key).?.string;
    const map_obj_ptr = value.asMap(map).?;
    const v = if (map_obj_ptr.map.get(key_str)) |vv| vv else value.val_nil;
    try self.value_stack.append(v);
}
fn execSet(self: *Vm) !void {
    const container = self.value_stack.pop();
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;
    // Combo assign
    const combo = @intToEnum(Node.Combo, self.bytecode[self.ip.*]);
    self.ip.* += 1;

    if (value.asList(container) == null and value.asMap(container) == null) return self.ctx.err(
        "Subscript assign on non-container.",
        .{},
        error.InvalidSubscript,
        offset,
    );

    return if (value.asList(container) != null)
        try self.execSetList(container, offset, combo)
    else
        try self.execSetMap(container, offset, combo);
}
fn execSetList(self: *Vm, list: Value, offset: u16, combo: Node.Combo) !void {
    const index = self.value_stack.pop();
    const index_u = value.asUint(index) orelse return self.ctx.err(
        "List subscript must be unsigned integer.",
        .{},
        error.InvalidSubscript,
        offset,
    );

    const list_obj_ptr = value.asList(list).?;

    if (index_u >= list_obj_ptr.list.items.len) return self.ctx.err(
        "Index out of bounds.",
        .{},
        error.InvalidSubscript,
        offset,
    );
    const rvalue = self.value_stack.pop();

    // Store
    if (combo == .none) {
        list_obj_ptr.list.items[index_u] = rvalue;
        try self.value_stack.append(rvalue);
    } else {
        const old_value = list_obj_ptr.list.items[index_u];

        const new_value = switch (combo) {
            .none => unreachable,
            .add => try value.add(old_value, rvalue),
            .sub => try value.sub(old_value, rvalue),
            .mul => try value.mul(old_value, rvalue),
            .div => try value.div(old_value, rvalue),
            .mod => try value.mod(old_value, rvalue),
        };

        list_obj_ptr.list.items[index_u] = new_value;
        try self.value_stack.append(new_value);
    }
}
fn execSetMap(self: *Vm, map: Value, offset: u16, combo: Node.Combo) !void {
    const key = self.value_stack.pop();
    if (!value.isAnyStr(key)) return self.ctx.err(
        "Map keys must be strings.",
        .{},
        error.InvalidSubscript,
        offset,
    );

    const rvalue = self.value_stack.pop();
    const key_str = if (value.unboxStr(key)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(key).?.string;
    const map_obj_ptr = value.asMap(map).?;
    const key_copy = try map_obj_ptr.map.allocator.dupe(u8, key_str);

    // Store
    if (combo == .none) {
        try map_obj_ptr.map.put(key_copy, try value.copy(rvalue, map_obj_ptr.map.allocator));
        try self.value_stack.append(rvalue);
    } else {
        const old_value = map_obj_ptr.map.get(key_str) orelse value.uintToValue(0);

        const new_value = switch (combo) {
            .none => unreachable,
            .add => try value.add(old_value, rvalue),
            .sub => try value.sub(old_value, rvalue),
            .mul => try value.mul(old_value, rvalue),
            .div => try value.div(old_value, rvalue),
            .mod => try value.mod(old_value, rvalue),
        };

        try map_obj_ptr.map.put(key_copy, try value.copy(new_value, map_obj_ptr.map.allocator));
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
    const inclusive = self.bytecode[self.ip.*] == 1;
    self.ip.* += 1;

    const to = self.value_stack.pop();
    const from = self.value_stack.pop();

    if (!value.isUint(from) or !value.isUint(to)) return self.ctx.err(
        "Invalid range.",
        .{},
        error.InvalidRange,
        offset,
    );

    const from_uint = value.asUint(from).?;
    var to_uint = value.asUint(to).?;
    if (inclusive) to_uint += 1;

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .range = [2]u32{ from_uint, to_uint } };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
}

fn execRecRange(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const range_id = self.bytecode[self.ip.*];
    self.ip.* += 1;
    const exclusive = self.bytecode[self.ip.*] == 1;
    self.ip.* += 1;

    const len = self.getU16(self.ip.*);
    self.ip.* += 2;
    const action_bytecode = if (len != 0) self.bytecode[self.ip.* .. self.ip.* + len] else "";
    self.ip.* += len;

    const has_from = self.bytecode[self.ip.*] == 1;
    self.ip.* += 1;
    const has_to = self.bytecode[self.ip.*] == 1;
    self.ip.* += 1;

    const from = if (has_from) self.value_stack.pop() else value.val_nil;
    const to = if (has_to) self.value_stack.pop() else value.val_nil;

    var result = value.val_nil;
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

        if (from != value.val_nil) {
            // We have from
            start_range = isTruthy(from);
        } else {
            // No from; start only at row == 1.
            const rnum = value.asUint(self.scope_stack.rnum).?;
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
                action_bytecode,
                self.scope_stack,
                self.ctx,
                self.output,
            );
            try vm.run();

            result = vm.last_popped;
        } else {
            // Default action
            var writer = self.output.writer();
            try value.print(self.scope_stack.record, writer);
        }
    }

    try self.value_stack.append(result);
}

fn execMatch(self: *Vm, opcode: Compiler.Opcode) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const right = self.value_stack.pop();
    if (!value.isAnyStr(right)) return self.ctx.err(
        "Match op right hand side must be a regex pattern string; got {s}",
        .{value.typeOf(right)},
        error.InvalidMatch,
        offset,
    );
    const pattern = if (value.unboxStr(right)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(right).?.string;
    const pattern_copy = try self.scope_stack.allocator.dupe(u8, pattern);

    // Check if code isn't cached.
    const code_gop = try self.scope_stack.regex_cache.getOrPut(pattern_copy);
    if (!code_gop.found_existing) {
        // Compile it
        code_gop.value_ptr.* = p2z.compile(pattern_copy, .{}) catch |err| return self.ctx.err(
            "Could not compile regex pattern: {s}",
            .{pattern},
            err,
            offset,
        );
        _ = code_gop.value_ptr.jitCompile(0);
    }

    const left = self.value_stack.pop();
    if (!value.isAnyStr(left)) return self.ctx.err(
        "Match op left hand side must be a string; got {s}",
        .{value.typeOf(left)},
        error.InvalidMatch,
        offset,
    );
    const subject = if (value.unboxStr(left)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(left).?.string;

    const data = try p2z.MatchData.init(code_gop.value_ptr.*);
    defer data.deinit();

    var matches = try p2z.match(
        code_gop.value_ptr.*,
        subject,
        0,
        data,
        .{},
    );

    if (opcode == .nomatch) matches = !matches;
    try self.value_stack.append(value.boolToValue(matches));
}
fn execMatcher(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const right = self.value_stack.pop();
    if (!value.isAnyStr(right)) return self.ctx.err(
        "Matcher op right hand side must be a regex pattern string; got {s}",
        .{value.typeOf(right)},
        error.InvalidMatcher,
        offset,
    );
    const pattern = if (value.unboxStr(right)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(right).?.string;

    const left = self.value_stack.pop();
    if (!value.isAnyStr(left)) return self.ctx.err(
        "Matcher op left hand side must be a string; got {s}",
        .{value.typeOf(left)},
        error.InvalidMatcher,
        offset,
    );
    const subject = if (value.unboxStr(left)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(left).?.string;

    // Check for cached result.
    const subpat = try self.allocator.alloc(u8, pattern.len + subject.len);
    std.mem.copy(u8, subpat, subject);
    std.mem.copy(u8, subpat[subject.len..], pattern);
    if (self.scope_stack.value_cache.get(subpat)) |v| {
        try self.value_stack.append(v);
        return;
    }

    // Check if code isn't cached.
    const code_gop = try self.scope_stack.regex_cache.getOrPut(pattern);
    if (!code_gop.found_existing) {
        // Compile and cache it.
        code_gop.value_ptr.* = p2z.compile(pattern, .{}) catch |err| return self.ctx.err(
            "Could not compile regex pattern: {s}",
            .{pattern},
            err,
            offset,
        );
        _ = code_gop.value_ptr.jitCompile(0);
    }

    const data = try p2z.MatchData.init(code_gop.value_ptr.*);
    errdefer data.deinit();

    var matches = try p2z.match(
        code_gop.value_ptr.*,
        subject,
        0,
        data,
        .{},
    );

    if (matches) {
        const obj_ptr = try self.scope_stack.allocator.create(value.Object);
        obj_ptr.* = .{ .matcher = p2z.MatchIterator.init(
            code_gop.value_ptr.*,
            data,
            try self.scope_stack.allocator.dupe(u8, subject),
        ) };
        const v = value.addrToValue(@ptrToInt(obj_ptr));

        // Cache the result.
        const subpat_copy = try self.scope_stack.allocator.dupe(u8, subpat);
        try self.scope_stack.value_cache.put(subpat_copy, v);
        // Put it on the stack too.
        try self.value_stack.append(v);
    } else {
        // This is false instead of nil so that record ranges without a from clause can still work.
        try self.value_stack.append(value.val_false);
    }
}
fn execCapture(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 3;

    const m = self.value_stack.pop();
    const matcher_obj_ptr = value.asMatcher(m) orelse return self.ctx.err(
        "`capture` only works on regex matchers; got {s}",
        .{value.typeOf(m)},
        error.InvalidCapture,
        offset,
    );

    const arg = self.value_stack.pop();
    if (value.isAnyStr(arg)) {
        // Named capture
        const name = if (value.unboxStr(arg)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(arg).?.string;
        if (p2z.namedCapture(
            matcher_obj_ptr.matcher.code,
            matcher_obj_ptr.matcher.data,
            matcher_obj_ptr.matcher.subject,
            name,
        )) |substring| {
            if (substring.len < 7) {
                try self.value_stack.append(value.strToValue(substring));
            } else {
                const obj_ptr = try self.allocator.create(value.Object);
                obj_ptr.* = .{ .string = substring };
                const obj_addr = @ptrToInt(obj_ptr);
                try self.value_stack.append(value.addrToValue(obj_addr));
            }
        } else {
            // No capture with that nae found.
            try self.value_stack.append(value.val_nil);
        }
    } else if (value.asUint(arg)) |u| {
        // Numbered captue
        if (p2z.numberedCapture(
            matcher_obj_ptr.matcher.data,
            matcher_obj_ptr.matcher.subject,
            u,
        )) |substring| {
            if (substring.len < 7) {
                try self.value_stack.append(value.strToValue(substring));
            } else {
                const obj_ptr = try self.allocator.create(value.Object);
                obj_ptr.* = .{ .string = substring };
                const obj_addr = @ptrToInt(obj_ptr);
                try self.value_stack.append(value.addrToValue(obj_addr));
            }
        } else {
            // No capture with that number found.
            try self.value_stack.append(value.val_nil);
        }
    } else return self.ctx.err(
        "`capture` arg must be a string or unsigned integer; got {s}",
        .{value.typeOf(arg)},
        error.InvalidCapture,
        offset,
    );
}
fn execNext(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 3;

    const m = self.value_stack.pop();
    const matcher_obj_ptr = value.asMatcher(m) orelse return self.ctx.err(
        "`next` only works on regex matchers; got {s}",
        .{value.typeOf(m)},
        error.InvalidNext,
        offset,
    );

    try self.value_stack.append(value.boolToValue(try matcher_obj_ptr.matcher.next()));
}
fn execReset(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 3;

    const m = self.value_stack.pop();
    const matcher_obj_ptr = value.asMatcher(m) orelse return self.ctx.err(
        "`reset` only works on regex matchers; got {s}",
        .{value.typeOf(m)},
        error.InvalidNext,
        offset,
    );

    matcher_obj_ptr.matcher.reset();
    try self.value_stack.append(m);
}

// Stack Frame

const Frame = struct {
    bytecode: []const u8,
    ip: u16 = 0,
};

fn pushFrame(self: *Vm, bytecode: []const u8) !void {
    try self.frame_stack.append(.{ .bytecode = bytecode });
    self.bytecode = bytecode;
    self.ip = &self.frame_stack.items[self.frame_stack.items.len - 1].ip;
}

fn popFrame(self: *Vm) void {
    _ = self.frame_stack.pop();
    self.bytecode = self.frame_stack.items[self.frame_stack.items.len - 1].bytecode;
    self.ip = &self.frame_stack.items[self.frame_stack.items.len - 1].ip;
}

// Scopes
fn execScopeIn(self: *Vm) anyerror!void {
    const child_scope_type = @intToEnum(Scope.Type, self.bytecode[self.ip.* + 1]);
    try self.pushScope(Scope.init(self.allocator, child_scope_type));
    self.ip.* += 2;
}

fn execScopeOut(self: *Vm) anyerror!void {
    const scope_type = @intToEnum(Scope.Type, self.bytecode[self.ip.* + 1]);
    if (scope_type == .loop) {
        while (true) {
            var popped_scope = self.popScope();
            popped_scope.map.deinit();
            if (popped_scope.ty == .loop) break;
        }
    } else {
        var popped_scope = self.popScope();
        popped_scope.map.deinit();
    }
    self.ip.* += 2;
}

// Builtins

fn execAtan2(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    // Get args count.
    const num_args = self.bytecode[self.ip.*];
    self.ip.* += 1;

    if (num_args != 2) return self.ctx.err(
        "atan2 requires 2 arguments.",
        .{},
        error.InvalidAtan2,
        offset,
    );

    const y_val = self.value_stack.pop();
    const y = value.toFloat(y_val) orelse return self.ctx.err(
        "atan2 y not convertible to float.",
        .{},
        error.InvalidAtan2,
        offset,
    );

    const x_val = self.value_stack.pop();
    const x = value.toFloat(x_val) orelse return self.ctx.err(
        "atan2 x not convertible to float.",
        .{},
        error.InvalidAtan2,
        offset,
    );

    const result = value.floatToValue(std.math.atan2(f64, y, x));
    try self.value_stack.append(result);
}
fn execChars(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const str = self.value_stack.pop();
    if (!value.isAnyStr(str)) return self.ctx.err(
        "`chars` method on non-string.",
        .{},
        error.InvalidCharsCall,
        offset,
    );

    const str_str = if (value.unboxStr(str)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(str).?.string;
    var list = std.ArrayList(Value).init(self.allocator);

    var giter = GraphemeIterator.init(str_str) catch |err| return self.ctx.err(
        "Unicode error.",
        .{},
        err,
        offset,
    );
    while (giter.next()) |grapheme| {
        if (grapheme.bytes.len < 7) {
            try list.append(value.strToValue(grapheme.bytes));
        } else {
            const obj_ptr = try self.allocator.create(value.Object);
            obj_ptr.* = .{ .string = grapheme.bytes };
            const obj_addr = @ptrToInt(obj_ptr);
            try list.append(value.addrToValue(obj_addr));
        }
    }

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .list = list };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1;
}
fn execPrint(self: *Vm) anyerror!void {
    self.ip.* += 3; // Func calls get offset.
    // Get args count.
    const num_args = self.bytecode[self.ip.*];
    self.ip.* += 1;

    var writer = self.output.writer();
    var i: usize = 0;
    while (i < num_args) : (i += 1) {
        if (i != 0) try value.print(self.scope_stack.ocs, writer);
        try value.print(self.value_stack.pop(), writer);
    }

    try self.value_stack.append(value.val_nil);
}
fn execSprint(self: *Vm) anyerror!void {
    self.ip.* += 3;
    // Get args count.
    const num_args = self.bytecode[self.ip.*];
    self.ip.* += 1;

    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();

    var i: usize = 0;
    while (i < num_args) : (i += 1) {
        if (i != 0) try value.print(self.scope_stack.ocs, writer);
        try value.print(self.value_stack.pop(), writer);
    }

    if (buf.items.len < 7) {
        try self.value_stack.append(value.strToValue(buf.items));
    } else {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .string = buf.items };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
    }
}
fn execOneArgMath(self: *Vm, builtin: Token.Tag) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    // Get args count.
    const num_args = self.bytecode[self.ip.*];
    self.ip.* += 1;

    if (num_args != 1) return self.ctx.err(
        "Math builtin call requires one argument.",
        .{},
        error.InvalidMathBuiltin,
        offset,
    );

    const x_val = self.value_stack.pop();
    const x = value.toFloat(x_val) orelse return self.ctx.err(
        "Arg not convertible to float.",
        .{},
        error.InvalidArg,
        offset,
    );

    const result = switch (builtin) {
        .pd_cos => value.floatToValue(@cos(x)),
        .pd_exp => value.floatToValue(std.math.exp(x)),
        .pd_int => value.intToValue(@floatToInt(i32, @trunc(x))),
        .pd_log => value.floatToValue(@log(x)),
        .pd_rand => value.uintToValue(std.rand.DefaultPrng.init(@intCast(usize, std.time.timestamp())).random().uintAtMost(u32, @floatToInt(u32, x))),
        .pd_sin => value.floatToValue(@sin(x)),
        .pd_sqrt => value.floatToValue(@sqrt(x)),
        else => unreachable,
    };

    try self.value_stack.append(result);
}
fn execContains(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const haystack = self.value_stack.pop();
    if (!value.isAnyStr(haystack) and value.asList(haystack) == null) return self.ctx.err(
        "`contains` only works on strings and lists.",
        .{},
        error.InvalidContains,
        offset,
    );

    const needle = self.value_stack.pop();
    var result = false;

    if (value.asList(haystack)) |list_obj_ptr| {
        for (list_obj_ptr.list.items) |item| {
            if (value.eql(item, needle)) result = true;
        }
    } else {
        if (!value.isAnyStr(needle)) return self.ctx.err(
            "`contains` arg on string must be a string.",
            .{},
            error.InvalidContains,
            offset,
        );
        const hay_str = if (value.unboxStr(haystack)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(haystack).?.string;
        const needle_str = if (value.unboxStr(needle)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(needle).?.string;
        result = std.mem.containsAtLeast(u8, hay_str, 1, needle_str);
    }

    try self.value_stack.append(value.boolToValue(result));
    self.ip.* += 1;
}
fn execIndexOf(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const haystack = self.value_stack.pop();
    if (!value.isAnyStr(haystack) and value.asList(haystack) == null) return self.ctx.err(
        "`indexOf` only works on strings and lists.",
        .{},
        error.InvalidIndexOf,
        offset,
    );

    const needle = self.value_stack.pop();
    var result: ?usize = null;

    if (value.asList(haystack)) |list_obj_ptr| {
        for (list_obj_ptr.list.items) |item, i| {
            if (value.eql(item, needle)) {
                result = i;
                break;
            }
        }
    } else {
        if (!value.isAnyStr(needle)) return self.ctx.err(
            "`indexOf` arg on string must be a string.",
            .{},
            error.InvalidContains,
            offset,
        );
        const hay_str = if (value.unboxStr(haystack)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(haystack).?.string;
        const needle_str = if (value.unboxStr(needle)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(needle).?.string;

        var giter = try GraphemeIterator.init(hay_str);
        var i: usize = 0;
        while (giter.next()) |grapheme| : (i += 1) {
            if (std.mem.eql(u8, grapheme.bytes, needle_str)) {
                result = i;
                break;
            }
        }
    }

    try self.value_stack.append(if (result) |idx| value.uintToValue(@intCast(u32, idx)) else value.val_nil);
    self.ip.* += 1;
}
fn execLastIndexOf(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const haystack = self.value_stack.pop();
    if (!value.isAnyStr(haystack) and value.asList(haystack) == null) return self.ctx.err(
        "`lastIndexOf` only works on strings and lists.",
        .{},
        error.InvalidLastIndexOf,
        offset,
    );

    const needle = self.value_stack.pop();
    var result: ?usize = null;

    if (value.asList(haystack)) |list_obj_ptr| {
        const len = list_obj_ptr.list.items.len;
        var i: usize = 1;
        while (i <= list_obj_ptr.list.items.len) : (i += 1) {
            if (value.eql(list_obj_ptr.list.items[len - i], (needle))) {
                result = i;
                break;
            }
        }
    } else {
        if (!value.isAnyStr(needle)) return self.ctx.err(
            "`lastIndexOf` arg on string must be a string.",
            .{},
            error.InvalidLastIndexOf,
            offset,
        );
        const hay_str = if (value.unboxStr(haystack)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(haystack).?.string;
        const needle_str = if (value.unboxStr(needle)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(needle).?.string;

        var giter = try GraphemeIterator.init(hay_str);
        var i: usize = 0;
        while (giter.next()) |grapheme| : (i += 1) {
            if (std.mem.eql(u8, grapheme.bytes, needle_str)) result = i;
        }
    }

    try self.value_stack.append(if (result) |idx| value.uintToValue(@intCast(u32, idx)) else value.val_nil);
    self.ip.* += 1;
}
fn execLen(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const v = self.value_stack.pop();
    if (value.asList(v) == null and value.asMap(v) == null and !value.isAnyStr(v)) return self.ctx.err(
        "`len` only works on strings, lists, and maps.",
        .{},
        error.InvalidLen,
        offset,
    );

    var len: usize = 0;
    if (value.asList(v)) |list_obj_ptr| {
        len = list_obj_ptr.list.items.len;
    } else if (value.asMap(v)) |map_obj_ptr| {
        len = map_obj_ptr.map.count();
    } else {
        const str = if (value.unboxStr(v)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(v).?.string;
        len = str.len;
    }

    try self.value_stack.append(value.uintToValue(@intCast(u32, len)));
    self.ip.* += 1;
}
fn execMapKeys(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const m = self.value_stack.pop();
    const map_obj_ptr = value.asMap(m) orelse return self.ctx.err(
        "`keys` only works on maps.",
        .{},
        error.InvalidKeys,
        offset,
    );

    const map_count = map_obj_ptr.map.count();

    if (map_count == 0) {
        var list = std.ArrayList(Value).init(self.allocator);
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .list = list };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
        self.ip.* += 1;
        return;
    }

    var list = try std.ArrayList(Value).initCapacity(self.allocator, map_count);

    var key_iter = map_obj_ptr.map.keyIterator();
    while (key_iter.next()) |key| {
        if (key.len < 7) {
            try list.append(value.strToValue(key.*));
        } else {
            const obj_ptr = try self.allocator.create(value.Object);
            obj_ptr.* = .{ .string = key.* };
            const obj_addr = @ptrToInt(obj_ptr);
            try list.append(value.addrToValue(obj_addr));
        }
    }

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .list = list };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1;
}

fn entryAsc(_: void, a: std.StringHashMap(Value).Entry, b: std.StringHashMap(Value).Entry) bool {
    return value.cmp(a.value_ptr.*, b.value_ptr.*) catch unreachable == .lt;
}
fn entryDesc(_: void, a: std.StringHashMap(Value).Entry, b: std.StringHashMap(Value).Entry) bool {
    return value.cmp(a.value_ptr.*, b.value_ptr.*) catch unreachable == .gt;
}

fn execMapKeysByValue(self: *Vm, asc: bool) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const m = self.value_stack.pop();
    const map_obj_ptr = value.asMap(m) orelse return self.ctx.err(
        "`keysByValueAsc` only works on maps.",
        .{},
        error.InvalidKeysByValueAsc,
        offset,
    );

    const map_count = map_obj_ptr.map.count();

    if (map_count == 0) {
        var list = std.ArrayList(Value).init(self.allocator);
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .list = list };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
        self.ip.* += 1;
        return;
    }

    var list = try std.ArrayList(Value).initCapacity(self.allocator, map_count);

    var entries = try std.ArrayList(std.StringHashMap(Value).Entry).initCapacity(self.allocator, map_count);
    defer entries.deinit();

    var iter = map_obj_ptr.map.iterator();
    while (iter.next()) |entry| entries.appendAssumeCapacity(entry);

    if (asc) {
        std.sort.sort(std.StringHashMap(Value).Entry, entries.items, {}, entryAsc);
    } else {
        std.sort.sort(std.StringHashMap(Value).Entry, entries.items, {}, entryDesc);
    }

    for (entries.items) |entry| {
        if (entry.key_ptr.len < 7) {
            list.appendAssumeCapacity(value.strToValue(entry.key_ptr.*));
        } else {
            const obj_ptr = try self.allocator.create(value.Object);
            obj_ptr.* = .{ .string = entry.key_ptr.* };
            const obj_addr = @ptrToInt(obj_ptr);
            list.appendAssumeCapacity(value.addrToValue(obj_addr));
        }
    }

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .list = list };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1; // num_args
}
fn execMapValues(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const m = self.value_stack.pop();
    const map_obj_ptr = value.asMap(m) orelse return self.ctx.err(
        "`values` only works on maps.",
        .{},
        error.InvalidValues,
        offset,
    );

    const map_count = map_obj_ptr.map.count();

    if (map_count == 0) {
        var list = std.ArrayList(Value).init(self.allocator);
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .list = list };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
        self.ip.* += 1;
        return;
    }

    var list = try std.ArrayList(Value).initCapacity(self.allocator, map_count);

    var value_iter = map_obj_ptr.map.valueIterator();
    while (value_iter.next()) |ev| list.appendAssumeCapacity(ev.*);

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .list = list };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1;
}
fn listMeanHelper(list: std.ArrayList(Value)) f64 {
    var sum: f64 = 0;
    var count: f64 = 0;
    for (list.items) |item| {
        if (value.toFloat(item)) |f| {
            sum += f;
            count += 1;
        }
    }

    return sum / count;
}
fn execListMean(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "`mean` only works on lists.",
        .{},
        error.InvalidMean,
        offset,
    );

    if (list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(value.floatToValue(0));
        self.ip.* += 1;
        return;
    }

    try self.value_stack.append(value.floatToValue(listMeanHelper(list_obj_ptr.list)));
    self.ip.* += 1;
}
fn execListMedian(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "`median` only works on lists.",
        .{},
        error.InvalidMedian,
        offset,
    );

    if (list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(value.floatToValue(0));
        self.ip.* += 1;
        return;
    }

    var list_copy = try std.ArrayList(f64).initCapacity(self.allocator, list_obj_ptr.list.items.len);

    for (list_obj_ptr.list.items) |item| {
        if (value.toFloat(item)) |f| list_copy.appendAssumeCapacity(f);
    }
    std.sort.sort(f64, list_copy.items, {}, comptime std.sort.asc(f64));

    var median: f64 = @intToFloat(f64, list_copy.items.len) + 1 / 2 - 1;
    if (list_copy.items.len % 2 == 0) {
        const mid = list_copy.items.len / 2 - 1;
        median = (list_copy.items[mid] + list_copy.items[mid + 1]) / 2;
    }

    try self.value_stack.append(value.floatToValue(median));
    self.ip.* += 1;
}
fn execListMode(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "`mode` only workds on lists.",
        .{},
        error.InvalidMode,
        offset,
    );

    if (list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(value.val_nil);
        self.ip.* += 1;
        return;
    }

    //TODO: Use arena to save memory.
    var counts = std.StringHashMap(usize).init(self.allocator);
    var key_buf: [4096]u8 = undefined;

    for (list_obj_ptr.list.items) |item| {
        if (value.toFloat(item)) |f| {
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
        if (entry.value_ptr.* == highest) try list.append(value.floatToValue(std.fmt.parseFloat(f64, entry.key_ptr.*) catch unreachable));
    }
    std.sort.sort(Value, list.items, {}, value.asc);

    const result = if (list.items.len == counts.count()) value.val_nil else blk: {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .list = list };
        const obj_addr = @ptrToInt(obj_ptr);
        break :blk value.addrToValue(obj_addr);
    };
    try self.value_stack.append(result);
    self.ip.* += 1;
}
fn execListStdev(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "`stdev` only works on lists.",
        .{},
        error.InvalidStdev,
        offset,
    );

    if (list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(value.floatToValue(0));
        self.ip.* += 1;
        return;
    }

    const mean = listMeanHelper(list_obj_ptr.list);

    var sum_of_squares: f64 = 0;
    var count: f64 = 0;
    for (list_obj_ptr.list.items) |item| {
        if (value.toFloat(item)) |f| {
            const diff = f - mean;
            const square = diff * diff;
            sum_of_squares += square;
            count += 1;
        }
    }

    const sos_by_count = sum_of_squares / count;

    try self.value_stack.append(value.floatToValue(@sqrt(sos_by_count)));
    self.ip.* += 1;
}
fn execListMin(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "min only works on lists.",
        .{},
        error.InvalidMin,
        offset,
    );

    if (list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(value.val_nil);
        self.ip.* += 1;
        return;
    }

    var min = list_obj_ptr.list.items[0];
    for (list_obj_ptr.list.items) |item| {
        const comparison = try value.cmp(min, item);
        if (comparison == .gt) min = item;
    }

    try self.value_stack.append(min);
    self.ip.* += 1;
}
fn execListMax(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "`max` only works on lists.",
        .{},
        error.InvalidMax,
        offset,
    );

    if (list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(value.val_nil);
        self.ip.* += 1;
        return;
    }

    var max = list_obj_ptr.list.items[0];
    for (list_obj_ptr.list.items) |item| {
        const comparison = try value.cmp(max, item);
        if (comparison == .lt) max = item;
    }

    try self.value_stack.append(max);
    self.ip.* += 1;
}
fn execListSort(self: *Vm, asc: bool) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "`listSortAsc` only works on lists.",
        .{},
        error.InvalidSortAsc,
        offset,
    );

    if (list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(l);
        self.ip.* += 1;
        return;
    }

    if (asc) {
        std.sort.sort(Value, list_obj_ptr.list.items, {}, value.asc);
    } else {
        std.sort.sort(Value, list_obj_ptr.list.items, {}, value.desc);
    }

    try self.value_stack.append(l);
    self.ip.* += 1;
}
fn execListReverse(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "`reverse` only works on lists.",
        .{},
        error.InvalidReverse,
        offset,
    );

    if (list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(l);
        self.ip.* += 1;
        return;
    }

    std.mem.reverse(Value, list_obj_ptr.list.items);
    try self.value_stack.append(l);
    self.ip.* += 1;
}
fn execStrSplit(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const str = self.value_stack.pop();
    if (!value.isAnyStr(str)) return self.ctx.err(
        "`split` only works on strings.",
        .{},
        error.InvalidSplit,
        offset,
    );

    const delim = self.value_stack.pop();
    if (!value.isAnyStr(delim)) return self.ctx.err(
        "`split` delimiter must be a string",
        .{},
        error.InvalidSplit,
        offset,
    );

    const str_str = if (value.unboxStr(str)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(str).?.string;
    const delim_str = if (value.unboxStr(delim)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(delim).?.string;

    var list = std.ArrayList(Value).init(self.allocator);
    var iter = std.mem.split(u8, str_str, delim_str);
    while (iter.next()) |sub| {
        if (sub.len < 7) {
            try list.append(value.strToValue(sub));
        } else {
            const obj_ptr = try self.allocator.create(value.Object);
            obj_ptr.* = .{ .string = sub };
            const obj_addr = @ptrToInt(obj_ptr);
            try list.append(value.addrToValue(obj_addr));
        }
    }

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .list = list };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1;
}
fn execListJoin(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "`join` only works on lists.",
        .{},
        error.InvalidJoin,
        offset,
    );

    const delim = self.value_stack.pop();
    if (!value.isAnyStr(delim)) return self.ctx.err(
        "`join` delimiter must be a string",
        .{},
        error.InvalidJoin,
        offset,
    );

    const str_delim = if (value.unboxStr(delim)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(delim).?.string;
    var buf = std.ArrayList(u8).init(self.allocator);
    var writer = buf.writer();

    for (list_obj_ptr.list.items) |item, i| {
        if (i != 0 and str_delim.len > 0) try buf.appendSlice(str_delim);
        try value.print(item, writer);
    }

    if (buf.items.len < 7) {
        try self.value_stack.append(value.strToValue(buf.items));
    } else {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .string = buf.items };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
    }
    self.ip.* += 1;
}
fn execStrEndStart(self: *Vm, start: bool) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const str = self.value_stack.pop();
    if (!value.isAnyStr(str)) return self.ctx.err(
        "This method only works on strings.",
        .{},
        error.InvalidEndsWith,
        offset,
    );

    const needle = self.value_stack.pop();
    if (!value.isAnyStr(needle)) return self.ctx.err(
        "Arg must be a string.",
        .{},
        error.InvalidXWith,
        offset,
    );

    const str_str = if (value.unboxStr(str)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(str).?.string;
    const str_needle = if (value.unboxStr(needle)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(needle).?.string;

    var result: Value = undefined;
    if (start) {
        result = value.boolToValue(std.mem.startsWith(u8, str_str, str_needle));
    } else {
        result = value.boolToValue(std.mem.endsWith(u8, str_str, str_needle));
    }

    try self.value_stack.append(result);
    self.ip.* += 1;
}
fn execMapMethod(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const container = self.value_stack.pop();
    if (value.asList(container)) |list_obj_ptr| {
        return self.execMapList(list_obj_ptr, offset);
    } else if (value.asRange(container)) |range_obj_ptr| {
        return self.execMapRange(range_obj_ptr, offset);
    } else return self.ctx.err(
        "`map` only works on lists and ranges.",
        .{},
        error.InvalidEach,
        offset,
    );
}
fn execMapList(self: *Vm, list_obj_ptr: *value.Object, offset: u16) anyerror!void {
    const f = self.value_stack.pop();
    const func_obj_ptr = value.asFunc(f) orelse return self.ctx.err(
        "`map` requres function argument.",
        .{},
        error.InvalidMap,
        offset,
    );

    // No-ops
    if (func_obj_ptr.func.bytecode == null or list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(value.addrToValue(@ptrToInt(list_obj_ptr)));
        self.ip.* += 1;
        return;
    }

    var list = try std.ArrayList(Value).initCapacity(self.allocator, list_obj_ptr.list.items.len);

    for (list_obj_ptr.list.items) |item, i| {
        const v = try self.execListPredicate(func_obj_ptr, item, i);
        list.appendAssumeCapacity(v);
    }

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .list = list };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1;
}
fn execMapRange(self: *Vm, range_obj_ptr: *const value.Object, offset: u16) anyerror!void {
    const f = self.value_stack.pop();
    const func_obj_ptr = value.asFunc(f) orelse return self.ctx.err(
        "`map` requres function argument.",
        .{},
        error.InvalidMap,
        offset,
    );

    // No-ops
    if (func_obj_ptr.func.bytecode == null or range_obj_ptr.range[1] - range_obj_ptr.range[0] == 0) {
        try self.value_stack.append(value.addrToValue(@ptrToInt(range_obj_ptr)));
        self.ip.* += 1;
        return;
    }

    var list = try std.ArrayList(Value).initCapacity(self.allocator, range_obj_ptr.range[1] - range_obj_ptr.range[0]);

    var n = range_obj_ptr.range[0];
    var i: u32 = 0;
    while (n < range_obj_ptr.range[1]) : ({
        n += 1;
        i += 1;
    }) {
        const v = try self.execRangePredicate(func_obj_ptr, n, i);
        list.appendAssumeCapacity(v);
    }

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .list = list };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1;
}
fn execFilter(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const container = self.value_stack.pop();
    if (value.asList(container)) |list_obj_ptr| {
        return self.execFilterList(list_obj_ptr, offset);
    } else if (value.asRange(container)) |range_obj_ptr| {
        return self.execFilterRange(range_obj_ptr, offset);
    } else return self.ctx.err(
        "`filter` only works on lists and ranges.",
        .{},
        error.InvalidEach,
        offset,
    );
}
fn execFilterList(self: *Vm, list_obj_ptr: *const value.Object, offset: u16) !void {
    const f = self.value_stack.pop();
    const func_obj_ptr = value.asFunc(f) orelse return self.ctx.err(
        "`filter` arg must be a function.",
        .{},
        error.InvalidFilter,
        offset,
    );

    // No-ops
    if (func_obj_ptr.func.bytecode == null or list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(value.addrToValue(@ptrToInt(list_obj_ptr)));
        self.ip.* += 1;
        return;
    }

    var list = std.ArrayList(Value).init(self.allocator);

    for (list_obj_ptr.list.items) |item, i| {
        const v = try self.execListPredicate(func_obj_ptr, item, i);
        if (isTruthy(v)) try list.append(item);
    }

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .list = list };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1;
}
fn execFilterRange(self: *Vm, range_obj_ptr: *const value.Object, offset: u16) !void {
    const f = self.value_stack.pop();
    const func_obj_ptr = value.asFunc(f) orelse return self.ctx.err(
        "`filter` arg must be a function.",
        .{},
        error.InvalidFilter,
        offset,
    );

    var list = std.ArrayList(Value).init(self.allocator);

    // No-ops
    if (func_obj_ptr.func.bytecode == null or range_obj_ptr.range[1] - range_obj_ptr.range[0] == 0) {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .list = list };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
        self.ip.* += 1;
        return;
    }

    var n = range_obj_ptr.range[0];
    var i: u32 = 0;
    while (n < range_obj_ptr.range[1]) : ({
        n += 1;
        i += 1;
    }) {
        const v = try self.execRangePredicate(func_obj_ptr, n, i);
        if (isTruthy(v)) try list.append(value.uintToValue(n));
    }

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .list = list };
    const obj_addr = @ptrToInt(obj_ptr);
    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1;
}
fn execEach(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const container = self.value_stack.pop();
    if (value.asList(container)) |list_obj_ptr| {
        return self.execEachList(list_obj_ptr, offset);
    } else if (value.asMap(container)) |map_obj_ptr| {
        return self.execEachMap(map_obj_ptr, offset);
    } else if (value.asRange(container)) |range_obj_ptr| {
        return self.execEachRange(range_obj_ptr, offset);
    } else return self.ctx.err(
        "`each` only works on lists, maps, and ranges.",
        .{},
        error.InvalidEach,
        offset,
    );
}
fn execEachList(self: *Vm, list_obj_ptr: *value.Object, offset: u16) !void {
    const f = self.value_stack.pop();
    const func_obj_ptr = value.asFunc(f) orelse return self.ctx.err(
        "`each` arg must be a function.",
        .{},
        error.InvalidEach,
        offset,
    );

    // No-ops
    const obj_addr = @ptrToInt(list_obj_ptr);
    if (func_obj_ptr.func.bytecode == null or list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(value.addrToValue(obj_addr));
        self.ip.* += 1;
        return;
    }

    for (list_obj_ptr.list.items) |item, i| _ = try self.execListPredicate(func_obj_ptr, item, i);

    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1;
}
fn execEachMap(self: *Vm, map_obj_ptr: *value.Object, offset: u16) !void {
    const f = self.value_stack.pop();
    const func_obj_ptr = value.asFunc(f) orelse return self.ctx.err(
        "`each` arg must be a function.",
        .{},
        error.InvalidEach,
        offset,
    );

    // No-ops
    const obj_addr = @ptrToInt(map_obj_ptr);
    if (func_obj_ptr.func.bytecode == null or map_obj_ptr.map.count() == 0) {
        try self.value_stack.append(value.addrToValue(obj_addr));
        self.ip.* += 1;
        return;
    }

    var iter = map_obj_ptr.map.iterator();
    var i: usize = 0;
    while (iter.next()) |entry| : (i += 1) _ = try self.execMapPredicate(func_obj_ptr, entry.key_ptr.*, entry.value_ptr.*, i);

    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1;
}
fn execEachRange(self: *Vm, range_obj_ptr: *const value.Object, offset: u16) !void {
    const f = self.value_stack.pop();
    const func_obj_ptr = value.asFunc(f) orelse return self.ctx.err(
        "`each` arg must be a function.",
        .{},
        error.InvalidEach,
        offset,
    );

    // No-ops
    const obj_addr = @ptrToInt(range_obj_ptr);
    if (func_obj_ptr.func.bytecode == null or range_obj_ptr.range[1] - range_obj_ptr.range[0] == 0) {
        try self.value_stack.append(value.addrToValue(obj_addr));
        self.ip.* += 1;
        return;
    }

    var n = range_obj_ptr.range[0];
    var i: u32 = 0;
    while (n < range_obj_ptr.range[1]) : ({
        n += 1;
        i += 1;
    }) _ = try self.execRangePredicate(func_obj_ptr, n, i);

    try self.value_stack.append(value.addrToValue(obj_addr));
    self.ip.* += 1;
}
fn execReduce(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const container = self.value_stack.pop();
    if (value.asList(container)) |list_obj_ptr| {
        return self.execReduceList(list_obj_ptr, offset);
    } else if (value.asRange(container)) |range_obj_ptr| {
        return self.execReduceRange(range_obj_ptr, offset);
    } else return self.ctx.err(
        "`reduce` only works on lists and ranges.",
        .{},
        error.InvalidEach,
        offset,
    );
}
fn execReduceList(self: *Vm, list_obj_ptr: *value.Object, offset: u16) anyerror!void {
    var acc = self.value_stack.pop();
    const f = self.value_stack.pop();
    const func_obj_ptr = value.asFunc(f) orelse return self.ctx.err(
        "`reduce` last arg must be a function.",
        .{},
        error.InvalidReduce,
        offset,
    );

    // No-ops
    if (func_obj_ptr.func.bytecode == null or list_obj_ptr.list.items.len == 0) {
        try self.value_stack.append(value.val_nil);
        self.ip.* += 1;
        return;
    }

    for (list_obj_ptr.list.items) |item, i| {
        // Set up function scope.
        var func_scope = Scope.init(self.allocator, .function);
        defer func_scope.map.deinit();

        // Assign args as locals in function scope.
        try func_scope.map.put("acc", acc);
        try func_scope.map.put("it", item);
        try func_scope.map.put("@0", item);
        try func_scope.map.put("index", value.uintToValue(@intCast(u32, i)));
        if (func_obj_ptr.func.params) |params| {
            if (params.len > 0) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[0]..], 0), acc);
            if (params.len > 1) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[1]..], 0), item);
        }

        try self.pushScope(func_scope);
        acc = try self.execPredicate(func_obj_ptr.func.bytecode.?);
        _ = self.popScope();
    }

    try self.value_stack.append(acc);
    self.ip.* += 1;
}
fn execReduceRange(self: *Vm, range_obj_ptr: *const value.Object, offset: u16) anyerror!void {
    var acc = self.value_stack.pop();
    const f = self.value_stack.pop();
    const func_obj_ptr = value.asFunc(f) orelse return self.ctx.err(
        "`reduce` last arg must be a function.",
        .{},
        error.InvalidReduce,
        offset,
    );

    // No-ops
    if (func_obj_ptr.func.bytecode == null or range_obj_ptr.range[1] - range_obj_ptr.range[0] == 0) {
        try self.value_stack.append(value.val_nil);
        self.ip.* += 1;
        return;
    }

    var n = range_obj_ptr.range[0];
    var i: u32 = 0;
    while (n < range_obj_ptr.range[1]) : ({
        n += 1;
        i += 1;
    }) {
        // Set up function scope.
        var func_scope = Scope.init(self.allocator, .function);
        defer func_scope.map.deinit();

        const n_val = value.uintToValue(n);
        const i_val = value.uintToValue(i);

        // Assign args as locals in function scope.
        try func_scope.map.put("acc", acc);
        try func_scope.map.put("it", n_val);
        try func_scope.map.put("@0", n_val);
        try func_scope.map.put("index", i_val);
        if (func_obj_ptr.func.params) |params| {
            if (params.len > 0) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[0]..], 0), acc);
            if (params.len > 1) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[1]..], 0), n_val);
            if (params.len > 2) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[2]..], 0), i_val);
        }

        try self.pushScope(func_scope);
        acc = try self.execPredicate(func_obj_ptr.func.bytecode.?);
        _ = self.popScope();
    }

    try self.value_stack.append(acc);
    self.ip.* += 1;
}
fn execRand(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    // Get args count.
    const num_args = self.bytecode[self.ip.*];
    self.ip.* += 1;
    if (num_args != 1) return self.ctx.err(
        "`rand` requires a single argument.",
        .{},
        error.InvalidRand,
        offset,
    );

    const x_val = self.value_stack.pop();
    const x = value.asUint(x_val) orelse return self.ctx.err(
        "rand argument must be unsigned integer.",
        .{},
        error.InvalidRand,
        offset,
    );

    const result = value.uintToValue(std.rand.DefaultPrng.init(@intCast(usize, std.time.timestamp())).random().uintAtMost(u32, x));
    try self.value_stack.append(result);
}
fn execListPush(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "`push` only works on lists.",
        .{},
        error.InvalidPush,
        offset,
    );

    var item = self.value_stack.pop();
    try list_obj_ptr.list.append(try value.copy(item, list_obj_ptr.list.allocator));

    try self.value_stack.append(l);
    self.ip.* += 1;
}
fn execListPop(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "`pop` only works on lists.",
        .{},
        error.InvalidPop,
        offset,
    );

    try self.value_stack.append(list_obj_ptr.list.pop());
    self.ip.* += 1;
}

fn execListPredicate(self: *Vm, func_obj_ptr: *value.Object, item: Value, index: usize) anyerror!Value {
    // Assign args as locals in function scope.
    var func_scope = Scope.init(self.allocator, .function); //TODO: Can we use other allocator here?
    defer func_scope.map.deinit();

    const index_val = value.uintToValue(@intCast(u32, index));

    try func_scope.map.put("it", item);
    try func_scope.map.put("@0", item);
    try func_scope.map.put("index", index_val);

    if (func_obj_ptr.func.params) |params| {
        if (params.len > 0) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[0]..], 0), item);
        if (params.len > 1) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[1]..], 0), index_val);
    }

    try self.pushScope(func_scope);
    const result = self.execPredicate(func_obj_ptr.func.bytecode.?);
    _ = self.popScope();

    return result;
}

fn execMapPredicate(self: *Vm, func_obj_ptr: *value.Object, key: []const u8, item: Value, index: usize) anyerror!Value {
    // Assign args as locals in function scope.
    var func_scope = Scope.init(self.allocator, .function);
    defer func_scope.map.deinit();

    var key_val: Value = undefined;
    if (key.len < 7) {
        key_val = value.strToValue(key);
    } else {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .string = key };
        const obj_addr = @ptrToInt(obj_ptr);
        key_val = value.addrToValue(obj_addr);
    }
    const index_val = value.uintToValue(@intCast(u32, index));

    try func_scope.map.put("key", key_val);
    try func_scope.map.put("value", item);
    try func_scope.map.put("index", index_val);

    if (func_obj_ptr.func.params) |params| {
        if (params.len > 0) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[0]..], 0), key_val);
        if (params.len > 1) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[1]..], 0), item);
        if (params.len > 2) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[2]..], 0), index_val);
    }

    try self.pushScope(func_scope);
    const result = self.execPredicate(func_obj_ptr.func.bytecode.?);
    _ = self.popScope();

    return result;
}

fn execRangePredicate(self: *Vm, func_obj_ptr: *value.Object, n: u32, i: u32) anyerror!Value {
    // Assign args as locals in function scope.
    var func_scope = Scope.init(self.allocator, .function); //TODO: Can we use other allocator here?
    defer func_scope.map.deinit();

    const n_val = value.uintToValue(n);
    const i_val = value.uintToValue(i);

    try func_scope.map.put("it", n_val);
    try func_scope.map.put("@0", n_val);
    try func_scope.map.put("index", i_val);

    if (func_obj_ptr.func.params) |params| {
        if (params.len > 0) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[0]..], 0), n_val);
        if (params.len > 1) try func_scope.map.put(std.mem.sliceTo(self.bytecode[params[1]..], 0), i_val);
    }

    try self.pushScope(func_scope);
    const result = self.execPredicate(func_obj_ptr.func.bytecode.?);
    _ = self.popScope();

    return result;
}

fn execPredicate(self: *Vm, bytecode: []const u8) anyerror!Value {
    // Set up Sub-VM arena.
    var vm_arena = std.heap.ArenaAllocator.init(self.allocator);
    defer vm_arena.deinit();
    const vm_allocator = vm_arena.allocator();

    var vm = try init(
        vm_allocator,
        bytecode,
        self.scope_stack,
        self.ctx,
        self.output,
    );
    try vm.run();

    return try value.copy(vm.last_popped, self.allocator);
}

fn execRedir(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const clobber = self.bytecode[self.ip.*] == 1;
    self.ip.* += 1;

    // Get filename
    const filename = self.value_stack.pop();
    if (!value.isAnyStr(filename)) return self.ctx.err(
        "Redirection filename must be a string",
        .{},
        error.InvalidRedirect,
        offset,
    );

    const str_filename = if (value.unboxStr(filename)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(filename).?.string;

    // Open file
    var create_flags: std.fs.File.CreateFlags = .{};
    if (!clobber) create_flags.truncate = false;
    var file = try std.fs.cwd().createFile(str_filename, create_flags);
    defer file.close();
    if (!clobber) try file.seekFromEnd(0);

    // Buffering
    var file_buf = std.io.bufferedWriter(file.writer());
    var writer = file_buf.writer();

    // Write
    const v = self.value_stack.pop();
    try value.print(v, writer);
    try file_buf.flush();

    try self.value_stack.append(v);
}

fn execStrCase(self: *Vm, lower: bool) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const s = self.value_stack.pop();
    if (!value.isAnyStr(s)) return self.ctx.err(
        "Case conversion only works on strings.",
        .{},
        error.InvalidToCase,
        offset,
    );

    const str_str = if (value.unboxStr(s)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(s).?.string;

    if (str_str.len == 0) {
        try self.value_stack.append(s);
        self.ip.* += 1;
        return;
    }

    var new_str: []const u8 = undefined;
    if (lower) {
        new_str = try ziglyph.toLowerStr(self.allocator, str_str);
    } else {
        new_str = try ziglyph.toUpperStr(self.allocator, str_str);
    }

    if (new_str.len < 7) {
        try self.value_stack.append(value.strToValue(new_str));
    } else {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .string = new_str };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
    }

    self.ip.* += 1; // num_args
}

fn execUnique(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const l = self.value_stack.pop();
    const list_obj_ptr = value.asList(l) orelse return self.ctx.err(
        "`unique` only works on lists.",
        .{},
        error.InvalidUnique,
        offset,
    );

    var new_list = std.ArrayList(Value).init(self.allocator);

    // No-op
    if (list_obj_ptr.list.items.len == 0) {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .list = new_list };
        try self.value_stack.append(value.addrToValue(@ptrToInt(obj_ptr)));
        self.ip.* += 1;
        return;
    }

    var item_set = std.AutoArrayHashMap(Value, void).init(self.allocator);
    defer item_set.deinit();
    try item_set.ensureTotalCapacity(@intCast(u32, list_obj_ptr.list.items.len));

    for (list_obj_ptr.list.items) |item| item_set.putAssumeCapacity(item, {});
    for (item_set.keys()) |item| try new_list.append(item);

    const obj_ptr = try self.allocator.create(value.Object);
    obj_ptr.* = .{ .list = new_list };
    try self.value_stack.append(value.addrToValue(@ptrToInt(obj_ptr)));

    self.ip.* += 1; // num_args
}

fn execReplace(self: *Vm) !void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const num_args = self.bytecode[self.ip.*];
    self.ip.* += 1;

    if (num_args != 3) return self.ctx.err(
        "`replace` requires two args",
        .{},
        error.InvalidReplace,
        offset,
    );

    const str = self.value_stack.pop();
    if (!value.isAnyStr(str)) return self.ctx.err(
        "`replace` only works on strings.",
        .{},
        error.InvalidReplace,
        offset,
    );
    const str_str = if (value.unboxStr(str)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(str).?.string;

    // No-op
    if (str_str.len == 0) {
        try self.value_stack.append(str);
        return;
    }

    const needle = self.value_stack.pop();
    if (!value.isAnyStr(needle)) return self.ctx.err(
        "`replace` needle must be a string.",
        .{},
        error.InvalidReplace,
        offset,
    );
    const needle_str = if (value.unboxStr(needle)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(needle).?.string;
    if (needle_str.len == 0) return self.ctx.err(
        "`replace` needle can't be empty.",
        .{},
        error.InvalidReplace,
        offset,
    );

    const rep = self.value_stack.pop();
    if (!value.isAnyStr(rep)) return self.ctx.err(
        "`replace` replacement must be a string.",
        .{},
        error.InvalidReplace,
        offset,
    );
    const rep_str = if (value.unboxStr(rep)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(rep).?.string;

    const new_str = try std.mem.replaceOwned(u8, self.allocator, str_str, needle_str, rep_str);

    if (new_str.len < 7) {
        try self.value_stack.append(value.strToValue(new_str));
    } else {
        const obj_ptr = try self.allocator.create(value.Object);
        obj_ptr.* = .{ .string = new_str };
        const obj_addr = @ptrToInt(obj_ptr);
        try self.value_stack.append(value.addrToValue(obj_addr));
    }
}

fn execMemo(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const f = self.value_stack.pop();
    const func_obj_ptr = value.asFunc(f) orelse return self.ctx.err(
        "`memo` only works on functions.",
        .{},
        error.InvalidMemo,
        offset,
    );

    func_obj_ptr.func.memo = true;

    try self.value_stack.append(f);
    self.ip.* += 1;
}

fn execCol(self: *Vm) anyerror!void {
    self.ip.* += 1;
    const offset = self.getOffset();
    self.ip.* += 2;

    const idx = self.value_stack.pop();
    if (value.asUint(idx)) |ui| {
        return self.execColUint(ui, offset);
    } else if (value.isAnyStr(idx)) {
        return self.execColStr(idx, offset);
    } else return self.ctx.err(
        "`col` arg must be a sting or unsigned integer.",
        .{},
        error.InvalidCol,
        offset,
    );
}

fn execColUint(self: *Vm, index: u32, offset: u16) !void {
    const cols_ptr = value.asList(self.scope_stack.columns).?;
    if (index >= cols_ptr.list.items.len) return self.ctx.err(
        "Column index out of bounds.",
        .{},
        error.OutOfBounds,
        offset,
    );

    try self.value_stack.append(cols_ptr.list.items[index]);
    self.ip.* += 1;
}

fn execColStr(self: *Vm, index_val: Value, offset: u16) !void {
    const header_str = if (value.asList(index_val)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(index_val).?.string;
    const index = self.scope_stack.headers.get(header_str) orelse return self.ctx.err(
        "Column '{s}' not found.",
        .{header_str},
        error.InvalidColumnName,
        offset,
    );

    const cols_ptr = value.asList(self.scope_stack.columns).?;
    if (index >= cols_ptr.list.items.len) return self.ctx.err(
        "Column index out of bounds.",
        .{},
        error.OutOfBounds,
        offset,
    );

    try self.value_stack.append(cols_ptr.list.items[index]);
    self.ip.* += 1;
}

// Scopes

fn pushScope(self: *Vm, scope: Scope) !void {
    try self.scope_stack.push(scope);
}

fn popScope(self: *Vm) Scope {
    return self.scope_stack.pop();
}

// Helpers
fn isTruthy(v: Value) bool {
    if (value.isBool(v)) return v == value.val_true;
    if (value.asFloat(v)) |f| return f != 0;
    if (value.asFunc(v)) |_| return true;
    if (value.asInt(v)) |i| return i != 0;
    if (value.asUint(v)) |u| return u != 0;
    if (value.asList(v)) |l| return l.list.items.len != 0;
    if (value.asMap(v)) |m| return m.map.count() != 0;
    if (value.asRange(v)) |r| return r.range[1] - r.range[0] != 0;
    if (value.asString(v)) |s| return s.string.len != 0;
    if (value.unboxStr(v)) |u| return u != 0;

    return v != value.val_nil;
}

fn getNumber(self: Vm, comptime T: type, start: usize, n: usize) T {
    return std.mem.bytesAsSlice(T, self.bytecode[start .. start + n])[0];
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
        compiler.bytecode.items,
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
    try std.testing.expectEqual(value.val_true, got);

    got = try testVmValue(allocator, "false");
    try std.testing.expectEqual(value.val_false, got);

    got = try testVmValue(allocator, "nil");
    try std.testing.expectEqual(value.val_nil, got);

    got = try testVmValue(allocator, "3.1415");
    try std.testing.expectEqual(@as(f64, 3.1415), value.asFloat(got).?);

    got = try testVmValue(allocator, "3.1415 3.1415");
    try std.testing.expectEqual(@as(f64, 3.1415), value.asFloat(got).?);

    got = try testVmValue(allocator, "-3");
    try std.testing.expectEqual(@as(i32, -3), value.asInt(got).?);

    got = try testVmValue(allocator, "-3 -3");
    try std.testing.expectEqual(@as(i32, -3), value.asInt(got).?);

    got = try testVmValue(allocator, "9");
    try std.testing.expectEqual(@as(u32, 9), value.asUint(got).?);

    got = try testVmValue(allocator, "9 9");
    try std.testing.expectEqual(@as(u32, 9), value.asUint(got).?);
}

test "Vm strings" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "\"foo\"");
    var got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("foo", got_str);

    got = try testVmValue(allocator, "\"foobar\" \"foobarbaz\"");
    got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("foobarbaz", got_str);

    got = try testVmValue(allocator,
        \\"foo {#d:0>3# 2} bar"
    );
    got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("foo 002 bar", got_str);
}

test "Vm function literal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\{ foo, bar => 1 }
    );
    const got_func = value.asFunc(got).?.func;
    try std.testing.expectEqual(@as(usize, 2), got_func.params.?.len);
    try std.testing.expectEqual(@as(usize, 6), got_func.bytecode.?.len);
    try std.testing.expectEqual(Compiler.Opcode.uint, @intToEnum(Compiler.Opcode, got_func.bytecode.?[0]));
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
    const got_uint = value.asUint(got).?;
    try std.testing.expectEqual(@as(u32, 42), got_uint);
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
    const got_uint = value.asUint(got).?;
    try std.testing.expectEqual(@as(u32, 3), got_uint);
}

test "Vm infix" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "1 + 2 * 3 / 2 % 2");
    const got_uint = value.asUint(got).?;
    try std.testing.expectEqual(@as(u32, 2), got_uint);

    got = try testVmValue(allocator, "1 + 2 * 3 / 2 % 2 == 2");
    try std.testing.expectEqual(value.val_true, got);

    got = try testVmValue(allocator, "(1 + 2) * 3 / 2 % 2 > 2");
    try std.testing.expectEqual(value.val_false, got);

    got = try testVmValue(allocator,
        \\"foo" ++ "bar"
    );
    var got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("foobar", got_str);

    got = try testVmValue(allocator,
        \\"-" ** 3
    );
    got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("---", got_str);

    got = try testVmValue(allocator, "true and false");
    try std.testing.expectEqual(value.val_false, got);

    got = try testVmValue(allocator, "false or true");
    try std.testing.expectEqual(value.val_true, got);
}

test "Vm prefix" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "foo := 42; -foo");
    const got_int = value.asFloat(got).?;
    try std.testing.expectEqual(@as(f64, -42), got_int);

    got = try testVmValue(allocator, "!true");
    try std.testing.expectEqual(value.val_false, got);
}

test "Vm list literal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "[1, 2, 3]");
    const got_list = value.asList(got).?.list;
    try std.testing.expectEqual(@as(usize, 3), got_list.items.len);
    try std.testing.expectEqual(@as(u32, 1), value.asUint(got_list.items[0]).?);
}

test "Vm map literal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\["a": 1, "b": 2]
    );
    const got_map = value.asMap(got).?.map;
    try std.testing.expectEqual(@as(usize, 2), got_map.count());
    try std.testing.expectEqual(@as(u32, 1), value.asUint(got_map.get("a").?).?);
}

test "Vm subscripts" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\["a": 1, "b": 2]["b"] + [1, 2, 3][1]
    );
    try std.testing.expectEqual(@as(u32, 4), value.asUint(got).?);
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
    try std.testing.expectEqual(@as(u32, 7), value.asUint(got).?);
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
    try std.testing.expectEqual(@as(u32, 6), value.asUint(got).?);
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
    try std.testing.expectEqual(@as(u32, 9), value.asUint(got).?);
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
    try std.testing.expectEqual(@as(u32, 11), value.asUint(got).?);
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
    try std.testing.expectEqual(@as(u32, 4), value.asUint(got).?);
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
    try std.testing.expectEqual(@as(u32, 8), value.asUint(got).?);
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
    try std.testing.expectEqual(@as(u32, 13), value.asUint(got).?);
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
    try std.testing.expectEqual(@as(u32, 13), value.asUint(got).?);
}

test "Vm list range subscript" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\[1, 2, 3, 4, 5][2..<5][1]
    );
    try std.testing.expectEqual(@as(u32, 4), value.asUint(got).?);
}

test "Vm math builtins" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "atan2(0, -1)");
    try std.testing.expectEqual(@as(f64, 3.141592653589793e+00), value.asFloat(got).?);
    got = try testVmValue(allocator, "cos(-1)");
    try std.testing.expectEqual(@as(f64, 5.403023058681398e-01), value.asFloat(got).?);
    got = try testVmValue(allocator, "exp(5)");
    try std.testing.expectEqual(@as(f64, 1.484131591025766e+02), value.asFloat(got).?);
    got = try testVmValue(allocator, "int(-3.9)");
    try std.testing.expectEqual(@as(i32, -3), value.asInt(got).?);
    got = try testVmValue(allocator, "int(3.9)");
    try std.testing.expectEqual(@as(i32, 3), value.asInt(got).?);
    got = try testVmValue(allocator, "log(3.14)");
    try std.testing.expectEqual(@as(f64, 1.144222799920162e+00), value.asFloat(got).?);
    //try testLastValue("rand(10)", Value.new(.{ .uint = 10 }));
    got = try testVmValue(allocator, "sin(3.14)");
    try std.testing.expectEqual(@as(f64, 1.5926529164868282e-03), value.asFloat(got).?);
    got = try testVmValue(allocator, "sqrt(49)");
    try std.testing.expectEqual(@as(f64, 7), value.asFloat(got).?);
}

test "Vm each, map, filter, reduce " {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\total := 0
        \\[1, 2, 3].each() { total = total + it }
        \\total
    );
    try std.testing.expectEqual(@as(u32, 6), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\m := [1, 2, 3].map() { a => a * 2 + index }
        \\m[1]
    );
    try std.testing.expectEqual(@as(u32, 5), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\f := { a => a * 2 + index }
        \\[1, 2, 3].map(f)[1]
    );
    try std.testing.expectEqual(@as(u32, 5), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\[1, 2, 3].filter() { it > 1 }[1]
    );
    try std.testing.expectEqual(@as(u32, 3), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\[1, 2, 3].reduce(1) { acc * it }
    );
    try std.testing.expectEqual(@as(u32, 6), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\(1..=3).reduce(1) { acc * it }
    );
    try std.testing.expectEqual(@as(u32, 6), value.asUint(got).?);
}

test "Vm len" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "[1, 2, 3].len()");
    try std.testing.expectEqual(@as(u32, 3), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\["a": 1, "b": 2, "c": 3].len()
    );
    try std.testing.expectEqual(@as(u32, 3), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\"foo".len()
    );
    try std.testing.expectEqual(@as(u32, 3), value.asUint(got).?);
}

test "Vm map methods" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\["a": 1, "b": 2, "c": 3].keys().len()
    );
    try std.testing.expectEqual(@as(u32, 3), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\["a": 3, "b": 2, "c": 1].keysByValueAsc()[0]
    );
    var got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("c", got_str);

    got = try testVmValue(allocator,
        \\["a": 3, "b": 2, "c": 1].keysByValueDesc()[0]
    );
    got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("a", got_str);

    got = try testVmValue(allocator,
        \\["a": 1, "b": 2, "c": 3].values().len()
    );
    try std.testing.expectEqual(@as(u32, 3), value.asUint(got).?);
}

test "Vm statistics" {
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

    var got = try testVmValue(allocator, mean_input);
    try std.testing.expectEqual(@as(f64, 2), value.asFloat(got).?);

    got = try testVmValue(allocator, median_input);
    try std.testing.expectEqual(@as(f64, 2.5), value.asFloat(got).?);

    got = try testVmValue(allocator, mode_input);
    try std.testing.expectEqual(@as(u32, 2), value.asUint(got).?);

    got = try testVmValue(allocator, stdev_input);
    try std.testing.expectEqual(@as(f64, 1.0671873729054748), value.asFloat(got).?);
}

test "Vm min max" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "[1, 2, 3].min()");
    try std.testing.expectEqual(@as(u32, 1), value.asUint(got).?);

    got = try testVmValue(allocator, "[1, 2, 3].max()");
    try std.testing.expectEqual(@as(u32, 3), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\["a", "z", "B"].min()
    );
    var got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("B", got_str);
}

test "Vm sort" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "[2, 3, 1].sortAsc()[0]");
    try std.testing.expectEqual(@as(u32, 1), value.asUint(got).?);

    got = try testVmValue(allocator, "[2, 3, 1].sortDesc()[0]");
    try std.testing.expectEqual(@as(u32, 3), value.asUint(got).?);
}

test "Vm contains indexOf lastIndexOf" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "[2, 3, 1].contains(3)");
    try std.testing.expectEqual(value.val_true, got);

    got = try testVmValue(allocator, "[2, 3, 1].contains(4)");
    try std.testing.expectEqual(value.val_false, got);

    got = try testVmValue(allocator,
        \\"foo".contains("oo")
    );
    try std.testing.expectEqual(value.val_true, got);

    got = try testVmValue(allocator, "[2, 3, 1].indexOf(1)");
    try std.testing.expectEqual(@as(u32, 2), value.asUint(got).?);

    got = try testVmValue(allocator, "[2, 3, 1].indexOf(4)");
    try std.testing.expectEqual(value.val_nil, got);

    got = try testVmValue(allocator,
        \\"H\u65\u301llo".indexOf("l")
    );
    try std.testing.expectEqual(@as(u32, 2), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\"H\u65\u301llo".lastIndexOf("l")
    );
    try std.testing.expectEqual(@as(u32, 3), value.asUint(got).?);
}

test "Vm reverse, split, join" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator, "[2, 3, 1].reverse()[0]");
    try std.testing.expectEqual(@as(u32, 1), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\"foo,bar,baz".split(",")[1]
    );
    var got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("bar", got_str);

    got = try testVmValue(allocator,
        \\["foo", 1, 2.3, nil].join(",")
    );
    got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("foo,1,2.3,", got_str);
}

test "Vm string methods" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\"H\u65\u301llo".chars()[1]
    );
    var got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("\u{65}\u{301}", got_str);

    got = try testVmValue(allocator,
        \\"Hello".startsWith("Hell")
    );
    try std.testing.expectEqual(value.val_true, got);

    got = try testVmValue(allocator,
        \\"Hello".endsWith("llo")
    );
    try std.testing.expectEqual(value.val_true, got);

    got = try testVmValue(allocator,
        \\"FOO".toLower()
    );
    got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("foo", got_str);

    got = try testVmValue(allocator,
        \\"foo".toUpper()
    );
    got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("FOO", got_str);

    got = try testVmValue(allocator, "\"\"");
    const got_u = value.unboxStr(got).?;
    try std.testing.expectEqual(@as(u64, 0), got_u);

    got = try testVmValue(allocator,
        \\"foo".replace("oo", "ee")
    );
    got_str = if (value.unboxStr(got)) |u| std.mem.sliceTo(std.mem.asBytes(&u), 0) else value.asString(got).?.string;
    try std.testing.expectEqualStrings("fee", got_str);
}

test "Vm list methods" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var got = try testVmValue(allocator,
        \\l := [1]
        \\l.push(2)
        \\l[1]
    );
    try std.testing.expectEqual(@as(u32, 2), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\l := [1]
        \\l.pop()
    );
    try std.testing.expectEqual(@as(u32, 1), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\l := [1, 2, 1, 3, 1, 3]
        \\u := l.unique()
        \\u.reduce(0) { acc + it }
    );
    try std.testing.expectEqual(@as(u32, 6), value.asUint(got).?);

    got = try testVmValue(allocator,
        \\l := [1, 2, 1, 3, 1, 3]
        \\u := l.unique()
        \\u[0]
    );
    try std.testing.expectEqual(@as(u32, 1), value.asUint(got).?);
}
