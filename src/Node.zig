const std = @import("std");

const Token = @import("Token.zig");

pub const Type = union(enum) {
    boolean: bool,
    builtin: Token.Tag,
    float: f64,
    func_return: *Node,
    global: Token.Tag,
    ident: []const u8,
    int: i32,
    list: []Node,
    map: []Entry,
    loop_break,
    loop_continue,
    nil,
    regex: []const u8,
    stmt_end,
    string: []const Segment,
    uint: u32,

    assign: Assign,
    call: Call,
    conditional: Conditional,
    define: Assign,
    event: Event,
    func: Function,
    infix: Infix,
    loop: Loop,
    prefix: Prefix,
    range: Range,
    rec_range: RecRange,
    redir: Redir,
    subscript: Subscript,
};

offset: u16,
ty: Type,

const Node = @This();

pub fn new(ty: Type, offset: u16) Node {
    return .{ .offset = offset, .ty = ty };
}

const Call = struct {
    args: []Node,
    callee: *Node,

    pub fn format(call: Call, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{}(", .{call.callee.*});
        for (call.args) |a, i| {
            if (i != 0) try writer.writeByte(',');
            _ = try writer.print("{}", .{a});
        }
        try writer.writeByte(')');
    }
};

const Conditional = struct {
    condition: *Node,
    then_branch: []Node,
    else_branch: []Node,

    pub fn format(conditional: Conditional, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("if ({}) {{", .{conditional.condition.*});
        for (conditional.then_branch) |n| _ = try writer.print("{} ", .{n});
        try writer.writeAll("} else {");
        for (conditional.else_branch) |n| _ = try writer.print("{} ", .{n});
        try writer.writeByte('}');
    }
};

pub const Combo = enum {
    none,
    add,
    sub,
    mul,
    div,
    mod,

    pub fn format(combo: Combo, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (combo) {
            .none => try writer.writeByte('='),
            .add => try writer.writeAll("+="),
            .sub => try writer.writeAll("-="),
            .mul => try writer.writeAll("*="),
            .div => try writer.writeAll("/="),
            .mod => try writer.writeAll("%="),
        }
    }
};

const Assign = struct {
    combo: Combo = .none,
    lvalue: *Node,
    rvalue: *Node,

    pub fn format(assign: Assign, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{} {} {}", .{ assign.lvalue.*, assign.combo, assign.rvalue.* });
    }
};

pub const Entry = struct {
    key: Node,
    value: Node,
};

const EventType = enum {
    init,
    file,
    rec,
    exit,

    pub fn format(event: EventType, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (event) {
            .init => try writer.writeAll("onInit"),
            .file => try writer.writeAll("onFile"),
            .rec => try writer.writeAll("onRec"),
            .exit => try writer.writeAll("onExit"),
        }
    }
};

const Event = struct {
    nodes: []Node,
    ty: EventType,

    pub fn format(event: Event, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{} {{", .{event.ty});
        for (event.nodes) |n| _ = try writer.print("{} ", .{n});
        try writer.writeByte('}');
    }
};

const Function = struct {
    name: []const u8 = "",
    params: [][]const u8,
    body: []Node,

    pub fn format(func: Function, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (func.name.len != 0) try writer.writeAll(func.name);
        try writer.writeByte('{');
        for (func.params) |param, i| {
            if (i != 0) try writer.writeByte(',');
            try writer.writeAll(param);
        }
        try writer.writeAll(" => ");
        for (func.body) |n| _ = try writer.print("{} ", .{n});
        try writer.writeByte('}');
    }
};

const Infix = struct {
    left: *Node,
    op: Token.Tag,
    right: *Node,

    pub fn format(infix: Infix, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{} {s} {}", .{ infix.left.*, @tagName(infix.op), infix.right.* });
    }
};

const Loop = struct {
    condition: *Node,
    body: []Node,
    is_do: bool,

    pub fn format(loop: Loop, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (loop.is_do) {
            try writer.writeAll("do {");
            for (loop.body) |n| _ = try writer.print("{} ", .{n});
            _ = try writer.print("}} while ({})", .{loop.condition.*});
        } else {
            _ = try writer.print("while ({}) {{", .{loop.condition.*});
            for (loop.body) |n| _ = try writer.print("{} ", .{n});
            try writer.writeByte('}');
        }
    }
};

const Prefix = struct {
    op: Token.Tag,
    operand: *Node,

    pub fn format(prefix: Prefix, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{s}{}", .{ @tagName(prefix.op), prefix.operand.* });
    }
};

const Range = struct {
    inclusive: bool,
    from: *Node,
    to: *Node,

    pub fn format(range: Range, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const op = if (range.inclusive) "..=" else "..<";
        _ = try writer.print("{}{s}{}", .{ range.from.*, op, range.to.* });
    }
};

const RecRange = struct {
    from: ?*Node,
    to: ?*Node,
    action: []Node,
    id: u8,
    exclusive: bool,

    pub fn format(recrange: RecRange, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("RecRange{}", .{recrange.id});
    }
};

const Redir = struct {
    expr: *Node,
    file: *Node,
    clobber: bool,

    pub fn format(redir: Redir, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const op = if (redir.clobber) "!>" else "+>";
        _ = try writer.print("{} {s} {}", .{ redir.expr.*, op, redir.file.* });
    }
};

pub const Ipol = struct {
    spec: ?[]const u8,
    nodes: []Node,
    offset: u16,

    pub fn format(ipol: Ipol, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeByte('{');
        if (ipol.spec) |spec| _ = try writer.print("#{s}#", .{spec});
        for (ipol.nodes) |n| _ = try writer.print("{} ", .{n});
        try writer.writeByte('}');
    }
};

pub const Segment = union(enum) {
    ipol: Ipol,
    plain: []const u8,

    pub fn format(segment: Segment, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (segment) {
            .ipol => |i| _ = try writer.print("{}", .{i}),
            .plain => |s| try writer.writeAll(s),
        }
    }
};

const Subscript = struct {
    container: *Node,
    index: *Node,

    pub fn format(subscript: Subscript, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{}[{}]", .{ subscript.container.*, subscript.index.* });
    }
};

pub fn format(node: Node, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    switch (node.ty) {
        .boolean => |b| _ = try writer.print("{}", .{b}),
        .builtin => |b| try writer.writeAll(@tagName(b)),
        .float => |f| _ = try writer.print("{d}", .{f}),
        .func_return => |r| _ = try writer.print("return {}", .{r.*}),
        .global => |g| try writer.writeAll(@tagName(g)),
        .ident => |i| try writer.writeAll(i),
        .int => |i| _ = try writer.print("{}", .{i}),
        .list => |l| try printList(l, writer),
        .map => |m| try printMap(m, writer),
        .loop_break => try writer.writeAll("break"),
        .loop_continue => try writer.writeAll("continue"),
        .nil => try writer.writeAll("nil"),
        .regex => |r| try writer.writeAll(r),
        .stmt_end => try writer.writeByte(';'),
        .string => |s| try printString(s, writer),
        .uint => |u| _ = try writer.print("{}", .{u}),
        .assign => |a| _ = try writer.print("{}", .{a}),
        .call => |c| _ = try writer.print("{}", .{c}),
        .conditional => |c| _ = try writer.print("{}", .{c}),
        .define => |d| _ = try writer.print("{}", .{d}),
        .event => |e| _ = try writer.print("{}", .{e}),
        .func => |f| _ = try writer.print("{}", .{f}),
        .infix => |i| _ = try writer.print("{}", .{i}),
        .loop => |l| _ = try writer.print("{}", .{l}),
        .prefix => |p| _ = try writer.print("{}", .{p}),
        .range => |r| _ = try writer.print("{}", .{r}),
        .rec_range => |r| _ = try writer.print("{}", .{r}),
        .redir => |r| _ = try writer.print("{}", .{r}),
        .subscript => |s| _ = try writer.print("{}", .{s}),
    }
}

fn printList(l: []Node, writer: anytype) !void {
    try writer.writeByte('[');
    for (l) |n, i| {
        if (i != 0) try writer.writeByte(',');
        _ = try writer.print("{}", .{n});
    }
    try writer.writeByte(']');
}
fn printMap(m: []Entry, writer: anytype) !void {
    try writer.writeByte('[');
    for (m) |e, i| {
        if (i != 0) try writer.writeByte(',');
        _ = try writer.print("{}:{}", .{ e.key, e.value });
    }
    try writer.writeByte(']');
}
fn printString(s: []const Segment, writer: anytype) !void {
    try writer.writeByte('"');
    for (s) |seg| _ = try writer.print("{}", .{seg});
    try writer.writeByte('"');
}
