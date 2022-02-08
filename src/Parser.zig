const std = @import("std");

const Location = @import("Location.zig");
const Node = @import("Node.zig");
const Token = @import("Token.zig");

allocator: std.mem.Allocator,
can_break: bool = false,
filename: []const u8,
token_index: ?u16 = null,
src: []const u8,
tokens: std.MultiArrayList(Token),

const Parser = @This();

pub const Program = struct {
    rules: []Node,
};

pub fn parse(self: *Parser) !Program {
    var rules = std.ArrayList(Node).init(self.allocator);

    while (try self.next()) |node| {
        try rules.append(node);
        try rules.append(Node.new(.stmt_end, 0, 0));
    }

    return Program{ .rules = rules.items };
}

fn next(self: *Parser) !?Node {
    return if (self.advance()) try self.parseExpression(.lowest) else null;
}

// Pratt Parser
const Precedence = enum {
    lowest,
    redir,
    print,
    assign,
    elvis,
    ternary,
    logic_or,
    logic_and,
    compare,
    range,
    sum,
    product,
    subscript,
    mselect,
    call,
    prefix,

    fn demote(self: *Precedence) void {
        self.* = @intToEnum(Precedence, @enumToInt(self.*) - 1);
    }

    fn forTag(tag: Token.Tag) Precedence {
        return switch (tag) {
            .op_redir_append, .op_redir_clobber => .redir,

            .op_add_eq,
            .op_sub_eq,
            .op_mul_eq,
            .op_div_eq,
            .op_mod_eq,
            .punct_equals,
            .op_define,
            .op_elvis_eq,
            => .assign,

            .op_elvis => .elvis,
            .punct_question => .ternary,

            .kw_or => .logic_or,
            .kw_and => .logic_and,

            .punct_lt,
            .punct_gt,
            .punct_tilde,
            .op_nomatch,
            .op_xmatch,
            .op_lte,
            .op_gte,
            .op_eq,
            .op_neq,
            => .compare,

            .op_range_ex, .op_range_in => .range,

            .punct_plus,
            .punct_minus,
            .op_concat,
            => .sum,

            .punct_star,
            .punct_slash,
            .punct_percent,
            => .product,

            .punct_dot => .mselect,
            .punct_lparen => .call,
            .punct_lbracket => .subscript,

            else => .lowest,
        };
    }

    fn lessThan(self: Precedence, other: Precedence) bool {
        return @enumToInt(self) < @enumToInt(other);
    }

    fn isRightAssociative(tag: Token.Tag) bool {
        return switch (tag) {
            .op_add_eq,
            .op_sub_eq,
            .op_mul_eq,
            .op_div_eq,
            .op_mod_eq,
            .punct_equals,
            .op_define,
            .op_redir_append,
            .op_redir_clobber,
            => true,

            else => false,
        };
    }
};

const PrefixFn = fn (*Parser) anyerror!Node;
const InfixFn = fn (*Parser, Node) anyerror!Node;

fn prefixFn(tag: Token.Tag) ?PrefixFn {
    return switch (tag) {
        .float => Parser.parseFloat,
        .ident => Parser.parseIdent,
        .int => Parser.parseInt,
        .string => Parser.parseString,
        .uint => Parser.parseUint,

        .kw_break => Parser.parseBreak,
        .kw_continue => Parser.parseContinue,
        .kw_if => Parser.parseIf,
        .kw_while => Parser.parseWhile,

        .op_neg, .punct_bang => Parser.parsePrefix,

        .pd_false, .pd_true => Parser.parseBoolean,
        .pd_nil => Parser.parseNil,

        else => null,
    };
}

fn infixFn(self: Parser) InfixFn {
    return switch (self.currentTag()) {
        .punct_plus,
        .punct_minus,
        .punct_star,
        .punct_slash,
        .punct_percent,
        .punct_lt,
        .op_lte,
        .punct_gt,
        .op_gte,
        .op_eq,
        .op_neq,
        .kw_and,
        .kw_or,
        => Parser.parseInfix,

        .punct_equals => Parser.parseAssign,
        .op_define => Parser.parseDefine,

        else => unreachable,
    };
}

fn parseExpression(self: *Parser, precedence: Precedence) anyerror!Node {
    const tag = self.currentTag();

    const pfn = prefixFn(tag) orelse {
        const location = Location.getLocation(self.filename, self.src, self.currentOffset());
        std.log.err("No parse function for {}; {}", .{ tag, location });
        return error.NoParseFn;
    };

    var left = try pfn(self);

    while (self.peekPrecedence()) |peek_prec| {
        if (!precedence.lessThan(peek_prec)) break;
        _ = self.advance();
        const ifn = self.infixFn();
        left = try ifn(self, left);
    }

    _ = self.skipTag(.punct_semicolon);

    return left;
}

// Parse functions.
fn parseAssign(self: *Parser, name: Node) anyerror!Node {
    if (name.ty != .ident) {
        const location = Location.getLocation(self.filename, self.src, name.offset);
        std.log.err("Invalid assignment left hand side {s}; {}", .{ @tagName(name.ty), location });
        return error.InvalidAssign;
    }

    var node = Node.new(
        .{ .assign = .{ .name = try self.allocator.create(Node), .value = try self.allocator.create(Node) } },
        self.token_index.?,
        self.currentOffset(),
    );
    node.ty.assign.name.* = name;
    try self.expectNext();
    node.ty.assign.value.* = try self.parseExpression(.lowest);
    return node;
}

fn parseBoolean(self: *Parser) anyerror!Node {
    return Node.new(
        .{ .boolean = self.currentIs(.pd_true) },
        self.token_index.?,
        self.currentOffset(),
    );
}

fn parseBreak(self: *Parser) anyerror!Node {
    if (!self.can_break) {
        const location = Location.getLocation(self.filename, self.src, self.currentOffset());
        std.log.err("break not allowed here; {}", .{location});
        return error.InvalidBreak;
    }

    return Node.new(
        .loop_break,
        self.token_index.?,
        self.currentOffset(),
    );
}

fn parseContinue(self: *Parser) anyerror!Node {
    if (!self.can_break) {
        const location = Location.getLocation(self.filename, self.src, self.currentOffset());
        std.log.err("continue not allowed here; {}", .{location});
        return error.InvalidContinue;
    }

    return Node.new(
        .loop_continue,
        self.token_index.?,
        self.currentOffset(),
    );
}

fn parseDefine(self: *Parser, name: Node) anyerror!Node {
    if (name.ty != .ident) {
        const location = Location.getLocation(self.filename, self.src, name.offset);
        std.log.err("Name definition of non-name {s}; {}", .{ @tagName(name.ty), location });
        return error.InvalidDefine;
    }

    var node = Node.new(
        .{ .define = .{ .name = try self.allocator.create(Node), .value = try self.allocator.create(Node) } },
        self.token_index.?,
        self.currentOffset(),
    );
    node.ty.define.name.* = name;
    try self.expectNext();
    node.ty.define.value.* = try self.parseExpression(.lowest);
    return node;
}

fn parseFloat(self: *Parser) anyerror!Node {
    const start = self.currentOffset();
    const end = self.currentOffset() + self.currentLen();
    const float = try std.fmt.parseFloat(f64, self.src[start..end]);

    return Node.new(
        .{ .float = float },
        self.token_index.?,
        self.currentOffset(),
    );
}

fn parseIdent(self: *Parser) anyerror!Node {
    const start = self.currentOffset();
    const end = self.currentOffset() + self.currentLen();
    return Node.new(.{ .ident = self.src[start..end] }, self.token_index.?, start);
}

fn parseIf(self: *Parser) anyerror!Node {
    try self.expectTag(.punct_lparen);
    try self.expectNext();
    const condition_ptr = try self.allocator.create(Node);
    condition_ptr.* = try self.parseExpression(.lowest);
    try self.expectTag(.punct_rparen);

    var then_branch_list = std.ArrayList(Node).init(self.allocator);

    if (self.skipTag(.punct_lbrace)) {
        try self.parseNodes(&then_branch_list, .punct_rbrace, .punct_semicolon);
    } else {
        try self.expectNext();
        const then_node = try self.parseExpression(.lowest);
        try then_branch_list.append(then_node);
    }

    var else_branch_list = std.ArrayList(Node).init(self.allocator);

    if (self.skipTag(.kw_else)) {
        if (self.skipTag(.punct_lbrace)) {
            try self.parseNodes(&else_branch_list, .punct_rbrace, .punct_semicolon);
        } else {
            try self.expectNext();
            const else_node = try self.parseExpression(.lowest);
            try else_branch_list.append(else_node);
        }
    } else {
        // Synthetic nil if no else branch provided.
        try else_branch_list.append(Node.new(.nil, self.token_index.?, self.currentOffset()));
    }

    return Node.new(.{ .conditional = .{
        .condition = condition_ptr,
        .then_branch = then_branch_list.items,
        .else_branch = else_branch_list.items,
    } }, self.token_index.?, self.currentOffset());
}

fn parseInfix(self: *Parser, left: Node) anyerror!Node {
    const left_ptr = try self.allocator.create(Node);
    left_ptr.* = left;

    var node = Node.new(
        .{ .infix = .{
            .left = left_ptr,
            .op = self.currentTag(),
            .right = undefined,
        } },
        self.token_index.?,
        self.currentOffset(),
    );
    try self.expectNext();
    node.ty.infix.right = try self.allocator.create(Node);
    var precedence = Precedence.forTag(node.ty.infix.op);
    if (Precedence.isRightAssociative(node.ty.infix.op)) precedence.demote();
    node.ty.infix.right.* = try self.parseExpression(precedence);
    return node;
}

fn parseInt(self: *Parser) anyerror!Node {
    const start = self.currentOffset();
    const end = self.currentOffset() + self.currentLen();
    const int = try std.fmt.parseInt(isize, self.src[start..end], 0);

    return Node.new(
        .{ .int = int },
        self.token_index.?,
        self.currentOffset(),
    );
}

fn parseNil(self: *Parser) anyerror!Node {
    return Node.new(
        .nil,
        self.token_index.?,
        self.currentOffset(),
    );
}

fn parsePrefix(self: *Parser) anyerror!Node {
    var node = Node.new(
        .{ .prefix = .{ .op = self.currentTag(), .operand = undefined } },
        self.token_index.?,
        self.currentOffset(),
    );
    try self.expectNext();
    node.ty.prefix.operand = try self.allocator.create(Node);
    node.ty.prefix.operand.* = try self.parseExpression(.prefix);
    return node;
}

fn parseString(self: *Parser) anyerror!Node {
    const start = self.currentOffset() + 1;
    const end = self.currentOffset() + self.currentLen() - 1;

    return Node.new(
        .{ .string = self.src[start..end] },
        self.token_index.?,
        self.currentOffset(),
    );
}

fn parseUint(self: *Parser) anyerror!Node {
    const start = self.currentOffset();
    const end = self.currentOffset() + self.currentLen();
    const uint = try std.fmt.parseUnsigned(usize, self.src[start..end], 0);

    return Node.new(
        .{ .uint = uint },
        self.token_index.?,
        self.currentOffset(),
    );
}

fn parseWhile(self: *Parser) anyerror!Node {
    self.can_break = true;
    defer self.can_break = false;

    var node = Node.new(
        .{ .loop = .{ .condition = undefined, .body = undefined } },
        self.token_index.?,
        self.currentOffset(),
    );
    // Condition
    try self.expectTag(.punct_lparen);
    try self.expectNext();
    node.ty.loop.condition = try self.allocator.create(Node);
    node.ty.loop.condition.* = try self.parseExpression(.lowest);
    try self.expectTag(.punct_rparen);
    // Body
    var body_list = std.ArrayList(Node).init(self.allocator);

    if (self.skipTag(.punct_lbrace)) {
        try self.parseNodes(&body_list, .punct_rbrace, .punct_semicolon);
    } else {
        try self.expectNext();
        const body_node = try self.parseExpression(.lowest);
        try body_list.append(body_node);
    }

    // while loops need to clean up the stack after every iteration.
    try body_list.append(Node.new(.stmt_end, 0, 0));

    node.ty.loop.body = body_list.items;

    return node;
}

// Parser movement.
fn len(self: Parser) u16 {
    return @intCast(u16, self.tokens.items(.tag).len);
}

fn currentLen(self: Parser) u16 {
    return self.tokens.items(.len)[self.token_index.?];
}

fn currentOffset(self: Parser) u16 {
    return self.tokens.items(.offset)[self.token_index.?];
}

fn currentTag(self: Parser) Token.Tag {
    return self.tokens.items(.tag)[self.token_index.?];
}

fn currentIs(self: Parser, tag: Token.Tag) bool {
    return self.tokens.items(.tag)[self.token_index.?] == tag;
}

fn advance(self: *Parser) bool {
    if (self.token_index) |*index| index.* += 1 else self.token_index = 0;
    return self.token_index.? < self.len();
}

fn peekNIs(self: Parser, n: usize, tag: Token.Tag) bool {
    if (self.token_index) |index| {
        return if (index + n < self.len()) self.tokens.items(.tag)[index + n] == tag else false;
    } else {
        return if (n - 1 < self.len()) self.tokens.items(.tag)[n - 1] == tag else false;
    }
}

fn peekIs(self: Parser, tag: Token.Tag) bool {
    return self.peekNIs(1, tag);
}

fn peekPrecedence(self: Parser) ?Precedence {
    if (self.token_index) |index| {
        return if (index + 1 < self.len()) Precedence.forTag(self.tokens.items(.tag)[index + 1]) else null;
    } else {
        return Precedence.forTag(self.tokens.items(.tag)[0]);
    }
}

fn skipTag(self: *Parser, tag: Token.Tag) bool {
    return if (self.peekIs(tag)) self.advance() else false;
}

fn expectNext(self: *Parser) !void {
    if (!self.advance()) {
        const location = Location.getLocation(self.filename, self.src, self.len() - 1);
        std.log.err("Unexpected end of tokens; {}", .{location});
        return error.UnexpectedEndOfTokens;
    }
}

fn expectTag(self: *Parser, tag: Token.Tag) !void {
    try self.expectNext();
    if (!self.currentIs(tag)) {
        const location = Location.getLocation(self.filename, self.src, self.currentOffset());
        std.log.err("Expected {}; {}", .{ tag, location });
        return error.UnexpectedToken;
    }
}

// Helpers
fn parseNodes(self: *Parser, list: *std.ArrayList(Node), stop: Token.Tag, skip: Token.Tag) anyerror!void {
    while (!self.skipTag(stop)) {
        try self.expectNext();
        const node = try self.parseExpression(.lowest);
        try list.append(node);
        try list.append(Node.new(.stmt_end, 0, 0));
        _ = self.skipTag(skip);
    }

    _ = list.pop(); // Remove last stmt_end
}

// Tests

test "Parser booleans" {
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const input = "false true";
    const Lexer = @import("Lexer.zig");
    var lexer = Lexer{ .filename = "inline", .src = input };
    var tokens = try lexer.lex(arena.allocator());
    var parser = Parser{
        .allocator = arena.allocator(),
        .filename = "inline",
        .src = input,
        .tokens = tokens,
    };
    const program = try parser.parse();

    try std.testing.expectEqual(@as(usize, 4), program.rules.len);
    try std.testing.expectEqual(Node.Type.boolean, program.rules[0].ty);
    try std.testing.expectEqual(false, program.rules[0].ty.boolean);
    try std.testing.expectEqual(@as(u16, 0), program.rules[0].offset);
    try std.testing.expectEqual(Node.Type.boolean, program.rules[2].ty);
    try std.testing.expectEqual(true, program.rules[2].ty.boolean);
    try std.testing.expectEqual(@as(u16, 6), program.rules[2].offset);
}
