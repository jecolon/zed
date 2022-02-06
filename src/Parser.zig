const std = @import("std");

const Location = @import("Location.zig");
const Node = @import("Node.zig");
const Token = @import("Token.zig");

allocator: std.mem.Allocator,
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
        try rules.append(Node.new(.stmt_end, 0, self.currentOffset()));
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

    fn isRightAssociative(token: Token) bool {
        return switch (token.ty) {
            .op_add_eq,
            .op_sub_eq,
            .op_mul_eq,
            .op_div_eq,
            .op_mod_eq,
            .punct_equals,
            .op_walrus,
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
        .pd_false, .pd_true => Parser.parseBoolean,
        .float => Parser.parseFloat,
        .int => Parser.parseInt,
        .string => Parser.parseString,
        .uint => Parser.parseUint,

        .op_neg, .punct_bang => Parser.parsePrefix,

        else => null,
    };
}

fn infixFn(self: Parser) InfixFn {
    return switch (self.currentTag()) {
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
fn parseBoolean(self: *Parser) anyerror!Node {
    return Node.new(
        .{ .boolean = self.currentIs(.pd_true) },
        self.token_index.?,
        self.currentOffset(),
    );
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
