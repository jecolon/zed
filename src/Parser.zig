const std = @import("std");

const Location = @import("Location.zig");
const Node = @import("Node.zig");
const Token = @import("Token.zig");

allocator: std.mem.Allocator,
filename: []const u8,
offset: ?u16 = null,
src: []const u8,
tokens: std.MultiArrayList(Token),

const Parser = @This();

pub const Program = struct {
    rules: []Node,
};

pub fn parse(self: *Parser) !Program {
    var rules = std.ArrayList(Node).init(self.allocator);
    const stmt_end = Node{ .token_offsets = &[_]u16{}, .tag = .stmt_end };

    while (try self.next()) |node| {
        try rules.append(node);
        try rules.append(stmt_end);
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
    var token_offsets = try self.allocator.alloc(u16, 1);
    token_offsets[0] = self.offset.?;
    var node = Node{ .token_offsets = token_offsets, .tag = .bool_false };
    if (self.currentIs(.pd_true)) node.tag = .bool_true;
    return node;
}

// Parser movement.
fn len(self: Parser) u16 {
    return @intCast(u16, self.tokens.items(.tag).len);
}

fn currentOffset(self: Parser) u16 {
    return self.tokens.items(.offset)[self.offset.?];
}

fn currentTag(self: Parser) Token.Tag {
    return self.tokens.items(.tag)[self.offset.?];
}

fn currentIs(self: Parser, tag: Token.Tag) bool {
    return self.tokens.items(.tag)[self.offset.?] == tag;
}

fn advance(self: *Parser) bool {
    if (self.offset) |*offset| offset.* += 1 else self.offset = 0;
    return self.offset.? < self.len();
}

fn peekNIs(self: Parser, n: usize, tag: Token.Tag) bool {
    if (self.offset) |offset| {
        return if (offset + n < self.len()) self.tokens.items(.tag)[offset + n] == tag else false;
    } else {
        return if (n - 1 < self.len()) self.tokens.items(.tag)[n - 1] == tag else false;
    }
}

fn peekIs(self: Parser, tag: Token.Tag) bool {
    return self.peekNIs(1, tag);
}

fn peekPrecedence(self: Parser) ?Precedence {
    if (self.offset) |offset| {
        return if (offset + 1 < self.len()) Precedence.forTag(self.tokens.items(.tag)[offset + 1]) else null;
    } else {
        return Precedence.forTag(self.tokens.items(.tag)[0]);
    }
}

fn skipTag(self: *Parser, tag: Token.Tag) bool {
    return if (self.peekIs(tag)) self.advance() else false;
}

fn expectNext(self: *Parser) !void {
    if (!self.advance()) {
        const location = Location.getLocation(self.filename, self.src, self.src.len() - 1);
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
    try std.testing.expectEqual(Node.Tag.bool_false, program.rules[0].tag);
    try std.testing.expectEqual(@as(u16, 0), tokens.items(.offset)[program.rules[0].token_offsets[0]]);
    try std.testing.expectEqual(Node.Tag.bool_true, program.rules[2].tag);
    try std.testing.expectEqual(@as(u16, 6), tokens.items(.offset)[program.rules[2].token_offsets[0]]);
}
