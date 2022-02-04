const std = @import("std");

const Location = @import("Location.zig");
const Token = @import("Token.zig");

filename: []const u8,
offset: ?u16 = null,
src: []const u8,

const Lexer = @This();

// API
pub fn lex(self: *Lexer, allocator: std.mem.Allocator) !std.MultiArrayList(Token) {
    var tokens = std.MultiArrayList(Token){};
    errdefer tokens.deinit(allocator);
    var after_newline = false;

    while (true) {
        const token_opt = self.next() catch |err| return self.reportErr(err, self.offset.?);

        if (token_opt) |token| {
            if (token.is(.punct_newline)) {
                after_newline = true;
            } else {
                if (after_newline) {
                    if (token.is(.punct_lparen) or token.is(.punct_lbracket)) try tokens.append(allocator, self.oneChar(.punct_semicolon));
                    after_newline = false;
                }

                try tokens.append(allocator, token);
            }
        } else break;
    }

    return tokens;
}

fn next(self: *Lexer) !?Token {
    if (self.skipWhitespce()) |byte| {
        if (isIdentStart(byte)) return self.lexIdent();
        if (isNumStart(byte)) return self.lexNumber(byte);

        return switch (byte) {
            '#' => try self.lexComment(),
            '\n' => self.lexNewline(),
            '"' => try self.lexString(),
            '@' => try self.lexGlobal(),

            ':',
            '!',
            '<',
            '>',
            '=',
            '~',
            '*',
            '/',
            '%',
            '.',
            '?',
            => self.lexOp(byte),

            ';' => self.oneChar(.punct_semicolon),
            '{' => self.oneChar(.punct_lbrace),
            '}' => self.oneChar(.punct_rbrace),
            ',' => self.oneChar(.punct_comma),
            '$' => self.oneChar(.punct_dollar),
            '(' => self.oneChar(.punct_lparen),
            ')' => self.oneChar(.punct_rparen),
            '[' => self.oneChar(.punct_lbracket),
            ']' => self.oneChar(.punct_rbracket),
            '|' => self.oneChar(.punct_pipe),

            else => error.UnknownByte,
        };
    }
    return null;
}

// Lexing
fn lexComment(self: *Lexer) anyerror!?Token {
    while (self.advance()) |byte| {
        switch (byte) {
            '\n', '#' => break,
            else => continue,
        }
    }
    return try self.next();
}

fn lexGlobal(self: *Lexer) !Token {
    const start = self.offset.?;

    if (self.peek()) |peek_byte| {
        if (isWhitespace(peek_byte)) return self.oneChar(.punct_at);
        if (!isIdentByte(peek_byte)) return error.InvalidGlobal;
    } else return self.oneChar(.punct_at);

    self.run(isIdentByte);
    const src = self.src[start .. self.offset.? + 1];

    return Token.new(.op_global, start, src.len);
}

fn lexIdent(self: *Lexer) Token {
    const start = self.offset.?;
    self.run(isIdentByte);
    const src = self.src[start .. self.offset.? + 1];
    const tag = if (Token.predef.get(src)) |tag| tag else Token.Tag.ident;
    return Token.new(tag, start, src.len);
}

fn lexNumber(self: *Lexer, byte: u8) Token {
    if ('+' == byte) {
        // Plus is special
        var token = self.oneChar(.punct_plus);
        if (self.peek() == null or self.peekPred(isWhitespace)) return token;

        if (self.skipByte('+')) {
            token.tag = .op_concat;
            token.len = 2;
            return token;
        } else if (self.skipByte('=')) {
            token.tag = .op_add_eq;
            token.len = 2;
            return token;
        } else if (self.skipByte('>')) {
            token.tag = .op_redir_append;
            token.len = 2;
            return token;
        }
    }

    if ('-' == byte) {
        // Minus is special too
        var token = self.oneChar(.punct_minus);
        if (self.peek() == null or self.peekPred(isWhitespace)) return token;

        if (self.peekPred(isIdentStart)) {
            token.tag = .op_neg;
            return token;
        }

        if (self.skipByte('=')) {
            token.tag = .op_sub_eq;
            token.len = 2;
            return token;
        }
    }

    const start = self.offset.?;

    while (self.peek()) |peek_byte| {
        if ('.' == peek_byte and self.peekNIs(2, '.')) break;
        if (!isNumeric(peek_byte)) break;
        _ = self.advance();
    }

    const src = self.src[start .. self.offset.? + 1];
    var token = Token.new(.uint, start, src.len);

    if (src.len > 2) {
        const is_float = for (src[1..]) |num_byte| {
            if (isFloatOnly(num_byte)) break true;
        } else false;

        if (is_float) token.tag = .float;
    }

    if ('-' == src[0] or '+' == src[0]) token.tag = .int;

    return token;
}

fn lexNewline(self: *Lexer) Token {
    const token = self.oneChar(.punct_newline);
    self.run(isNewline);
    return token;
}

fn lexCombineAssing(self: *Lexer, base: Token.Tag, with_eq: Token.Tag) Token {
    var token = self.oneChar(base);
    if (self.skipByte('=')) {
        token.tag = with_eq;
        token.len = 2;
    }
    return token;
}

fn lexOp(self: *Lexer, byte: u8) Token {
    return switch (byte) {
        ':' => self.lexCombineAssing(.punct_colon, .op_define),
        '=' => op: {
            if (self.skipByte('>')) break :op self.twoChar(.punct_fat_rarrow);
            break :op self.lexCombineAssing(.punct_equals, .op_eq);
        },
        '!' => op: {
            var token = self.oneChar(.punct_bang);
            if (self.skipByte('=')) {
                token.tag = .op_neq;
                token.len = 2;
            } else if (self.skipByte('~')) {
                token.tag = .op_nomatch;
                token.len = 2;
            } else if (self.skipByte('>')) {
                token.tag = .op_redir_clobber;
                token.len = 2;
            }
            break :op token;
        },
        '<' => self.lexCombineAssing(.punct_lt, .op_lte),
        '>' => self.lexCombineAssing(.punct_gt, .op_gte),
        '~' => self.lexCombineAssing(.punct_tilde, .op_xmatch),
        '*' => self.lexCombineAssing(.punct_star, .op_mul_eq),
        '/' => self.lexCombineAssing(.punct_slash, .op_div_eq),
        '%' => self.lexCombineAssing(.punct_percent, .op_mod_eq),
        '.' => op: {
            var token = self.oneChar(.punct_dot);

            if (self.peekStr(".<")) {
                self.skipN(2);
                token.tag = .op_range_ex;
                token.len = 3;
            } else if (self.peekStr(".=")) {
                self.skipN(2);
                token.tag = .op_range_in;
                token.len = 3;
            }

            break :op token;
        },
        '?' => op: {
            var token = self.oneChar(.punct_question);

            if (self.skipByte(':')) {
                token.tag = .op_elvis;
                token.len = 2;
            } else if (self.skipByte('=')) {
                token.tag = .op_elvis_eq;
                token.len = 2;
            }

            break :op token;
        },

        else => unreachable,
    };
}

fn lexString(self: *Lexer) !Token {
    const start = self.offset.?;
    var nest_level: usize = 0;
    var reached_eof = true;

    while (self.advance()) |byte| {
        if ('{' == byte and self.peekIs('{')) {
            _ = self.advance();
        } else if ('}' == byte and self.peekIs('}')) {
            _ = self.advance();
        } else if ('{' == byte and !self.peekIs('{')) {
            nest_level += 1;
        } else if (nest_level > 0 and '}' == byte and !self.peekIs('}')) {
            nest_level -= 1;
        } else if ('\\' == byte and self.peekIs('"')) {
            _ = self.advance();
        } else if ('"' == byte and nest_level == 0) {
            reached_eof = false;
            break;
        }
    }

    if (reached_eof) return error.UnterminatedString;

    const src = self.src[start .. self.offset.? + 1];
    return Token.new(.string, start, src.len);
}

// Scanning
fn advance(self: *Lexer) ?u8 {
    if (self.src.len == 0) return null;
    if (self.offset) |*offset| {
        offset.* += 1;
        if (offset.* >= self.src.len) return null;
    } else self.offset = 0;

    return self.src[self.offset.?];
}

fn peekN(self: Lexer, n: u16) ?u8 {
    if (self.src.len == 0) return null;

    if (self.offset) |offset| {
        return if (offset + n < self.src.len) return self.src[offset + n] else null;
    } else {
        return if (n - 1 < self.src.len) self.src[n - 1] else null;
    }
}

fn peekNIs(self: Lexer, n: u16, byte: u8) bool {
    return if (self.peekN(n)) |peek_byte| peek_byte == byte else false;
}

fn peek(self: Lexer) ?u8 {
    return self.peekN(1);
}

fn peekIs(self: Lexer, byte: u8) bool {
    return self.peekNIs(1, byte);
}

fn peekPred(self: Lexer, pred: Predicate) bool {
    return if (self.peek()) |peek_byte| pred(peek_byte) else false;
}

fn peekStr(self: Lexer, str: []const u8) bool {
    if (self.peek() == null) return false;

    if (self.offset) |offset| {
        return std.mem.startsWith(u8, self.src[offset + 1 ..], str);
    } else {
        return std.mem.startsWith(u8, self.src, str);
    }
}

fn skipN(self: *Lexer, n: usize) void {
    var i: usize = 0;
    while (i < n) : (i += 1) _ = self.advance();
}

fn skipByte(self: *Lexer, byte: u8) bool {
    if (self.peekIs(byte)) {
        _ = self.advance();
        return true;
    } else return false;
}

fn run(self: *Lexer, predicate: Predicate) void {
    while (self.peek()) |peek_byte| {
        if (!predicate(peek_byte)) break;
        _ = self.advance();
    }
}

fn skipWhitespce(self: *Lexer) ?u8 {
    self.run(isWhitespace);
    return self.advance();
}

// Predicates
const Predicate = fn (u8) bool;

fn isFloatOnly(byte: u8) bool {
    return switch (byte) {
        '+',
        '-',
        '.',
        'p',
        => true,
        else => false,
    };
}

fn isIdentByte(byte: u8) bool {
    return switch (byte) {
        '_',
        'a'...'z',
        'A'...'Z',
        '0'...'9',
        => true,
        else => false,
    };
}

fn isIdentStart(byte: u8) bool {
    return switch (byte) {
        '_',
        'a'...'z',
        'A'...'Z',
        => true,
        else => false,
    };
}

fn isNewline(byte: u8) bool {
    return '\n' == byte or '\r' == byte;
}

fn isNumeric(byte: u8) bool {
    return switch (byte) {
        '+',
        '-',
        '_',
        '.',
        'o',
        'p',
        'x',
        'a'...'f',
        'A'...'F',
        '0'...'9',
        => true,
        else => false,
    };
}

fn isNumStart(byte: u8) bool {
    return ('0' <= byte and byte <= '9') or
        '+' == byte or
        '-' == byte;
}

fn isWhitespace(byte: u8) bool {
    return ' ' == byte or '\t' == byte or '\r' == byte;
}

fn requiresSemi(tag: Token.Tag) bool {
    return switch (tag) {
        .float,
        .ident,
        .int,
        .string,
        .uint,
        .pd_false,
        .pd_nil,
        .pd_true,
        .punct_rparen,
        .punct_rbrace,
        .punct_rbracket,
        => true,

        else => false,
    };
}

// Helpers
fn oneChar(self: Lexer, tag: Token.Tag) Token {
    return Token.new(tag, self.offset.?, 1);
}
fn twoChar(self: Lexer, tag: Token.Tag) Token {
    return Token.new(tag, self.offset.? - 1, 2);
}

fn reportErr(self: Lexer, err: anyerror, offset: u16) anyerror {
    const location = Location.getLocation(self.filename, self.src, offset);
    std.log.err("Lexer: {}; {}", .{ err, location });
    return err;
}

// Tests
fn testLex(allocator: std.mem.Allocator, input: []const u8) !std.MultiArrayList(Token) {
    var lexer = Lexer{ .filename = "inline", .src = input };
    return try lexer.lex(allocator);
}

fn singleTokenTests(tokens: std.MultiArrayList(Token), tag: Token.Tag, len: u16) !void {
    try std.testing.expectEqual(@as(usize, 1), tokens.items(.tag).len);
    try std.testing.expectEqual(tag, tokens.items(.tag)[0]);
    try std.testing.expectEqual(@as(u16, 0), tokens.items(.offset)[0]);
    try std.testing.expectEqual(len, tokens.items(.len)[0]);
}

test "Lex booleans" {
    const allocator = std.testing.allocator;
    var tokens = try testLex(allocator, "true; false");
    defer tokens.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 3), tokens.items(.tag).len);
    try std.testing.expectEqual(Token.Tag.pd_true, tokens.items(.tag)[0]);
    try std.testing.expectEqual(@as(usize, 0), tokens.items(.offset)[0]);
    try std.testing.expectEqual(@as(usize, 4), tokens.items(.len)[0]);
    try std.testing.expectEqual(Token.Tag.punct_semicolon, tokens.items(.tag)[1]);
    try std.testing.expectEqual(Token.Tag.pd_false, tokens.items(.tag)[2]);
    try std.testing.expectEqual(@as(usize, 6), tokens.items(.offset)[2]);
    try std.testing.expectEqual(@as(usize, 5), tokens.items(.len)[2]);
}

test "Lex ident" {
    const allocator = std.testing.allocator;
    var tokens = try testLex(allocator, "foo");
    defer tokens.deinit(allocator);
    try singleTokenTests(tokens, .ident, 3);
}

test "Lex float" {
    const allocator = std.testing.allocator;
    var tokens = try testLex(allocator, "3.14e+00");
    defer tokens.deinit(allocator);
    try singleTokenTests(tokens, .float, 8);
}

test "Lex int" {
    const allocator = std.testing.allocator;
    var tokens = try testLex(allocator, "-314");
    defer tokens.deinit(allocator);
    try singleTokenTests(tokens, .int, 4);
}

test "Lex uint" {
    const allocator = std.testing.allocator;
    var tokens = try testLex(allocator, "314");
    defer tokens.deinit(allocator);
    try singleTokenTests(tokens, .uint, 3);
}

test "Lex comment" {
    const allocator = std.testing.allocator;
    var tokens = try testLex(allocator, "# foo bar");
    defer tokens.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 0), tokens.items(.tag).len);
}

test "Lex string" {
    const allocator = std.testing.allocator;
    var tokens = try testLex(allocator,
        \\"foo { "bar" } \" baz"
    );
    errdefer tokens.deinit(allocator);
    try singleTokenTests(tokens, .string, 22);
    tokens.deinit(allocator);

    tokens = try testLex(allocator,
        \\"foo {{ "
    );
    try singleTokenTests(tokens, .string, 9);
    tokens.deinit(allocator);

    tokens = try testLex(allocator,
        \\"foo }} "
    );
    try singleTokenTests(tokens, .string, 9);
    tokens.deinit(allocator);

    tokens = try testLex(allocator,
        \\"foo } "
    );
    try singleTokenTests(tokens, .string, 8);
    tokens.deinit(allocator);

    //try std.testing.expectError(error.UnterminatedString, testLex(allocator,
    //    \\"foo { "
    //));
}

test "Lex global" {
    const allocator = std.testing.allocator;
    var tokens = try testLex(allocator, "@foo");
    defer tokens.deinit(allocator);
    try singleTokenTests(tokens, .op_global, 4);

    //try std.testing.expectError(error.InvalidGlobal, testLex(allocator,
    //    \\@+
    //));
}

test "Lex ops" {
    const allocator = std.testing.allocator;
    const tests = [_]struct {
        input: []const u8,
        tag: Token.Tag,
        len: u16,
    }{
        .{ .input = "+", .tag = .punct_plus, .len = 1 },
        .{ .input = "-", .tag = .punct_minus, .len = 1 },
        .{ .input = "*", .tag = .punct_star, .len = 1 },
        .{ .input = "/", .tag = .punct_slash, .len = 1 },
        .{ .input = "%", .tag = .punct_percent, .len = 1 },
        .{ .input = "+=", .tag = .op_add_eq, .len = 2 },
        .{ .input = "-=", .tag = .op_sub_eq, .len = 2 },
        .{ .input = "*=", .tag = .op_mul_eq, .len = 2 },
        .{ .input = "/=", .tag = .op_div_eq, .len = 2 },
        .{ .input = "%=", .tag = .op_mod_eq, .len = 2 },
        .{ .input = "<", .tag = .punct_lt, .len = 1 },
        .{ .input = "<=", .tag = .op_lte, .len = 2 },
        .{ .input = ">", .tag = .punct_gt, .len = 1 },
        .{ .input = ">=", .tag = .op_gte, .len = 2 },
        .{ .input = "=", .tag = .punct_equals, .len = 1 },
        .{ .input = "==", .tag = .op_eq, .len = 2 },
        .{ .input = "!", .tag = .punct_bang, .len = 1 },
        .{ .input = "!>", .tag = .op_redir_clobber, .len = 2 },
        .{ .input = "+>", .tag = .op_redir_append, .len = 2 },
        .{ .input = "!=", .tag = .op_neq, .len = 2 },
        .{ .input = "!~", .tag = .op_nomatch, .len = 2 },
        .{ .input = ":", .tag = .punct_colon, .len = 1 },
        .{ .input = ".", .tag = .punct_dot, .len = 1 },
        .{ .input = ":=", .tag = .op_define, .len = 2 },
        .{ .input = "~", .tag = .punct_tilde, .len = 1 },
        .{ .input = "~=", .tag = .op_xmatch, .len = 2 },
        .{ .input = "..<", .tag = .op_range_ex, .len = 3 },
        .{ .input = "..=", .tag = .op_range_in, .len = 3 },
        .{ .input = "?", .tag = .punct_question, .len = 1 },
        .{ .input = "?:", .tag = .op_elvis, .len = 2 },
        .{ .input = "?=", .tag = .op_elvis_eq, .len = 2 },
        .{ .input = "$", .tag = .punct_dollar, .len = 1 },
        .{ .input = ",", .tag = .punct_comma, .len = 1 },
        .{ .input = "|", .tag = .punct_pipe, .len = 1 },
        .{ .input = "{", .tag = .punct_lbrace, .len = 1 },
        .{ .input = "[", .tag = .punct_lbracket, .len = 1 },
        .{ .input = "(", .tag = .punct_lparen, .len = 1 },
        .{ .input = "}", .tag = .punct_rbrace, .len = 1 },
        .{ .input = "]", .tag = .punct_rbracket, .len = 1 },
        .{ .input = ")", .tag = .punct_rparen, .len = 1 },
    };

    for (tests) |t| {
        var tokens = try testLex(allocator, t.input);
        defer tokens.deinit(allocator);
        try singleTokenTests(tokens, t.tag, t.len);
    }
}

test "Lex synthetic semicolons" {
    const avoid_call =
        \\5 + 5
        \\(5)
    ;
    const allocator = std.testing.allocator;
    var tokens = try testLex(allocator, avoid_call);
    defer tokens.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 7), tokens.items(.tag).len);
    try std.testing.expectEqual(Token.Tag.uint, tokens.items(.tag)[0]);
    try std.testing.expectEqual(Token.Tag.punct_plus, tokens.items(.tag)[1]);
    try std.testing.expectEqual(Token.Tag.uint, tokens.items(.tag)[2]);
    try std.testing.expectEqual(Token.Tag.punct_semicolon, tokens.items(.tag)[3]);
    try std.testing.expectEqual(Token.Tag.punct_lparen, tokens.items(.tag)[4]);
    try std.testing.expectEqual(Token.Tag.uint, tokens.items(.tag)[5]);
    try std.testing.expectEqual(Token.Tag.punct_rparen, tokens.items(.tag)[6]);
    tokens.deinit(allocator);

    const avoid_subscript =
        \\5 + 5
        \\[5]
    ;
    tokens = try testLex(allocator, avoid_subscript);

    try std.testing.expectEqual(@as(usize, 7), tokens.items(.tag).len);
    try std.testing.expectEqual(Token.Tag.uint, tokens.items(.tag)[0]);
    try std.testing.expectEqual(Token.Tag.punct_plus, tokens.items(.tag)[1]);
    try std.testing.expectEqual(Token.Tag.uint, tokens.items(.tag)[2]);
    try std.testing.expectEqual(Token.Tag.punct_semicolon, tokens.items(.tag)[3]);
    try std.testing.expectEqual(Token.Tag.punct_lbracket, tokens.items(.tag)[4]);
    try std.testing.expectEqual(Token.Tag.uint, tokens.items(.tag)[5]);
    try std.testing.expectEqual(Token.Tag.punct_rbracket, tokens.items(.tag)[6]);
    tokens.deinit(allocator);

    const no_semi_needed =
        \\5 + 5
        \\- 5
    ;
    tokens = try testLex(allocator, no_semi_needed);

    try std.testing.expectEqual(@as(usize, 5), tokens.items(.tag).len);
    try std.testing.expectEqual(Token.Tag.uint, tokens.items(.tag)[0]);
    try std.testing.expectEqual(Token.Tag.punct_plus, tokens.items(.tag)[1]);
    try std.testing.expectEqual(Token.Tag.uint, tokens.items(.tag)[2]);
    try std.testing.expectEqual(Token.Tag.punct_minus, tokens.items(.tag)[3]);
    try std.testing.expectEqual(Token.Tag.uint, tokens.items(.tag)[4]);
}
