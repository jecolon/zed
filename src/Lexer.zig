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

    var prev_token: ?Token = null;
    var semicolon = Token{
        .len = 1,
        .offset = 0,
        .tag = .punct_semicolon,
    };

    while (true) {
        const token_opt = self.next() catch |err| return self.reportErr(err, self.offset.?);

        if (token_opt) |token| {
            if (token.is(.punct_newline)) {
                if (prev_token) |prev| {
                    if (requiresSemi(prev.tag)) {
                        semicolon.offset = token.offset;
                        try tokens.append(allocator, semicolon);
                    }
                }
            } else {
                try tokens.append(allocator, token);
            }

            prev_token = token;
        } else break;
    }

    if (prev_token == null or !prev_token.?.is(.punct_semicolon)) try tokens.append(allocator, self.oneChar(.punct_semicolon));

    return tokens;
}

fn next(self: *Lexer) !?Token {
    if (self.skipWhitespce()) |byte| {
        if (isIdentStart(byte)) return self.lexIdent();
        if (isNumStart(byte)) return self.lexNumber(byte);

        return switch (byte) {
            '#' => try self.lexComment(),
            '\n' => self.lexNewline(),
            ';' => self.oneChar(.punct_semicolon),
            '"' => try self.lexString(),
            '@' => try self.lexGlobal(),
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
        if (!isIdentByte(peek_byte)) return self.reportErr(error.InvalidGlobal, start);
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
        if (self.peekPred(isWhitespace)) return token;

        if (self.skipByte('+')) {
            token.tag = .op_concat;
            return token;
        } else if (self.skipByte('=')) {
            token.tag = .op_add_eq;
            return token;
        } else if (self.skipByte('>')) {
            token.tag = .op_redir_append;
            return token;
        }
    }

    if ('-' == byte) {
        // Minus is special too
        var token = self.oneChar(.punct_minus);
        if (self.peekPred(isWhitespace)) return token;

        if (self.peekPred(isIdentStart)) {
            token.tag = .op_neg;
            return token;
        }

        if (self.skipByte('=')) {
            token.tag = .op_sub_eq;
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

    if (reached_eof) {
        const err = error.UnterminatedString;
        const location = Location.getLocation(self.filename, self.src, start);
        std.log.err("Lexer: {}; {}", .{ err, location });
        return err;
    }

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
    return Token.new(tag, self.offset.?, 2);
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
    try std.testing.expectEqual(@as(usize, 2), tokens.items(.tag).len);
    try std.testing.expectEqual(tag, tokens.items(.tag)[0]);
    try std.testing.expectEqual(@as(u16, 0), tokens.items(.offset)[0]);
    try std.testing.expectEqual(len, tokens.items(.len)[0]);
}

test "Lex booleans" {
    const allocator = std.testing.allocator;
    var tokens = try testLex(allocator, "true; false");
    defer tokens.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 4), tokens.items(.tag).len);
    try std.testing.expectEqual(Token.Tag.pd_true, tokens.items(.tag)[0]);
    try std.testing.expectEqual(@as(usize, 0), tokens.items(.offset)[0]);
    try std.testing.expectEqual(@as(usize, 4), tokens.items(.len)[0]);
    try std.testing.expectEqual(Token.Tag.punct_semicolon, tokens.items(.tag)[1]);
    try std.testing.expectEqual(Token.Tag.pd_false, tokens.items(.tag)[2]);
    try std.testing.expectEqual(@as(usize, 6), tokens.items(.offset)[2]);
    try std.testing.expectEqual(@as(usize, 5), tokens.items(.len)[2]);
    try std.testing.expectEqual(Token.Tag.punct_semicolon, tokens.items(.tag)[3]);
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

    try std.testing.expectEqual(@as(usize, 1), tokens.items(.tag).len);
    try std.testing.expectEqual(Token.Tag.punct_semicolon, tokens.items(.tag)[0]);
}

test "Lex string" {
    const allocator = std.testing.allocator;
    var tokens = try testLex(allocator,
        \\"foo { "bar" } \" baz"
    );
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
