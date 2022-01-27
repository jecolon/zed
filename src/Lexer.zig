const std = @import("std");

const Token = @import("Token.zig");

offset: ?u16 = null,
src: []const u8,

const Lexer = @This();

pub fn lex(self: *Lexer, allocator: std.mem.Allocator) !Token.TokenList {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var prev_tag: ?Token.Tag = null;

    while (try self.next(allocator)) |token| {
        if (token.is(.punct_newline)) {
            if (prev_tag) |prev| {
                if (requiresSemi(prev)) try tokens.append(Token.init(.punct_semicolon, token.offset));
            }
        } else {
            try tokens.append(token);
        }

        prev_tag = token.ty;
    }

    return Token.TokenList.init(allocator, tokens.toOwnedSlice());
}

pub fn next(self: *Lexer, allocator: std.mem.Allocator) !?Token {
    if (self.skipWhiteSpace()) |byte| switch (byte) {
        '"' => return try self.lexString(allocator),

        else => return error.UnknownByte,
    } else return null;
}

// Lexer movement
fn advance(self: *Lexer) ?u8 {
    if (self.offset) |*offset| {
        offset.* += 1;
        if (offset.* >= self.src.len) return null;
    } else {
        self.offset = 0;
    }

    return self.src[self.offset.?];
}

fn peek(self: Lexer) ?u8 {
    if (self.offset) |offset| {
        return if (offset + 1 < self.src.len) return self.src[offset + 1] else null;
    } else {
        return if (self.src.len > 0) self.src[0] else null;
    }
}

fn peekIs(self: Lexer, byte: u8) bool {
    return if (self.peek()) |peek_byte| peek_byte == byte else false;
}

fn skipByte(self: *Lexer, byte: u8) bool {
    if (self.peekIs(byte)) {
        _ = self.advance();
        return true;
    }

    return false;
}

fn run(self: *Lexer, pred: Predicate) void {
    while (self.peek()) |peek_byte| {
        if (!pred(peek_byte)) break;
        _ = self.advance();
    }
}

fn skipWhiteSpace(self: *Lexer) ?u8 {
    self.run(isWhiteSpace);
    return self.advance();
}

// Predicates
const Predicate = fn (u8) bool;

fn isWhiteSpace(byte: u8) bool {
    return switch (byte) {
        ' ',
        '\t',
        '\r',
        => true,

        else => false,
    };
}

fn requiresSemi(tag: Token.Tag) bool {
    return switch (tag) {
        .a_string,
        .s_string,
        => true,

        else => false,
    };
}

// Lex funcs
fn lexString(self: *Lexer, allocator: std.mem.Allocator) !Token {
    const quote_offset = self.offset.?;
    if (self.skipByte('"')) return Token.init(.{ .s_string = "" }, quote_offset);

    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    var reached_eof = true;
    var is_static = true;

    while (self.advance()) |byte| {
        if ('\\' == byte) {
            if (self.peek()) |peek_byte| switch (peek_byte) {
                'n' => {
                    _ = self.advance(); // n
                    try buf.append('\n');
                    is_static = false;
                },
                'r' => {
                    _ = self.advance(); // r
                    try buf.append('\r');
                    is_static = false;
                },
                't' => {
                    _ = self.advance(); // t
                    try buf.append('\t');
                    is_static = false;
                },
                'u' => {
                    _ = self.advance(); // u
                    const start = self.offset.? + 1; // 1 past u
                    while (self.peek()) |ubyte| {
                        switch (ubyte) {
                            '0'...'9',
                            'a'...'f',
                            'A'...'F',
                            => _ = self.advance(),
                            else => break,
                        }
                    }

                    const code = try std.fmt.parseInt(u21, self.src[start .. self.offset.? + 1], 16);
                    var ubuf: [4]u8 = undefined;
                    const len = std.unicode.utf8Encode(code, &ubuf) catch std.unicode.utf8Encode(0xFFFD, &ubuf) catch unreachable;
                    try buf.appendSlice(ubuf[0..len]);
                    is_static = false;
                },

                else => continue,
            };
        } else if ('{' == byte and self.peekIs('{')) {
            // Escaped {
            try buf.append(byte);
            try buf.append(self.advance().?);
            is_static = false;
        } else if ('{' == byte) {
            // Normal interpolation {
            try buf.append(byte);
            try self.lexInterpolation(&buf);
            is_static = false;
        } else if ('}' == byte and self.peekIs('}')) {
            // Escped }
            try buf.append(byte);
            try buf.append(self.advance().?);
            is_static = false;
        } else if ('"' == byte) {
            // Closing quotes
            reached_eof = false;
            break;
        } else {
            // Any other character.
            try buf.append(byte);
        }
    }

    if (reached_eof) return error.UnterminatedInterpolation;

    return if (is_static)
        Token.init(.{ .s_string = self.src[quote_offset + 1 .. self.offset.?] }, quote_offset)
    else
        Token.init(.{ .a_string = buf.toOwnedSlice() }, quote_offset);
}

fn lexInterpolation(self: *Lexer, buf_ptr: *std.ArrayList(u8)) anyerror!void {
    while (self.advance()) |byte| {
        try buf_ptr.append(byte);

        if ('{' == byte and self.peekIs('{')) {
            // Escaped {
            try buf_ptr.append(self.advance().?);
            continue;
        }

        if ('{' == byte) {
            // Normal interpolation {
            try self.lexInterpolation(buf_ptr);
            continue;
        }

        if ('}' == byte and self.peekIs('}')) {
            // Escaped }
            try buf_ptr.append(self.advance().?);
            continue;
        }

        if ('}' == byte) return;
    }

    return error.UnterminatedInterpolation;
}

test "Lexer static string" {
    const input =
        \\"Hello World!"
    ;
    var allocator = std.testing.allocator;
    var lexer = Lexer{ .src = input };
    const token_list = try lexer.lex(allocator);
    defer token_list.deinit();

    try std.testing.expectEqualStrings("Hello World!", token_list.tokens[0].ty.s_string);
}

test "Lexer alloc string" {
    const input =
        \\"Hello { "wat" } World!"
    ;
    var allocator = std.testing.allocator;
    var lexer = Lexer{ .src = input };
    const token_list = try lexer.lex(allocator);
    defer token_list.deinit();

    try std.testing.expectEqualStrings("Hello { \"wat\" } World!", token_list.tokens[0].ty.a_string);
}
