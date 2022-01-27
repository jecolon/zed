const std = @import("std");

pub const Tag = enum {
    a_string,
    s_string,

    punct_newline,
    punct_semicolon,
};

pub const Type = union(Tag) {
    a_string: []u8,
    s_string: []const u8,

    punct_newline,
    punct_semicolon,
};

offset: u16,
ty: Type,

const Token = @This();

pub fn init(ty: Type, offset: u16) Token {
    return .{ .ty = ty, .offset = offset };
}

pub fn is(self: Token, tag: Tag) bool {
    return self.ty == tag;
}

pub const TokenList = struct {
    allocator: std.mem.Allocator,
    tokens: []Token,

    pub fn init(allocator: std.mem.Allocator, tokens: []Token) TokenList {
        return .{ .allocator = allocator, .tokens = tokens };
    }

    pub fn deinit(self: TokenList) void {
        for (self.tokens) |token| {
            switch (token.ty) {
                .a_string => |bytes| self.allocator.free(bytes),
                else => {},
            }
        }

        self.allocator.free(self.tokens);
    }
};
