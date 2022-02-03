const std = @import("std");

pub const Tag = enum {
    float,
    ident,
    int,
    string,
    uint,

    pd_false,
    pd_nil,
    pd_true,

    op_add_eq,
    op_concat,
    op_global,
    op_neg,
    op_sub_eq,
    op_redir_append,
    op_redir_clobber,

    punct_at,
    punct_minus,
    punct_newline,
    punct_plus,
    punct_rbrace,
    punct_rbracket,
    punct_rparen,
    punct_semicolon,
};

pub const predef = std.ComptimeStringMap(Tag, .{
    .{ "false", .pd_false },
    .{ "nil", .pd_nil },
    .{ "true", .pd_true },
});

len: u16,
offset: u16,
tag: Tag,

const Token = @This();

pub fn new(tag: Tag, offset: u16, len: usize) Token {
    return Token{
        .len = @intCast(u16, len),
        .offset = offset,
        .tag = tag,
    };
}

pub fn is(self: Token, tag: Tag) bool {
    return self.tag == tag;
}
