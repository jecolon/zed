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
    op_define,
    op_div_eq,
    op_elvis,
    op_elvis_eq,
    op_eq,
    op_global,
    op_gte,
    op_lte,
    op_mod_eq,
    op_mul_eq,
    op_neg,
    op_neq,
    op_nomatch,
    op_range_ex,
    op_range_in,
    op_sub_eq,
    op_redir_append,
    op_redir_clobber,
    op_xmatch,

    punct_at,
    punct_bang,
    punct_colon,
    punct_comma,
    punct_dollar,
    punct_dot,
    punct_equals,
    punct_fat_rarrow,
    punct_gt,
    punct_lbrace,
    punct_lbracket,
    punct_lparen,
    punct_lt,
    punct_minus,
    punct_newline,
    punct_percent,
    punct_pipe,
    punct_plus,
    punct_question,
    punct_rbrace,
    punct_rbracket,
    punct_rparen,
    punct_semicolon,
    punct_slash,
    punct_star,
    punct_tilde,
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
