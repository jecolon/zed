const std = @import("std");

pub const Tag = enum {
    float,
    ident,
    int,
    string,
    uint,

    kw_and,
    kw_break,
    kw_continue,
    kw_else,
    kw_from,
    kw_if,
    kw_or,
    kw_return,
    kw_to,
    kw_until,
    kw_while,

    pd_false,
    pd_nil,
    pd_true,

    pd_chars,
    pd_contains,
    pd_each,
    pd_filter,
    pd_indexOf,
    pd_join,
    pd_keys,
    pd_lastIndexOf,
    pd_len,
    pd_map,
    pd_max,
    pd_mean,
    pd_median,
    pd_min,
    pd_mode,
    pd_push,
    pd_reduce,
    pd_reverse,
    pd_sort,
    pd_split,
    pd_stdev,
    pd_values,

    pd_onInit,
    pd_onFile,
    pd_onRec,
    pd_onExit,

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
    .{ "and", .kw_and },
    .{ "break", .kw_break },
    .{ "continue", .kw_continue },
    .{ "else", .kw_else },
    .{ "from", .kw_from },
    .{ "if", .kw_if },
    .{ "or", .kw_or },
    .{ "false", .pd_false },
    .{ "nil", .pd_nil },
    .{ "return", .kw_return },
    .{ "to", .kw_to },
    .{ "true", .pd_true },
    .{ "until", .kw_until },
    .{ "while", .kw_while },

    .{ "chars", .pd_chars },
    .{ "contains", .pd_contains },
    .{ "each", .pd_each },
    .{ "filter", .pd_filter },
    .{ "indexOf", .pd_indexOf },
    .{ "join", .pd_join },
    .{ "keys", .pd_keys },
    .{ "lastIndexOf", .pd_lastIndexOf },
    .{ "len", .pd_len },
    .{ "map", .pd_map },
    .{ "mean", .pd_mean },
    .{ "median", .pd_median },
    .{ "mode", .pd_mode },
    .{ "min", .pd_min },
    .{ "max", .pd_max },
    .{ "push", .pd_push },
    .{ "reduce", .pd_reduce },
    .{ "reverse", .pd_reverse },
    .{ "stdev", .pd_stdev },
    .{ "sort", .pd_sort },
    .{ "split", .pd_split },
    .{ "values", .pd_values },

    .{ "onInit", .pd_onInit },
    .{ "onFile", .pd_onFile },
    .{ "onRec", .pd_onRec },
    .{ "onExit", .pd_onExit },
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
