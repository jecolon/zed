pub const Tag = enum {
    bool_false,
    bool_true,
    stmt_end,
};

token_offsets: []const u16,
tag: Tag,
