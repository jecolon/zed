const std = @import("std");

const Context = @import("Context.zig");
const Lexer = @import("Lexer.zig");
const Location = @import("Location.zig");
const Node = @import("Node.zig");
const Token = @import("Token.zig");

allocator: std.mem.Allocator,
can_break: bool = false, // Parsing a loop?
ctx: Context,
range_id: u8 = 0,
token_index: ?u16 = null,
tokens: std.MultiArrayList(Token),

const Parser = @This();

pub const Program = struct {
    inits: []Node,
    files: []Node,
    recs: []Node,
    rules: []Node,
    exits: []Node,
};

pub fn parse(self: *Parser) !Program {
    var inits = std.ArrayList(Node).init(self.allocator);
    var files = std.ArrayList(Node).init(self.allocator);
    var recs = std.ArrayList(Node).init(self.allocator);
    var rules = std.ArrayList(Node).init(self.allocator);
    var exits = std.ArrayList(Node).init(self.allocator);

    while (try self.next()) |node| {
        if (node.ty == .event) {
            switch (node.ty.event.ty) {
                .init => for (node.ty.event.nodes) |n| {
                    try inits.append(n);
                    try inits.append(Node.new(.stmt_end, 0)); // Stack clean up.
                },
                .file => for (node.ty.event.nodes) |n| {
                    try files.append(n);
                    try files.append(Node.new(.stmt_end, 0)); // Stack clean up.
                },
                .rec => for (node.ty.event.nodes) |n| {
                    try recs.append(n);
                    try recs.append(Node.new(.stmt_end, 0)); // Stack clean up.
                },
                .exit => for (node.ty.event.nodes) |n| {
                    try exits.append(n);
                    try exits.append(Node.new(.stmt_end, 0)); // Stack clean up.
                },
            }
        } else {
            try rules.append(node);
            try rules.append(Node.new(.stmt_end, 0)); // Stack clean up.
        }
    }

    return Program{
        .inits = inits.items,
        .files = files.items,
        .recs = recs.items,
        .rules = rules.items,
        .exits = exits.items,
    };
}

fn next(self: *Parser) anyerror!?Node {
    return if (self.advance()) switch (self.currentTag()) {
        .punct_semicolon => try self.next(),
        else => try self.parseExpression(.lowest),
    } else null;
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
            .op_match,
            .op_matcher,
            .op_nomatch,
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
            .op_repeat,
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

    fn isRightAssociative(tag: Token.Tag) bool {
        return switch (tag) {
            .op_add_eq,
            .op_sub_eq,
            .op_mul_eq,
            .op_div_eq,
            .op_mod_eq,
            .punct_equals,
            .op_define,
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
        .float => Parser.parseFloat,
        .ident => Parser.parseIdent,
        .int => Parser.parseInt,
        .raw_str => Parser.parseRawStr,
        .string => Parser.parseString,
        .uint => Parser.parseUint,

        .kw_break => Parser.parseBreak,
        .kw_continue => Parser.parseContinue,
        .kw_do => Parser.parseDoWhile,
        .kw_if => Parser.parseIf,
        .kw_return => Parser.parseReturn,
        .kw_select => Parser.parseRecRange,
        .kw_while => Parser.parseWhile,

        .at_cols,
        .at_file,
        .at_frnum,
        .at_head,
        .at_headers,
        .at_ics,
        .at_irs,
        .at_ocs,
        .at_ors,
        .at_rec,
        .at_rnum,
        => Parser.parseGlobal,

        .op_neg, .punct_bang => Parser.parsePrefix,

        .pd_false, .pd_true => Parser.parseBoolean,
        .pd_nil => Parser.parseNil,

        .pd_atan2,
        .pd_capture,
        .pd_chars,
        .pd_col,
        .pd_contains,
        .pd_cos,
        .pd_each,
        .pd_endsWith,
        .pd_exp,
        .pd_filter,
        .pd_int,
        .pd_indexOf,
        .pd_join,
        .pd_keys,
        .pd_keysByValueAsc,
        .pd_keysByValueDesc,
        .pd_lastIndexOf,
        .pd_len,
        .pd_log,
        .pd_map,
        .pd_max,
        .pd_mean,
        .pd_median,
        .pd_memo,
        .pd_min,
        .pd_mode,
        .pd_next,
        .pd_print,
        .pd_pop,
        .pd_push,
        .pd_rand,
        .pd_reduce,
        .pd_replace,
        .pd_reverse,
        .pd_sin,
        .pd_sortAsc,
        .pd_sortDesc,
        .pd_split,
        .pd_sqrt,
        .pd_startsWith,
        .pd_stdev,
        .pd_toLower,
        .pd_toUpper,
        .pd_unique,
        .pd_values,
        => Parser.parseBuiltin,

        .pd_onInit,
        .pd_onFile,
        .pd_onRec,
        .pd_onExit,
        => Parser.parseEvent,

        .punct_lbrace => Parser.parseFunc,
        .punct_lbracket => Parser.parseList,
        .punct_lparen => Parser.parseGrouped,

        else => null,
    };
}

fn infixFn(self: Parser) InfixFn {
    return switch (self.currentTag()) {
        .punct_plus,
        .punct_minus,
        .punct_star,
        .punct_slash,
        .punct_percent,
        .punct_lt,
        .op_lte,
        .punct_gt,
        .op_gte,
        .op_eq,
        .op_neq,
        .kw_and,
        .kw_or,
        .op_concat,
        .op_repeat,
        .op_match,
        .op_matcher,
        .op_nomatch,
        => Parser.parseInfix,

        .punct_equals,
        .op_add_eq,
        .op_sub_eq,
        .op_mul_eq,
        .op_div_eq,
        .op_mod_eq,
        => Parser.parseAssign,

        .op_define => Parser.parseDefine,
        .op_elvis => Parser.parseElvis,
        .op_elvis_eq => Parser.parseElvisAssign,
        .op_range_ex, .op_range_in => Parser.parseRange,
        .op_redir_append, .op_redir_clobber => Parser.parseRedir,

        .punct_dot => Parser.parseBuiltinMethod,
        .punct_lbracket => Parser.parseSubscript,
        .punct_lparen => Parser.parseCall,
        .punct_question => Parser.parseTernary,

        else => unreachable,
    };
}

fn parseExpression(self: *Parser, precedence: Precedence) anyerror!Node {
    const tag = self.currentTag();

    const pfn = prefixFn(tag) orelse return self.ctx.err(
        "No parse function for {}.",
        .{tag},
        error.NoParseFn,
        self.currentOffset(),
    );

    var left = try pfn(self);

    while (self.peekPrecedence()) |peek_prec| {
        if (!precedence.lessThan(peek_prec)) break;
        _ = self.advance();
        const ifn = self.infixFn();
        left = try ifn(self, left);
    }

    return left;
}

// Parse functions.
fn parseAssign(self: *Parser, lvalue: Node) anyerror!Node {
    if (lvalue.ty != .ident and
        lvalue.ty != .subscript and
        lvalue.ty != .global) return self.ctx.err(
        "{s} = ?",
        .{@tagName(lvalue.ty)},
        error.InvalidAssignment,
        lvalue.offset,
    );

    const combo: Node.Combo = switch (self.currentTag()) {
        .punct_equals => .none,
        .op_add_eq => .add,
        .op_sub_eq => .sub,
        .op_mul_eq => .mul,
        .op_div_eq => .div,
        .op_mod_eq => .mod,
        else => unreachable,
    };

    var node = Node.new(
        .{ .assign = .{
            .combo = combo,
            .lvalue = try self.allocator.create(Node),
            .rvalue = try self.allocator.create(Node),
        } },
        self.currentOffset(),
    );
    node.ty.assign.lvalue.* = lvalue;
    try self.expectNext();
    node.ty.assign.rvalue.* = try self.parseExpression(.lowest);
    return node;
}

fn parseBoolean(self: *Parser) anyerror!Node {
    return Node.new(.{ .boolean = self.currentIs(.pd_true) }, self.currentOffset());
}

fn parseBreak(self: *Parser) anyerror!Node {
    if (!self.can_break) return self.ctx.err(
        "Invalid break.",
        .{},
        error.InvalidBreak,
        self.currentOffset(),
    );
    return Node.new(.loop_break, self.currentOffset());
}

fn parseBuiltin(self: *Parser) anyerror!Node {
    return Node.new(.{ .builtin = self.currentTag() }, self.currentOffset());
}

fn isBuiltinMethod(tag: Token.Tag) bool {
    return switch (tag) {
        .pd_capture,
        .pd_chars,
        .pd_contains,
        .pd_each,
        .pd_endsWith,
        .pd_filter,
        .pd_indexOf,
        .pd_join,
        .pd_keys,
        .pd_keysByValueAsc,
        .pd_keysByValueDesc,
        .pd_lastIndexOf,
        .pd_len,
        .pd_map,
        .pd_max,
        .pd_mean,
        .pd_median,
        .pd_min,
        .pd_mode,
        .pd_next,
        .pd_pop,
        .pd_push,
        .pd_reduce,
        .pd_replace,
        .pd_reverse,
        .pd_sortAsc,
        .pd_sortDesc,
        .pd_split,
        .pd_startsWith,
        .pd_stdev,
        .pd_toLower,
        .pd_toUpper,
        .pd_unique,
        .pd_values,
        => true,

        else => false,
    };
}

fn parseBuiltinMethod(self: *Parser, object: Node) anyerror!Node {
    try self.expectNext();
    if (!isBuiltinMethod(self.currentTag())) return self.ctx.err(
        "{s} is not a builtin method.",
        .{@tagName(self.currentTag())},
        error.InvalidBuiltinMethod,
        self.currentOffset(),
    );
    const builtin = try self.parseBuiltin();
    try self.expectTag(.punct_lparen);
    var call = try self.parseCall(builtin);
    var new_args = try self.allocator.alloc(Node, call.ty.call.args.len + 1);
    new_args[0] = object;
    std.mem.copy(Node, new_args[1..], call.ty.call.args);
    call.ty.call.args = new_args;
    return call;
}

fn parseCall(self: *Parser, callee: Node) anyerror!Node {
    var node = Node.new(
        .{ .call = .{ .args = &[_]Node{}, .callee = try self.allocator.create(Node) } },
        self.currentOffset(),
    );

    node.ty.call.callee.* = callee;
    var args_list = std.ArrayList(Node).init(self.allocator);

    while (!self.skipTag(.punct_rparen)) {
        try self.expectNext();
        const arg_node = try self.parseExpression(.lowest);
        try args_list.append(arg_node);
        _ = self.skipTag(.punct_comma);
    }

    // Check for predicate.
    if (self.skipTag(.punct_lbrace)) {
        const pred_node = try self.parseFunc();
        try args_list.append(pred_node);
    }

    node.ty.call.args = args_list.items;

    return node;
}

fn parseContinue(self: *Parser) anyerror!Node {
    if (!self.can_break) return self.ctx.err(
        "Invalid continue.",
        .{},
        error.InvalidContinue,
        self.currentOffset(),
    );
    return Node.new(.loop_continue, self.currentOffset());
}

fn parseDefine(self: *Parser, name: Node) anyerror!Node {
    if (name.ty != .ident) return self.ctx.err(
        "{s} := ?",
        .{@tagName(name.ty)},
        error.InvalidDefine,
        name.offset,
    );

    var node = Node.new(
        .{ .define = .{ .lvalue = try self.allocator.create(Node), .rvalue = try self.allocator.create(Node) } },
        self.currentOffset(),
    );
    node.ty.define.lvalue.* = name;
    try self.expectNext();
    node.ty.define.rvalue.* = try self.parseExpression(.lowest);

    // Self-references
    if (node.ty.define.rvalue.ty == .func) node.ty.define.rvalue.ty.func.name = name.ty.ident;

    return node;
}

fn parseElvis(self: *Parser, condition: Node) anyerror!Node {
    const offset = self.currentOffset();
    // Condition
    var conditon_ptr = try self.allocator.create(Node);
    conditon_ptr.* = condition;
    // Then branch
    var then_slice = try self.allocator.alloc(Node, 1);
    then_slice[0] = condition;
    // Else branch
    var else_slice = try self.allocator.alloc(Node, 1);
    try self.expectNext();
    else_slice[0] = try self.parseExpression(.ternary);

    return Node.new(.{ .conditional = .{
        .condition = conditon_ptr,
        .then_branch = then_slice,
        .else_branch = else_slice,
    } }, offset);
}

fn parseElvisAssign(self: *Parser, condition: Node) anyerror!Node {
    const offset = self.currentOffset();
    // Condition
    var conditon_ptr = try self.allocator.create(Node);
    conditon_ptr.* = condition;
    // Then branch
    var then_slice = try self.allocator.alloc(Node, 1);
    then_slice[0] = condition;
    // Else branch
    const assign_node = Node.new(.{ .assign = .{
        .lvalue = conditon_ptr,
        .rvalue = try self.allocator.create(Node),
    } }, offset);
    try self.expectNext();
    assign_node.ty.assign.rvalue.* = try self.parseExpression(.ternary);
    var else_slice = try self.allocator.alloc(Node, 1);
    else_slice[0] = assign_node;

    return Node.new(.{ .conditional = .{
        .condition = conditon_ptr,
        .then_branch = then_slice,
        .else_branch = else_slice,
    } }, offset);
}

fn parseEvent(self: *Parser) anyerror!Node {
    var node = Node.new(.{ .event = .{
        .nodes = undefined,
        .ty = undefined,
    } }, self.currentOffset());

    node.ty.event.ty = switch (self.currentTag()) {
        .pd_onInit => .init,
        .pd_onFile => .file,
        .pd_onRec => .rec,
        .pd_onExit => .exit,
        else => unreachable,
    };

    try self.expectTag(.punct_lbrace);
    var list = std.ArrayList(Node).init(self.allocator);

    while (!self.skipTag(.punct_rbrace)) {
        try self.expectNext();
        const next_node = try self.parseExpression(.lowest);
        try list.append(next_node);
        _ = self.skipTag(.punct_semicolon);
    }

    node.ty.event.nodes = list.items;
    return node;
}

fn parseFloat(self: *Parser) anyerror!Node {
    const start = self.currentOffset();
    const end = self.currentOffset() + self.currentLen();
    const float = try std.fmt.parseFloat(f64, self.ctx.src[start..end]);

    return Node.new(.{ .float = float }, self.currentOffset());
}

fn parseFunc(self: *Parser) anyerror!Node {
    var node = Node.new(
        .{ .func = .{ .body = undefined, .params = &[_][]const u8{} } },
        self.currentOffset(),
    );

    if (self.peekIs(.ident) and (self.peekNIs(2, .punct_comma) or self.peekNIs(2, .punct_fat_rarrow))) {
        var params_list = std.ArrayList([]const u8).init(self.allocator);
        while (!self.skipTag(.punct_fat_rarrow)) {
            try self.expectNext();
            if (!self.currentIs(.ident)) return self.ctx.err(
                "Non-identifier function param.",
                .{},
                error.InvalidParam,
                self.currentOffset(),
            );

            const param_node = try self.parseIdent();
            try params_list.append(param_node.ty.ident);
            _ = self.skipTag(.punct_comma);
        }

        node.ty.func.params = params_list.items;
    } else {
        _ = self.skipTag(.punct_fat_rarrow); // Optional =>
    }

    var body_list = std.ArrayList(Node).init(self.allocator);
    try self.parseNodes(&body_list, .punct_rbrace, .punct_semicolon);

    // Implicit return
    if (body_list.items.len == 0 or body_list.items[body_list.items.len - 1].ty != .func_return) {
        var synth_return = Node.new(
            .{ .func_return = try self.allocator.create(Node) },
            self.currentOffset(),
        );
        synth_return.ty.func_return.* = if (body_list.items.len == 0) Node.new(.nil, self.currentOffset()) else body_list.pop();
        try body_list.append(synth_return);
    }

    node.ty.func.body = body_list.items;
    return node;
}

fn parseGlobal(self: *Parser) anyerror!Node {
    return Node.new(.{ .global = self.currentTag() }, self.currentOffset());
}

fn parseGrouped(self: *Parser) anyerror!Node {
    try self.expectNext();
    const node = try self.parseExpression(.lowest);
    try self.expectTag(.punct_rparen);
    return node;
}

fn parseIdent(self: *Parser) anyerror!Node {
    const start = self.currentOffset();
    const end = self.currentOffset() + self.currentLen();
    return Node.new(.{ .ident = self.ctx.src[start..end] }, start);
}

fn parseIf(self: *Parser) anyerror!Node {
    try self.expectTag(.punct_lparen);
    try self.expectNext();
    const condition_ptr = try self.allocator.create(Node);
    condition_ptr.* = try self.parseExpression(.lowest);
    try self.expectTag(.punct_rparen);

    var then_branch_list = std.ArrayList(Node).init(self.allocator);

    if (self.skipTag(.punct_lbrace)) {
        try self.parseNodes(&then_branch_list, .punct_rbrace, .punct_semicolon);
    } else {
        try self.expectNext();
        const then_node = try self.parseExpression(.lowest);
        try then_branch_list.append(then_node);
    }

    var else_branch_list = std.ArrayList(Node).init(self.allocator);

    if (self.skipTag(.kw_else)) {
        if (self.skipTag(.punct_lbrace)) {
            try self.parseNodes(&else_branch_list, .punct_rbrace, .punct_semicolon);
        } else {
            try self.expectNext();
            const else_node = try self.parseExpression(.lowest);
            try else_branch_list.append(else_node);
        }
    } else {
        // Synthetic nil if no else branch provided.
        try else_branch_list.append(Node.new(.nil, self.currentOffset()));
    }

    return Node.new(.{ .conditional = .{
        .condition = condition_ptr,
        .then_branch = then_branch_list.items,
        .else_branch = else_branch_list.items,
    } }, self.currentOffset());
}

fn parseInfix(self: *Parser, left: Node) anyerror!Node {
    const left_ptr = try self.allocator.create(Node);
    left_ptr.* = left;

    var node = Node.new(
        .{ .infix = .{
            .left = left_ptr,
            .op = self.currentTag(),
            .right = undefined,
        } },
        self.currentOffset(),
    );

    node.ty.infix.right = try self.allocator.create(Node);
    var precedence = Precedence.forTag(node.ty.infix.op);

    try self.expectNext();
    if (Precedence.isRightAssociative(node.ty.infix.op)) precedence.demote();
    node.ty.infix.right.* = try self.parseExpression(precedence);

    return node;
}

fn parseInt(self: *Parser) anyerror!Node {
    const start = self.currentOffset();
    const end = self.currentOffset() + self.currentLen();
    const int = try std.fmt.parseInt(i32, self.ctx.src[start..end], 0);

    return Node.new(.{ .int = int }, self.currentOffset());
}

fn parseList(self: *Parser) anyerror!Node {
    var node = Node.new(.{ .list = &[_]Node{} }, self.currentOffset());
    if (self.skipTag(.punct_rbracket)) return node; // Empty list.

    if (self.skipTag(.punct_colon)) {
        // Empty map.
        try self.expectTag(.punct_rbracket);
        node.ty = .{ .map = &[_]Node.Entry{} };
        return node;
    }

    try self.expectNext();
    var next_node = try self.parseExpression(.lowest);
    if (self.skipTag(.punct_colon)) return self.parseMap(node, next_node);

    var node_list = std.ArrayList(Node).init(self.allocator);
    try node_list.append(next_node);
    _ = self.skipTag(.punct_comma);

    while (!self.skipTag(.punct_rbracket)) {
        try self.expectNext();
        next_node = try self.parseExpression(.lowest);
        try node_list.append(next_node);
        _ = self.skipTag(.punct_comma);
    }
    node.ty.list = node_list.items;

    return node;
}

fn parseMap(self: *Parser, info_node: Node, first_key: Node) anyerror!Node {
    // Have to complete the first entry.
    try self.expectNext();
    var value_node = try self.parseExpression(.lowest);
    var entry = Node.Entry{ .key = first_key, .value = value_node };
    var entry_list = std.ArrayList(Node.Entry).init(self.allocator);
    try entry_list.append(entry);
    _ = self.skipTag(.punct_comma);

    while (!self.skipTag(.punct_rbracket)) {
        // Key
        try self.expectNext();
        const key_node = try self.parseExpression(.lowest);
        try self.expectTag(.punct_colon);
        // Value
        try self.expectNext();
        value_node = try self.parseExpression(.lowest);
        // Entry
        entry = Node.Entry{ .key = key_node, .value = value_node };
        try entry_list.append(entry);
        _ = self.skipTag(.punct_comma);
    }

    return Node.new(.{ .map = entry_list.items }, info_node.offset);
}

fn parseNil(self: *Parser) anyerror!Node {
    return Node.new(.nil, self.currentOffset());
}

fn parsePrefix(self: *Parser) anyerror!Node {
    var node = Node.new(.{ .prefix = .{ .op = self.currentTag(), .operand = undefined } }, self.currentOffset());
    try self.expectNext();
    node.ty.prefix.operand = try self.allocator.create(Node);
    node.ty.prefix.operand.* = try self.parseExpression(.prefix);
    return node;
}

fn parseRange(self: *Parser, from: Node) anyerror!Node {
    var from_ptr = try self.allocator.create(Node);
    from_ptr.* = from;

    var node = Node.new(
        .{ .range = .{
            .inclusive = self.currentIs(.op_range_in),
            .from = from_ptr,
            .to = undefined,
        } },
        self.currentOffset(),
    );

    try self.expectNext();
    node.ty.range.to = try self.allocator.create(Node);
    node.ty.range.to.* = try self.parseExpression(.range);

    return node;
}

fn parseRecRange(self: *Parser) anyerror!Node {
    // Ranges must be unique.
    self.range_id += 1;

    var node = Node.new(.{ .rec_range = .{
        .from = null,
        .to = null,
        .action = &[0]Node{},
        .id = self.range_id,
        .exclusive = false,
    } }, self.currentOffset());

    try self.expectNext();

    if (self.currentIs(.punct_lparen)) {
        // From
        try self.expectNext();
        node.ty.rec_range.from = try self.allocator.create(Node);
        node.ty.rec_range.from.?.* = try self.parseExpression(.lowest);
        try self.expectTag(.punct_rparen);

        // No range end expression with default action.
        if (self.atEnd() or self.peekIs(.punct_semicolon)) return node;

        _ = self.advance();
    }

    // Range with end expression.
    if (self.currentIs(.op_range_ex) or self.currentIs(.op_range_in)) {
        node.ty.rec_range.exclusive = self.currentIs(.op_range_ex);
        try self.expectTag(.punct_lparen);
        try self.expectNext();
        node.ty.rec_range.to = try self.allocator.create(Node);
        node.ty.rec_range.to.?.* = try self.parseExpression(.lowest);
        try self.expectTag(.punct_rparen);

        // Default action.
        if (self.atEnd() or self.peekIs(.punct_semicolon)) return node;

        _ = self.advance();
    }

    if (self.currentIs(.punct_lbrace)) {
        var list = std.ArrayList(Node).init(self.allocator);
        try self.parseNodes(&list, .punct_rbrace, .punct_semicolon);
        node.ty.rec_range.action = list.items;
    } else {
        node.ty.rec_range.action = try self.allocator.alloc(Node, 1);
        node.ty.rec_range.action[0] = try self.parseExpression(.lowest);
    }

    return node;
}

fn parseRedir(self: *Parser, expr: Node) anyerror!Node {
    const expr_ptr = try self.allocator.create(Node);
    expr_ptr.* = expr;

    const current_tag = self.currentTag();
    var node = Node.new(
        .{ .redir = .{
            .expr = expr_ptr,
            .file = undefined,
            .clobber = current_tag == .op_redir_clobber,
        } },
        self.currentOffset(),
    );

    var precedence = Precedence.forTag(current_tag);
    if (Precedence.isRightAssociative(current_tag)) precedence.demote();

    try self.expectNext();
    node.ty.redir.file = try self.allocator.create(Node);
    node.ty.redir.file.* = try self.parseExpression(precedence);

    return node;
}

fn parseReturn(self: *Parser) anyerror!Node {
    var node = Node.new(.{ .func_return = try self.allocator.create(Node) }, self.currentOffset());

    if (!self.peekIs(.punct_semicolon) and
        !self.peekIs(.punct_rbrace) and
        !self.atEnd())
    {
        _ = self.advance();
        node.ty.func_return.* = try self.parseExpression(.lowest);
    } else {
        node.ty.func_return.* = Node.new(.nil, self.currentOffset());
    }

    return node;
}

fn parseSubscript(self: *Parser, container: Node) anyerror!Node {
    var node = Node.new(.{ .subscript = .{
        .container = try self.allocator.create(Node),
        .index = undefined,
    } }, self.currentOffset());
    node.ty.subscript.container.* = container;
    try self.expectNext();
    node.ty.subscript.index = try self.allocator.create(Node);
    node.ty.subscript.index.* = try self.parseExpression(.lowest);
    _ = try self.expectTag(.punct_rbracket);
    return node;
}

// Strings
fn processEscapes(self: *Parser, str: []const u8) anyerror![]const u8 {
    if (!std.mem.containsAtLeast(u8, str, 1, "\\")) return str;
    var copy = std.ArrayList(u8).init(self.allocator);
    var i: usize = 0;

    while (i < str.len) : (i += 1) {
        const byte = str[i];

        if ('\\' == byte) {
            if (i + 1 < str.len) {
                const peek_byte = str[i + 1];

                switch (peek_byte) {
                    '"' => {
                        i += 1;
                        try copy.append('"');
                    },
                    'n' => {
                        i += 1;
                        try copy.append('\n');
                    },
                    'r' => {
                        i += 1;
                        try copy.append('\r');
                    },
                    't' => {
                        i += 1;
                        try copy.append('\t');
                    },
                    'u' => {
                        i += 1;
                        const ustart = i + 1; // 1 past u
                        var j: usize = ustart;
                        while (j < str.len) : (j += 1) {
                            const ubyte = str[j];

                            switch (ubyte) {
                                '0'...'9',
                                'a'...'f',
                                'A'...'F',
                                => i += 1,
                                else => break,
                            }
                        }

                        const code = try std.fmt.parseInt(u21, str[ustart..j], 16);

                        var ubuf: [4]u8 = undefined;
                        if (std.unicode.utf8Encode(code, &ubuf)) |ulen| {
                            try copy.appendSlice(ubuf[0..ulen]);
                        } else |_| {
                            const ulen = std.unicode.utf8Encode(0xFFFD, &ubuf) catch unreachable;
                            try copy.appendSlice(ubuf[0..ulen]);
                        }
                    },

                    else => continue,
                }
            }
        } else try copy.append(byte);
    }

    return copy.items;
}

fn parseIpol(self: *Parser, src: []const u8, offset: u16) anyerror!Node.Ipol {
    var parsable = src;
    var format: ?[]const u8 = null;
    var end: usize = 1;

    if ('#' == src[0]) {
        var reached_eof = true;

        while (end < src.len) : (end += 1) {
            if ('#' == src[end]) {
                reached_eof = false;
                break;
            }
        }

        if (reached_eof) return self.ctx.err(
            "Unterminated format spec.",
            .{},
            error.InvalidFormat,
            offset,
        );

        format = src[1..end];
        parsable = src[end + 1 ..];
    }

    // Need this for proper offsets in error messages.
    const spec_len = if (format) |f| @intCast(u16, f.len) + 4 else 1;

    // NOTE: Made this mistake more than once; using the same input as the main parser here will cause an infinite loop,
    // subsequent stack overfflow, and NO error message, which will drive you crazy!
    var sub_lexer = Lexer{ .allocator = self.allocator, .ctx = Context{ .filename = self.ctx.filename, .src = parsable } };
    const sub_tokens = sub_lexer.lex() catch |err| return self.ctx.err(
        "Error lexing sting interpolation `{s}`.",
        .{parsable},
        err,
        offset + spec_len,
    );
    var sub_parser = Parser{
        .allocator = self.allocator,
        .ctx = Context{ .filename = self.ctx.filename, .src = parsable },
        .tokens = sub_tokens,
    };
    const sub_program = sub_parser.parse() catch |err| return self.ctx.err(
        "Error parsing sting interpolation `{s}`.",
        .{parsable},
        err,
        offset + spec_len,
    );
    const sub_nodes = sub_program.rules;

    // Avoid format of stmt_end bug.
    end = sub_nodes.len;
    if (sub_nodes[end - 1].ty == .stmt_end) end = end - 1;

    // Adjust offsets for error messages.
    for (sub_nodes) |*n| {
        if (spec_len != 1) {
            n.offset = offset + spec_len + n.offset - 1;
        } else {
            n.offset = offset + spec_len + n.offset;
        }
    }

    return Node.Ipol{
        .spec = format,
        .nodes = sub_nodes[0..end],
        .offset = offset,
    };
}

fn parseString(self: *Parser) anyerror!Node {
    const offset = self.currentOffset();
    const src = try self.processEscapes(self.ctx.src[self.currentOffset() + 1 .. self.currentOffset() + self.currentLen() - 1]);

    const src_len = src.len;
    var start: usize = 0;
    var i: usize = 0;

    var segments = std.ArrayList(Node.Segment).init(self.allocator);

    while (i < src_len) {
        const byte = src[i];

        if ('{' == byte and i + 1 < src_len and '{' == src[i + 1]) {
            i += 2;
            continue;
        }
        if ('}' == byte and i + 1 < src_len and '}' == src[i + 1]) {
            i += 2;
            continue;
        }

        if ('{' == byte) {
            if (i > start) {
                const str_segment = Node.Segment{ .plain = src[start..i] };
                try segments.append(str_segment);
            }

            start = i + 1;
            var end: usize = i + 1;
            var nest_level: usize = 0;
            var reached_eof = false;

            while (end < src_len) {
                const ibyte = src[end];

                if ('{' == ibyte and end + 1 < src_len and '{' == src[end + 1]) {
                    end += 2;
                    continue;
                }
                if ('}' == ibyte and end + 1 < src_len and '}' == src[end + 1]) {
                    end += 2;
                    continue;
                }

                if ('{' == ibyte) nest_level += 1;

                if ('}' == ibyte) {
                    if (nest_level > 0) {
                        nest_level -= 1;
                    } else {
                        reached_eof = false;
                        break;
                    }
                }

                end += 1;
            }

            if (reached_eof) return self.ctx.err(
                "Unterminated string interpolation.",
                .{},
                error.InvalidIpol,
                offset,
            );
            const ipol_segment = Node.Segment{ .ipol = try self.parseIpol(src[start..end], offset + @intCast(u16, start)) };
            try segments.append(ipol_segment);

            i = end + 1;
            start = i;
            continue;
        }

        i += 1;
    }

    if (start < src_len) {
        const str_segment = Node.Segment{ .plain = src[start..src_len] };
        try segments.append(str_segment);
    }

    return Node.new(.{ .string = segments.items }, offset);
}

fn parseRawStr(self: *Parser) anyerror!Node {
    const start = self.currentOffset();
    const end = self.currentOffset() + self.currentLen();
    return Node.new(.{ .raw_str = self.ctx.src[start + 1 .. end - 1] }, start);
}

// End Strings

fn parseTernary(self: *Parser, condition: Node) anyerror!Node {
    const offset = self.currentOffset();
    // Condition
    var conditon_ptr = try self.allocator.create(Node);
    conditon_ptr.* = condition;
    // Then branch
    try self.expectNext();
    var then_slice = try self.allocator.alloc(Node, 1);
    then_slice[0] = try self.parseExpression(.ternary);
    // Else branch
    try self.expectTag(.punct_colon);
    var else_slice = try self.allocator.alloc(Node, 1);
    try self.expectNext();
    else_slice[0] = try self.parseExpression(.ternary);

    return Node.new(.{ .conditional = .{
        .condition = conditon_ptr,
        .then_branch = then_slice,
        .else_branch = else_slice,
    } }, offset);
}

fn parseUint(self: *Parser) anyerror!Node {
    const start = self.currentOffset();
    const end = self.currentOffset() + self.currentLen();
    const uint = try std.fmt.parseUnsigned(u32, self.ctx.src[start..end], 0);

    return Node.new(.{ .uint = uint }, self.currentOffset());
}

fn parseDoWhile(self: *Parser) anyerror!Node {
    self.can_break = true;
    defer self.can_break = false;

    var node = Node.new(.{ .loop = .{
        .condition = undefined,
        .body = undefined,
        .is_do = true,
    } }, self.currentOffset());

    // Body
    var body_list = std.ArrayList(Node).init(self.allocator);

    if (self.skipTag(.punct_lbrace)) {
        try self.parseNodes(&body_list, .punct_rbrace, .punct_semicolon);
    } else {
        try self.expectNext();
        const body_node = try self.parseExpression(.lowest);
        try body_list.append(body_node);
    }

    // while loops need to clean up the stack after every iteration.
    try body_list.append(Node.new(.stmt_end, 0));
    node.ty.loop.body = body_list.items;

    try self.expectTag(.kw_while);

    // Condition
    try self.expectTag(.punct_lparen);
    try self.expectNext();
    node.ty.loop.condition = try self.allocator.create(Node);
    node.ty.loop.condition.* = try self.parseExpression(.lowest);
    try self.expectTag(.punct_rparen);

    return node;
}

fn parseWhile(self: *Parser) anyerror!Node {
    self.can_break = true;
    defer self.can_break = false;

    var node = Node.new(.{ .loop = .{
        .condition = undefined,
        .body = undefined,
        .is_do = false,
    } }, self.currentOffset());

    // Condition
    try self.expectTag(.punct_lparen);
    try self.expectNext();
    node.ty.loop.condition = try self.allocator.create(Node);
    node.ty.loop.condition.* = try self.parseExpression(.lowest);
    try self.expectTag(.punct_rparen);
    // Body
    var body_list = std.ArrayList(Node).init(self.allocator);

    if (self.skipTag(.punct_lbrace)) {
        try self.parseNodes(&body_list, .punct_rbrace, .punct_semicolon);
    } else {
        try self.expectNext();
        const body_node = try self.parseExpression(.lowest);
        try body_list.append(body_node);
    }

    // while loops need to clean up the stack after every iteration.
    try body_list.append(Node.new(.stmt_end, 0));

    node.ty.loop.body = body_list.items;

    return node;
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
    if (!self.advance()) return self.ctx.err(
        "Unexpected end of tokens.",
        .{},
        error.ExpectNext,
        @intCast(u16, self.ctx.src.len - 1),
    );
}

fn expectTag(self: *Parser, tag: Token.Tag) !void {
    try self.expectNext();
    if (!self.currentIs(tag)) return self.ctx.err(
        "Expected {s}, got {s}.",
        .{ @tagName(tag), @tagName(self.currentTag()) },
        error.expectTag,
        self.currentOffset(),
    );
}

// Helpers
fn atEnd(self: Parser) bool {
    return self.token_index.? + 1 >= self.tokens.items(.tag).len;
}

fn parseNodes(self: *Parser, list: *std.ArrayList(Node), stop: Token.Tag, skip: Token.Tag) anyerror!void {
    while (!self.skipTag(stop)) {
        try self.expectNext();
        const node = try self.parseExpression(.lowest);
        try list.append(node);
        try list.append(Node.new(.stmt_end, 0)); // Stack clean up.
        _ = self.skipTag(skip);
    }

    if (list.items.len > 0) _ = list.pop(); // Remove last stmt_end to leave only last value on the stack.
}

// Tests

test "Parser booleans" {
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const ctx = Context{ .filename = "inline", .src = "false true" };
    var lexer = Lexer{ .allocator = arena.allocator(), .ctx = ctx };
    var tokens = try lexer.lex();
    var parser = Parser{
        .allocator = arena.allocator(),
        .ctx = ctx,
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

//test "Parser print function" {
//    const allocator = std.testing.allocator;
//    var arena = std.heap.ArenaAllocator.init(allocator);
//    defer arena.deinit();
//    const ctx = Context{ .filename = "inline", .src = "foo := { a, b => z := 1; a + z - b !> \"foo.txt\" }" };
//    var lexer = Lexer{ .allocator = arena.allocator(), .ctx = ctx };
//    var tokens = try lexer.lex();
//    var parser = Parser{
//        .allocator = arena.allocator(),
//        .ctx = ctx,
//        .tokens = tokens,
//    };
//    const program = try parser.parse();
//
//    std.debug.print("\n--> {} <--\n", .{program.rules[0]});
//}
