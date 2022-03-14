const std = @import("std");
const FormatOptions = std.fmt.FormatOptions;

const value = @import("value.zig");
const Value = value.Value;

const Lexer = struct {
    offset: ?usize = null,
    src: []const u8,

    fn advance(self: *Lexer) ?u8 {
        if (self.offset) |*offset| {
            offset.* += 1;
            if (offset.* >= self.src.len) return null;
        } else {
            self.offset = 0;
        }

        return self.src[self.offset.?];
    }

    fn peekN(self: Lexer, n: usize) ?u8 {
        if (self.offset) |offset| {
            return if (offset + n < self.src.len) self.src[offset + n] else null;
        } else {
            return if (n - 1 < self.src.len) self.src[n - 1] else null;
        }
    }

    fn peekNIs(self: Lexer, n: usize, byte: u8) bool {
        return if (self.peekN(n)) |peek_byte| peek_byte == byte else false;
    }

    fn peek(self: Lexer) ?u8 {
        return self.peekN(1);
    }

    fn peekIs(self: Lexer, byte: u8) bool {
        return self.peekNIs(1, byte);
    }

    fn skip(self: *Lexer, byte: u8) bool {
        if (self.peekIs(byte)) {
            _ = self.advance();
            return true;
        }

        return false;
    }

    fn until(self: *Lexer, byte: u8) void {
        while (self.peek() != null and !self.peekIs(byte)) _ = self.advance();
    }
};

pub const FormatSpec = struct {
    options: FormatOptions = .{},
    spec: u8 = 'd',

    pub fn format(self: FormatSpec, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        const f =
            \\Fortmat{{
            \\  .spec = '{u}',
            \\  .fill = '{u}',
            \\  .alignment = {},
            \\  .width = {},
            \\  .precision = {},
            \\}}
        ;
        _ = try writer.print(f, .{
            self.spec,
            self.options.fill,
            self.options.alignment,
            self.options.width,
            self.options.precision,
        });
    }
};

fn isValidSpec(byte: u8) bool {
    return switch (byte) {
        'd',
        'e',
        's',
        => true,
        else => false,
    };
}

pub fn parseFormat(fmt: []const u8) anyerror!FormatSpec {
    var format = FormatSpec{};
    var lxr = Lexer{ .src = fmt };
    if (!lxr.peekIs(':')) {
        // We have specifier.
        format.spec = lxr.advance().?;
        if (!isValidSpec(format.spec)) return error.InvalidSpec;
    }

    if (lxr.skip(':')) {
        if (lxr.peekN(2)) |peek_byte| {
            if (std.mem.indexOfScalar(u8, "<^>", peek_byte) != null) {
                // We have alignment, so we must have fill too.
                format.options.fill = lxr.advance().?;
                format.options.alignment = switch (lxr.advance().?) {
                    '<' => .Left,
                    '^' => .Center,
                    '>' => .Right,
                    else => unreachable,
                };
            }
        }

        // Next maybe be width.
        if (lxr.peek()) |peek_byte| {
            if ('.' != peek_byte) {
                const w_start = lxr.offset.? + 1;
                lxr.until('.');
                const end = lxr.offset.? + 1;
                format.options.width = try std.fmt.parseInt(usize, fmt[w_start..end], 10);
            }
        }

        if (lxr.peek()) |peek_byte| {
            if ('.' == peek_byte) {
                // We have precision.
                format.options.precision = try std.fmt.parseInt(usize, fmt[lxr.offset.? + 2 ..], 10);
            }
        }
    }

    return format;
}

pub fn runtimePrint(
    allocator: std.mem.Allocator,
    fmt: []const u8,
    v: Value,
    writer: anytype,
) anyerror!void {
    const format = try parseFormat(fmt);

    switch (format.spec) {
        'd' => {
            if (!value.isFloat(v) and !value.isInt(v) and !value.isUint(v)) return error.InvalidFormatD;
            if (value.asFloat(v)) |f| {
                var buf = std.ArrayList(u8).init(allocator);
                defer buf.deinit();
                var buf_writer = buf.writer();
                try std.fmt.formatFloatDecimal(
                    f,
                    format.options,
                    buf_writer,
                );
                try std.fmt.formatBuf(
                    buf.items,
                    format.options,
                    writer,
                );
            }
            if (value.asInt(v)) |i| {
                try std.fmt.formatInt(
                    i,
                    10,
                    .lower,
                    format.options,
                    writer,
                );
            }
            if (value.asUint(v)) |u| {
                try std.fmt.formatInt(
                    u,
                    10,
                    .lower,
                    format.options,
                    writer,
                );
            }
        },
        'e' => {
            if (!value.isFloat(v)) return error.InvalidFormatE;
            var buf = std.ArrayList(u8).init(allocator);
            defer buf.deinit();
            var buf_writer = buf.writer();
            try std.fmt.formatFloatScientific(
                value.asFloat(v).?,
                format.options,
                buf_writer,
            );
            try std.fmt.formatBuf(
                buf.items,
                format.options,
                writer,
            );
        },
        's' => {
            if (!value.isAnyStr(v)) return error.InvalidFormatS;
            const s = if (value.unboxStr(v)) |u| std.mem.asBytes(&u) else value.asString(v).?.string;
            try std.fmt.formatBuf(
                s,
                format.options,
                writer,
            );
        },

        else => return error.UnknownFormatSpec,
    }
}
