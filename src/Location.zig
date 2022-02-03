const std = @import("std");

col: usize,
ctx: []const u8,
filename: []const u8,
line: usize,
offset: u16,

const Location = @This();

fn getDigitCount(n: f64) usize {
    return @floatToInt(usize, @log10(n) + 1.0);
}

fn writeArrowLine(self: Location, up: bool, writer: anytype) !void {
    const digits = getDigitCount(@intToFloat(f64, self.line));
    var i: usize = 0;
    while (i < digits + self.col + 2) : (i += 1) {
        try writer.writeByte(' ');
    }

    if (up) {
        try writer.writeAll("↑\n");
    } else {
        try writer.writeAll("↓");
    }
}

pub fn format(self: Location, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = fmt;
    _ = options;

    _ = try writer.print("in {s}, at line: {}, column: {}, file offset: {}.\n", .{ self.filename, self.line, self.col, self.offset });
    try self.writeArrowLine(false, writer);
    _ = try writer.print("\n{} | {s}\n", .{ self.line, self.ctx });
    try self.writeArrowLine(true, writer);
}

pub fn getLocation(filename: []const u8, src: []const u8, offset: u16) Location {
    var ctx_start: usize = 0;
    var location = Location{
        .col = 0,
        .ctx = "",
        .filename = filename,
        .line = 1,
        .offset = offset,
    };
    var i: usize = 0;

    while (i < src.len) : (i += 1) {
        if (i > offset) {
            const ctx_end = for (src[i..]) |byte, j| {
                if ('\n' == byte or j >= 40) break i + j;
            } else src.len;

            location.ctx = src[ctx_start..ctx_end];

            break;
        }

        if ('\n' == src[i]) {
            location.col = 0;
            location.line += 1;
            ctx_start = i + 1;
        } else {
            location.col += 1;
        }
    }

    if (location.ctx.len == 0) location.ctx = src[ctx_start..];

    return location;
}
