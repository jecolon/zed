const std = @import("std");

const Location = @import("Location.zig");

filename: []const u8,
src: []const u8,

const Context = @This();

pub fn err(self: Context, comptime fmt: []const u8, args: anytype, e: anyerror, offset: u16) anyerror {
    const location = Location.getLocation(self.filename, self.src, offset);
    std.log.err(fmt, args);
    std.debug.print("{}", .{location});
    return e;
}
