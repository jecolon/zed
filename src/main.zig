const std = @import("std");

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
}

test {
    _ = @import("Lexer.zig");
    _ = @import("Parser.zig");
    _ = @import("Compiler.zig");
    _ = @import("Vm.zig");
}
