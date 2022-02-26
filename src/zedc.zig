const std = @import("std");

const Compiler = @import("Compiler.zig");
const Context = @import("Context.zig");
const Lexer = @import("Lexer.zig");
const Node = @import("Node.zig");
const Parser = @import("Parser.zig");

fn printUsage() !void {
    std.log.err("Usage: zedc <your_program_file.zed>", .{});
    return error.InvalidUsage;
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    // Command line args.
    var args = try std.process.argsWithAllocator(arena.allocator());
    _ = args.skip(); // skip program name.

    // Program file.
    const program_filename = args.next() orelse return printUsage();
    var program_file = try std.fs.cwd().openFile(program_filename, .{});
    defer program_file.close();
    const program_src = try program_file.readToEndAlloc(arena.allocator(), 1024 * 64); // 64K

    // Context
    const ctx = Context{ .filename = program_filename, .src = program_src };

    // Frontend
    // Lex
    var lexer = Lexer{ .allocator = arena.allocator(), .ctx = ctx };
    const tokens = try lexer.lex();
    // Parse
    var parser = Parser{
        .allocator = arena.allocator(),
        .ctx = ctx,
        .tokens = tokens,
    };
    const program = try parser.parse();

    // Backend / Compile to bytecode
    var compiler = try Compiler.init(arena.allocator());
    const compiled = try compiler.compileProgram(arena.allocator(), program);

    // Output
    var buf = try std.ArrayList(u8).initCapacity(arena.allocator(), program_filename.len + 4);
    if (std.mem.endsWith(u8, program_filename, ".zed")) {
        _ = try buf.writer().print("{s}.zbc", .{program_filename[0 .. program_filename.len - 4]});
    } else {
        _ = try buf.writer().print("{s}.zbc", .{program_filename});
    }
    var bytecode_file = try std.fs.cwd().createFile(buf.items, .{});
    var out_buf = std.io.bufferedWriter(bytecode_file.writer());
    var writer = out_buf.writer();

    for (compiled) |bytes| {
        const len_bytes = std.mem.sliceAsBytes(&[1]u16{@intCast(u16, bytes.len)});
        try writer.writeAll(len_bytes);
        try writer.writeAll(bytes);
    }

    try out_buf.flush();
}
