const std = @import("std");

const Compiler = @import("Compiler.zig");
const Context = @import("Context.zig");
const Lexer = @import("Lexer.zig");
const Node = @import("Node.zig");
const Parser = @import("Parser.zig");
//const Program = @import("Node.zig").Program;
const Scope = @import("Scope.zig");
const ScopeStack = @import("ScopeStack.zig");
const Value = @import("Value.zig");
const Vm = @import("Vm.zig");

fn printUsage() !void {
    std.log.err("Usage: zed <your_program_file.zed> <data_file_1> <data_file_2> ... <data_file_n>\n", .{});
    return error.InvalidUsage;
}

pub fn main() anyerror!void {
    // Set defaults for field and record globals.
    const ifs = ",";
    const irs = "\n";
    const ofs = ",";
    const ors = "\n";

    // Allocation
    //var allocator = std.testing.allocator;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var static_arena = std.heap.ArenaAllocator.init(allocator);
    defer static_arena.deinit();
    const static_allocator = static_arena.allocator();

    // Command line args.
    var args = try std.process.argsWithAllocator(allocator);
    _ = args.skip(); // skip program name.

    // Program file.
    const program_filename = args.next() orelse return printUsage();
    var program_file = try std.fs.cwd().openFile(program_filename, .{});
    defer program_file.close();
    const program_src = try program_file.readToEndAlloc(static_allocator, 1024 * 64); // 64K

    // Output change check
    var output = std.ArrayList(u8).init(static_allocator);
    var prev_output_len: usize = 0;

    // Frontend
    var compiler_arena = std.heap.ArenaAllocator.init(allocator);
    errdefer compiler_arena.deinit();
    const compiler_allocator = compiler_arena.allocator();

    // Context
    const ctx = Context{ .filename = program_filename, .src = program_src };

    var lexer = Lexer{ .allocator = compiler_allocator, .ctx = ctx };
    const tokens = try lexer.lex();
    var parser = Parser{
        .allocator = compiler_allocator,
        .ctx = ctx,
        .tokens = tokens,
    };
    const program = try parser.parse();

    // Backend
    var compiler = try Compiler.init(compiler_allocator);
    for (program.rules) |n| try compiler.compile(n);
    const compiled = try compiler.compileProgram(static_allocator, program);
    compiler_arena.deinit();

    // Program global scope
    var scope_stack = ScopeStack.init(static_allocator);
    try scope_stack.push(Scope.init(static_allocator, .function));
    const global_scope = scope_stack.head();
    try Vm.addBuiltins(global_scope);

    // Some global state
    try global_scope.store("@ifs", Value.new(.{ .string = ifs }, 0));
    try global_scope.store("@irs", Value.new(.{ .string = irs }, 0));
    try global_scope.store("@ofs", Value.new(.{ .string = ofs }, 0));
    try global_scope.store("@ors", Value.new(.{ .string = ors }, 0));

    // Ranges map init.
    global_scope.rec_ranges = std.AutoHashMap(u8, void).init(static_allocator);

    // Init blocks
    //var inits_arena = std.heap.ArenaAllocator.init(allocator);
    //errdefer inits_arena.deinit();
    //const inits_allocator = inits_arena.allocator();
    var inits_vm = try Vm.init(
        static_allocator,
        compiled[0],
        scope_stack,
        ctx,
        &output,
    );
    inits_vm.run() catch |err| {
        std.log.err("Error executing onInit blocks: {}.", .{err});
        return err;
    };
    //inits_arena.deinit();

    // Global record numbering
    var rnum: usize = 1;

    // Loop over input files.
    while (args.next()) |filename| {
        // Filename
        try global_scope.store("@file", Value.new(.{ .string = filename }, 0));

        // onFile
        //var files_arena = std.heap.ArenaAllocator.init(allocator);
        //errdefer files_arena.deinit();
        //const files_allocator = files_arena.allocator();
        var files_vm = try Vm.init(
            static_allocator,
            compiled[1],
            scope_stack,
            ctx,
            &output,
        );
        files_vm.run() catch |err| {
            std.log.err("Error executing onFile blocls: {}.", .{err});
            return err;
        };
        //files_arena.deinit();

        // Data file
        var data_file: std.fs.File = undefined;

        if (std.mem.eql(u8, filename, "-")) {
            data_file = std.io.getStdIn();
        } else {
            data_file = try std.fs.cwd().openFile(filename, .{});
        }
        defer data_file.close();
        var data_reader = std.io.bufferedReader(data_file.reader()).reader();

        // File record numbering
        var frnum: usize = 1;

        // Loop over records.
        while (try data_reader.readUntilDelimiterOrEof(&global_scope.rec_buf, global_scope.map.get("@irs").?.ty.string[0])) |record| : ({
            rnum += 1;
            frnum += 1;
        }) {
            //var recs_arena = std.heap.ArenaAllocator.init(allocator);
            //defer recs_arena.deinit();
            //const recs_allocator = recs_arena.allocator();

            // Record vars
            try global_scope.store("@rnum", Value.new(.{ .uint = rnum }, 0));
            try global_scope.store("@frnum", Value.new(.{ .uint = frnum }, 0));
            global_scope.record = record;

            var recs_vm = try Vm.init(
                static_allocator,
                compiled[2],
                scope_stack,
                ctx,
                &output,
            );
            recs_vm.run() catch |err| {
                std.log.err("Error executing onRec blocks: {}.", .{err});
                return err;
            };

            // New record, new fileds.
            global_scope.columns = try static_allocator.create(std.ArrayList(Value));
            defer static_allocator.destroy(global_scope.columns);
            global_scope.columns.* = std.ArrayList(Value).init(static_allocator);
            defer global_scope.columns.deinit();

            // Loop over fields
            var field_iter = std.mem.split(u8, global_scope.record, global_scope.map.get("@ifs").?.ty.string);
            while (field_iter.next()) |field| try global_scope.columns.append(Value.new(.{ .string = field }, 0));

            // Eval the program
            var rules_vm = try Vm.init(
                static_allocator,
                compiled[3],
                scope_stack,
                ctx,
                &output,
            );
            try rules_vm.run();
            //rules_vm.run() catch |err| {
            //    std.log.err("Error executing per-record rules: {}.", .{err});
            //    return err;
            //};

            // Output
            if (output.items.len != 0 and output.items.len != prev_output_len) {
                try output.appendSlice(global_scope.map.get("@ors").?.ty.string);
            }

            // To know if we have new output.
            prev_output_len = output.items.len;
        }
    }

    // Exit blocks
    //var exits_arena = std.heap.ArenaAllocator.init(allocator);
    //defer exits_arena.deinit();
    //const exits_allocator = exits_arena.allocator();
    var exits_vm = try Vm.init(
        static_allocator,
        compiled[4],
        scope_stack,
        ctx,
        &output,
    );
    exits_vm.run() catch |err| {
        std.log.err("Error executing onExit blocks: {}.", .{err});
        return err;
    };

    // Print hte output.
    _ = try std.io.getStdOut().writer().print("{s}", .{output.items});
}

test {
    _ = @import("Lexer.zig");
    _ = @import("Parser.zig");
    _ = @import("Compiler.zig");
    _ = @import("Value.zig");
    _ = @import("Vm.zig");
}
