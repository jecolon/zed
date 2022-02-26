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
    // Allocation
    //var allocator = std.testing.allocator;
    const allocator = std.heap.page_allocator;

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

    // Lex
    var lexer = Lexer{ .allocator = compiler_allocator, .ctx = ctx };
    const tokens = try lexer.lex();
    // Parse
    var parser = Parser{
        .allocator = compiler_allocator,
        .ctx = ctx,
        .tokens = tokens,
    };
    const program = try parser.parse();

    // Backend / Compile to bytecode
    var compiler = try Compiler.init(compiler_allocator);
    const compiled = try compiler.compileProgram(static_allocator, program);
    compiler_arena.deinit();

    // Program scope stack with global scope
    var scope_stack = ScopeStack.init(static_allocator);

    // onInit
    var inits_arena = std.heap.ArenaAllocator.init(allocator);
    errdefer inits_arena.deinit();
    var inits_vm = try Vm.init(
        inits_arena.allocator(),
        compiled[0],
        &scope_stack,
        ctx,
        &output,
    );
    try inits_vm.run();
    inits_arena.deinit();

    // Loop over input files.
    while (args.next()) |filename| {
        // Filename
        scope_stack.file = filename;

        // onFile
        var files_arena = std.heap.ArenaAllocator.init(allocator);
        errdefer files_arena.deinit();
        var files_vm = try Vm.init(
            files_arena.allocator(),
            compiled[1],
            &scope_stack,
            ctx,
            &output,
        );
        try files_vm.run();
        files_arena.deinit();

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
        scope_stack.frnum = 1;

        // Loop over records.
        while (try data_reader.readUntilDelimiterOrEof(&scope_stack.rec_buf, scope_stack.irs[0])) |record| : ({
            scope_stack.rnum += 1;
            scope_stack.frnum += 1;
        }) {
            var recs_arena = std.heap.ArenaAllocator.init(allocator);
            defer recs_arena.deinit();
            const recs_allocator = recs_arena.allocator();
            //
            scope_stack.record = record;

            // onRec
            var recs_vm = try Vm.init(
                recs_allocator,
                compiled[2],
                &scope_stack,
                ctx,
                &output,
            );
            try recs_vm.run();

            // New record, new fileds.
            scope_stack.columns = try recs_allocator.create(std.ArrayList(Value));
            scope_stack.columns.* = std.ArrayList(Value).init(recs_allocator);

            // Loop over fields
            var field_iter = std.mem.split(u8, scope_stack.record, scope_stack.ifs);
            while (field_iter.next()) |field| try scope_stack.columns.append(Value.new(.{ .string = field }, 0));

            // For each record, exec the rules.
            var rules_vm = try Vm.init(
                recs_allocator,
                compiled[3],
                &scope_stack,
                ctx,
                &output,
            );
            try rules_vm.run();

            // Output
            if (output.items.len != 0 and output.items.len != prev_output_len) {
                try output.appendSlice(scope_stack.ors);
            }

            // To know if we have new output.
            prev_output_len = output.items.len;
        }
    }

    // onExit
    var exits_arena = std.heap.ArenaAllocator.init(allocator);
    defer exits_arena.deinit();
    var exits_vm = try Vm.init(
        exits_arena.allocator(),
        compiled[4],
        &scope_stack,
        ctx,
        &output,
    );
    try exits_vm.run();

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
