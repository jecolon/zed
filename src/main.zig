const std = @import("std");

const Compiler = @import("Compiler.zig");
const Lexer = @import("Lexer.zig");
const Node = @import("Node.zig");
const Parser = @import("Parser.zig");
const Program = @import("Node.zig").Program;
const Scope = @import("Scope.zig");
const Value = @import("Value.zig");
const Vm = @import("Vm.zig");

pub fn main() anyerror!void {
    // TODO: Replace with command line flags.
    const program_filename = "run/program.zed";
    const filenames = [_][]const u8{
        "run/data_1.csv",
        "run/data_2.csv",
        "run/lang_mix.txt",
    };
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

    // Program file.
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

    var lexer = Lexer{
        .allocator = compiler_allocator,
        .filename = program_filename,
        .src = program_src,
    };
    const tokens = try lexer.lex();
    var parser = Parser{
        .allocator = compiler_allocator,
        .filename = program_filename,
        .src = program_src,
        .tokens = tokens,
    };
    const program = try parser.parse();

    // Backend
    var compiler = try Compiler.init(compiler_allocator);
    const compiled = try compiler.compileProgram(static_allocator, program);
    compiler_arena.deinit();

    // Program global scope
    var global_scope = Scope.init(static_allocator, null);
    try Vm.addBuiltins(&global_scope);

    // Some global state
    try global_scope.store("@ifs", Value.new(.{ .string = ifs }, 0));
    try global_scope.store("@irs", Value.new(.{ .string = irs }, 0));
    try global_scope.store("@ofs", Value.new(.{ .string = ofs }, 0));
    try global_scope.store("@ors", Value.new(.{ .string = ors }, 0));

    // Ranges map init.
    var ranges_map_ptr = try static_allocator.create(std.AutoHashMap(usize, void));
    ranges_map_ptr.* = std.AutoHashMap(usize, void).init(static_allocator);
    try global_scope.store("@ranges", Value.new(.{ .rec_range_map = ranges_map_ptr }, 0));

    // Init blocks
    var inits_arena = std.heap.ArenaAllocator.init(allocator);
    errdefer inits_arena.deinit();
    const inits_allocator = inits_arena.allocator();
    var inits_vm = try Vm.init(
        inits_allocator,
        program_filename,
        program_src,
        compiled.inits.constants,
        compiled.inits.instructions,
        &global_scope,
        &output,
    );
    try inits_vm.run();
    inits_arena.deinit();

    // Global record numbering
    var row: usize = 1;

    // Loop over input files.
    for (filenames) |filename| {
        // Filename
        try global_scope.store("@file", Value.new(.{ .string = filename }, 0));

        // onFile
        var files_arena = std.heap.ArenaAllocator.init(allocator);
        errdefer files_arena.deinit();
        const files_allocator = files_arena.allocator();
        var files_vm = try Vm.init(
            files_allocator,
            program_filename,
            program_src,
            compiled.files.constants,
            compiled.files.instructions,
            &global_scope,
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

        // Sime state
        var frow: usize = 1;
        var record_buf: [4096]u8 = undefined; //TODO: Is 4K too much?

        // Loop over records.
        while (try data_reader.readUntilDelimiterOrEof(&record_buf, global_scope.map.get("@irs").?.ty.string[0])) |record| : ({
            row += 1;
            frow += 1;
        }) {
            var recs_arena = std.heap.ArenaAllocator.init(allocator);
            defer recs_arena.deinit();
            const recs_allocator = recs_arena.allocator();

            // Record vars
            try global_scope.store("@row", Value.new(.{ .uint = row }, 0));
            try global_scope.store("@frow", Value.new(.{ .uint = frow }, 0));
            try global_scope.store("@rec", Value.new(.{ .string = record }, 0));

            // Rec blocks
            var recs_vm = try Vm.init(
                recs_allocator,
                program_filename,
                program_src,
                compiled.recs.constants,
                compiled.recs.instructions,
                &global_scope,
                &output,
            );
            try recs_vm.run();

            // New record, new fileds.
            var fields_list_ptr = try recs_allocator.create(std.ArrayList(Value));
            fields_list_ptr.* = std.ArrayList(Value).init(recs_allocator);

            // Loop over fields
            var field_iter = std.mem.split(u8, global_scope.map.get("@rec").?.ty.string, global_scope.map.get("@ifs").?.ty.string);
            while (field_iter.next()) |field| try fields_list_ptr.append(Value.new(.{ .string = field }, 0));
            try global_scope.store("@cols", Value.new(.{ .list = fields_list_ptr }, 0));

            // Eval the program
            var rules_vm = try Vm.init(
                recs_allocator,
                program_filename,
                program_src,
                compiled.rules.constants,
                compiled.rules.instructions,
                &global_scope,
                &output,
            );
            try rules_vm.run();

            // Output
            if (output.items.len != 0 and output.items.len != prev_output_len) {
                try output.append(global_scope.map.get("@ors").?.ty.string[0]);
            }

            // To know if we have new output.
            prev_output_len = output.items.len;
        }
    }

    // Exit blocks
    var exits_arena = std.heap.ArenaAllocator.init(allocator);
    defer exits_arena.deinit();
    const exits_allocator = exits_arena.allocator();
    var exits_vm = try Vm.init(
        exits_allocator,
        program_filename,
        program_src,
        compiled.exits.constants,
        compiled.exits.instructions,
        &global_scope,
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
    _ = @import("Vm.zig");
}
