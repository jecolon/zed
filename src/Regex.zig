const std = @import("std");

const pcre2 = @cImport({
    @cDefine("PCRE2_CODE_UNIT_WIDTH", "8");
    @cInclude("pcre2.h");
});

const Regex = @This();

code: *pcre2.pcre2_code_8,
data: *pcre2.pcre2_match_data_8,

pub fn compile(pattern: []const u8) !Regex {
    var errornumber: c_int = 0;
    var erroroffset: usize = 0;

    var re_opt_ptr: ?*pcre2.pcre2_code_8 = pcre2.pcre2_compile_8(
        pattern.ptr, //  the pattern
        pattern.len,
        0, //  default options
        &errornumber, //  for error number
        &erroroffset, //  for error offset
        null, //  use default compile context
    );

    //  Compilation failed: print the error message and exit.
    if (re_opt_ptr == null) {
        var buf: [256]u8 = undefined;
        _ = pcre2.pcre2_get_error_message_8(errornumber, &buf, buf.len);
        std.log.err("PCRE2 compilation failed at offset {}: {s}", .{ erroroffset, &buf });
        return error.RegexCompileError;
    }

    // No error, we can unwrap it safely.
    var self = Regex{
        .code = re_opt_ptr.?,
        .data = undefined,
    };

    // JIT compile it.
    const jit_options = pcre2.PCRE2_JIT_COMPLETE; // | pcre2.PCRE2_JIT_PARTIAL_HARD | pcre2.PCRE2_JIT_PARTIAL_SOFT;
    const jit_error = pcre2.pcre2_jit_compile_8(self.code, jit_options);
    if (jit_error != 0) std.log.debug("PCRE2 JIT compile failed: {}", .{jit_error}); // Fall-back to non-JIT mode.

    // match_data: *pcre2.pcre2_match_data
    self.data = pcre2.pcre2_match_data_create_from_pattern_8(self.code, null).?;

    return self;
}

pub fn match(self: Regex, subject: []const u8) bool {
    var rc = pcre2.pcre2_match_8(
        self.code, //  the compiled pattern
        subject.ptr, //  the subject string
        subject.len, //  the length of the subject
        0, //  start at offset 0 in the subject
        0, //  default options
        self.data, //  block for storing the result
        null, //  use default match context
    );

    if (rc < 0) {
        //  Matching failed: handle error cases
        switch (rc) {
            pcre2.PCRE2_ERROR_NOMATCH => {},
            else => std.log.debug("Regex.match error: {}", .{rc}),
        }

        return false;
    }

    return true;
}

pub fn deinit(self: Regex) void {
    pcre2.pcre2_match_data_free_8(self.data);
    pcre2.pcre2_code_free_8(self.code);
}
