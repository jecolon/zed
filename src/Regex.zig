const std = @import("std");

const value = @import("value.zig");

const pcre2 = @cImport({
    @cDefine("PCRE2_CODE_UNIT_WIDTH", "8");
    @cInclude("pcre2.h");
});

pub const Regex = struct {
    code: *pcre2.pcre2_code_8,
    pattern: []const u8,

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
            .pattern = pattern,
        };

        // JIT compile it.
        const jit_options = pcre2.PCRE2_JIT_COMPLETE; // | pcre2.PCRE2_JIT_PARTIAL_HARD | pcre2.PCRE2_JIT_PARTIAL_SOFT;
        const jit_error = pcre2.pcre2_jit_compile_8(self.code, jit_options);
        if (jit_error != 0) std.log.debug("PCRE2 JIT compile failed: {}", .{jit_error}); // Fall-back to non-JIT mode.

        return self;
    }

    pub fn deinit(self: Regex) void {
        pcre2.pcre2_code_free_8(self.code);
    }

    pub fn match(self: Regex, subject: []const u8) !?Match {
        // match_data: *pcre2.pcre2_match_data
        var data_opt_ptr = pcre2.pcre2_match_data_create_from_pattern_8(self.code, null);
        // Match data creation failed?
        if (data_opt_ptr == null) return error.RegexCreateData;

        // No error, we can unwrap it safely.
        var m = Match{
            .code = self.code,
            .data = data_opt_ptr.?,
            .subject = subject,
            .ovector = undefined,
            .pattern = self.pattern,
        };

        var rc = pcre2.pcre2_match_8(
            self.code, //  the compiled pattern
            subject.ptr, //  the subject string
            subject.len, //  the length of the subject
            0, //  start at offset 0 in the subject
            0, //  default options
            m.data, //  block for storing the result
            null, //  use default match context
        );

        if (rc < 0) {
            //  Matching failed: handle error cases
            switch (rc) {
                pcre2.PCRE2_ERROR_NOMATCH => {},
                else => std.log.debug("Regex.match error: {}", .{rc}),
            }

            m.deinit();

            return null;
        }
        // The output vector wasn't big enough. This should not happen, because we used
        // pcre2_match_data_create_from_pattern() above.
        if (rc == 0) std.log.debug("ovector was not big enough for all the captured substrings.", .{});

        // Match succeeded. Get a pointer to the output vector, where string offsets are stored.
        m.captures_len = @intCast(usize, rc);
        m.ovector = pcre2.pcre2_get_ovector_pointer_8(m.data);

        // Since release 10.38 PCRE2 has locked out the use of \K in lookaround
        // assertions. However, there is an option to re-enable the old behaviour. If that
        // is set, it is possible to run patterns such as /(?=.\K)/ that use \K in an
        // assertion to set the start of a match later than its end. In this demonstration
        // program, we show how to detect this case, but it shouldn't arise because the
        // option is never set.
        if (m.ovector[0] > m.ovector[1]) {
            m.deinit();
            return error.RegexStartAfterFinish;
        }

        var namecount: usize = 0;
        _ = pcre2.pcre2_pattern_info_8(
            self.code, //  the compiled pattern
            pcre2.PCRE2_INFO_NAMECOUNT, //  get the number of named substrings
            &namecount, //  where to put the answer
        );

        if (namecount != 0) {
            m.name_count = namecount;
            // Before we can access the substrings, we must extract the table for
            // translating names to numbers, and the size of each entry in the table.

            var name_table_addr: usize = 0;
            _ = pcre2.pcre2_pattern_info_8(
                self.code, //  the compiled pattern
                pcre2.PCRE2_INFO_NAMETABLE, //  address of the table
                &name_table_addr, //  where to put the answer
            );

            _ = pcre2.pcre2_pattern_info_8(
                self.code, //  the compiled pattern
                pcre2.PCRE2_INFO_NAMEENTRYSIZE, //  size of each entry in the table
                &m.name_entry_size, //  where to put the answer
            );

            m.name_table_ptr = @intToPtr([*]u8, name_table_addr);
        }

        return m;
    }
};

pub const Match = struct {
    captures_len: usize = 0,
    code: *pcre2.pcre2_code_8,
    data: *pcre2.pcre2_match_data_8,
    name_count: usize = 0,
    name_entry_size: usize = 0,
    name_table_ptr: ?[*]u8 = null,
    ovector: [*c]usize,
    pattern: []const u8,
    subject: []const u8,

    pub fn deinit(self: Match) void {
        pcre2.pcre2_match_data_free_8(self.data);
    }

    pub fn offset(self: Match) usize {
        return self.ovector[0];
    }

    fn nameToIndex(self: Match, name: []const u8) ?usize {
        if (self.name_table_ptr == null) return null;

        var ptr_copy = self.name_table_ptr.?;
        var i: usize = 0;

        return while (i < self.name_count) : (i += 1) {
            const n: u16 = (@as(u16, ptr_copy[0]) << 8) | @as(u16, ptr_copy[1]);
            const cap_name = ptr_copy[2 .. self.name_entry_size - 1];
            if (std.mem.eql(u8, cap_name, name)) break n;
            ptr_copy += self.name_entry_size;
        } else null;
    }

    pub fn capture(self: Match, name: []const u8) ?[]const u8 {
        return if (self.nameToIndex(name)) |index| self.captureN(index) else null;
    }

    pub fn captureN(self: Match, n: usize) ?[]const u8 {
        if (n > self.captures_len) return null;
        return self.subject[self.ovector[2 * n]..self.ovector[2 * n + 1]];
    }
};
