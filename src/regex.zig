const std = @import("std");

const value = @import("value.zig");

const pcre2 = @cImport({
    @cDefine("PCRE2_CODE_UNIT_WIDTH", "8");
    @cInclude("pcre2.h");
});

pub const Regex = struct {
    code: *pcre2.pcre2_code_8,
    is_crlf: bool = false,
    is_utf8: bool = false,
    name_count: usize = 0,
    name_entry_size: usize = 0,
    name_table_ptr: ?[*]u8 = null,
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
        //std.debug.print("\n--> {s} {} <--\n", .{ pattern, errornumber });

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
        const jit_options = pcre2.PCRE2_JIT_COMPLETE | pcre2.PCRE2_JIT_PARTIAL_HARD | pcre2.PCRE2_JIT_PARTIAL_SOFT;
        const jit_error = pcre2.pcre2_jit_compile_8(self.code, jit_options);
        if (jit_error != 0) std.log.debug("PCRE2 JIT compile failed: {}", .{jit_error}); // Fall-back to non-JIT mode.

        // Get name count.
        _ = pcre2.pcre2_pattern_info_8(
            self.code, //  the compiled pattern
            pcre2.PCRE2_INFO_NAMECOUNT, //  get the number of named substrings
            &self.name_count, //  where to put the answer
        );

        if (self.name_count != 0) {
            // Before we can access the substrings, we must extract the table for
            // translating names to numbers, and the size of each entry in the table.
            var name_table_addr: usize = 0;
            _ = pcre2.pcre2_pattern_info_8(
                self.code, //  the compiled pattern
                pcre2.PCRE2_INFO_NAMETABLE, //  address of the table
                &name_table_addr, //  where to put the answer
            );
            self.name_table_ptr = @intToPtr([*]u8, name_table_addr);

            _ = pcre2.pcre2_pattern_info_8(
                self.code, //  the compiled pattern
                pcre2.PCRE2_INFO_NAMEENTRYSIZE, //  size of each entry in the table
                &self.name_entry_size, //  where to put the answer
            );
        }

        var option_bits: u32 = 0;
        _ = pcre2.pcre2_pattern_info_8(
            self.code,
            pcre2.PCRE2_INFO_ALLOPTIONS,
            &option_bits,
        );
        self.is_utf8 = (option_bits & pcre2.PCRE2_UTF) != 0;

        // Now find the newline convention and see whether CRLF is a valid newline
        // sequence.
        var newline: u32 = 0;
        _ = pcre2.pcre2_pattern_info_8(
            self.code,
            pcre2.PCRE2_INFO_NEWLINE,
            &newline,
        );
        self.is_crlf = newline == pcre2.PCRE2_NEWLINE_ANY or
            newline == pcre2.PCRE2_NEWLINE_CRLF or
            newline == pcre2.PCRE2_NEWLINE_ANYCRLF;

        return self;
    }

    pub fn deinit(self: Regex) void {
        pcre2.pcre2_code_free_8(self.code);
    }

    pub fn match(self: Regex, subject: []const u8) !?Match {
        //std.debug.print("\n--> {s} {s} <--\n", .{ self.pattern, subject });
        // match_data: *pcre2.pcre2_match_data
        var data_opt_ptr = pcre2.pcre2_match_data_create_from_pattern_8(self.code, null);
        // Match data creation failed?
        if (data_opt_ptr == null) return error.RegexCreateData;
        errdefer pcre2.pcre2_match_data_free_8(data_opt_ptr.?);

        // No error, we can unwrap it safely.
        var m = Match{
            .re = self,
            .data = data_opt_ptr.?,
            .subject = subject,
        };

        m.captures_len = pcre2.pcre2_match_8(
            self.code, //  the compiled pattern
            subject.ptr, //  the subject string
            subject.len, //  the length of the subject
            0, //  start at offset 0 in the subject
            0, //  default options
            m.data, //  block for storing the result
            null, //  use default match context
        );

        if (m.captures_len < 0) {
            //  Matching failed: handle error cases
            switch (m.captures_len) {
                pcre2.PCRE2_ERROR_NOMATCH => {},
                else => std.log.err("Regex.match error: {}", .{m.captures_len}),
            }

            return null;
        }
        // The output vector wasn't big enough. This should not happen, because we used
        // pcre2_match_data_create_from_pattern() above.
        if (m.captures_len == 0) std.log.debug("ovector was not big enough for all the captured substrings.", .{});

        // Match succeeded. Get a pointer to the output vector, where string offsets are stored.
        const ovector = pcre2.pcre2_get_ovector_pointer_8(m.data);

        // Since release 10.38 PCRE2 has locked out the use of \K in lookaround
        // assertions. However, there is an option to re-enable the old behaviour. If that
        // is set, it is possible to run patterns such as /(?=.\K)/ that use \K in an
        // assertion to set the start of a match later than its end. In this demonstration
        // program, we show how to detect this case, but it shouldn't arise because the
        // option is never set.
        if (ovector[0] > ovector[1]) return error.RegexStartAfterFinish;

        return m;
    }

    pub fn replace(
        self: Regex,
        allocator: std.mem.Allocator,
        subject: []const u8,
        rep: []const u8,
    ) ![]u8 {
        //std.debug.print("\n--> {s} {s} {s} <--\n", .{ self.pattern, subject, rep });
        var buf = try allocator.alloc(u8, 1024); //TODO: Deal with this limit.
        var buf_len = buf.len;
        const num_replacements = pcre2.pcre2_substitute_8(
            self.code, //  the compiled pattern
            subject.ptr, //  the subject string
            subject.len, //  the length of the subject
            0, //  start at offset 0 in the subject
            pcre2.PCRE2_SUBSTITUTE_EXTENDED, //  default options
            null, // existing match_data or null
            null, // match context or null
            rep.ptr, // replacement
            rep.len, // replacement length
            buf.ptr, // output buffer
            &buf_len, // buffer length
        );

        if (num_replacements < 0) {
            std.log.err("Regex.replace error: {}", .{num_replacements});
            return error.RegexReplaceError;
        }

        return buf[0..buf_len];
    }

    pub fn nameToIndex(self: Regex, name: []const u8) ?usize {
        if (self.name_table_ptr == null) return null;

        var ptr_copy = self.name_table_ptr.?;
        var i: usize = 0;

        return while (i < self.name_count) : (i += 1) {
            const n: u16 = (@as(u16, ptr_copy[0]) << 8) | @as(u16, ptr_copy[1]);
            const capture_name = ptr_copy[2 .. self.name_entry_size - 1];
            if (std.mem.eql(u8, capture_name, name)) break n;
            ptr_copy += self.name_entry_size;
        } else null;
    }
};

pub const Match = struct {
    captures_len: c_int = 0,
    data: *pcre2.pcre2_match_data_8,
    re: Regex,
    ovector: ?[*c]usize = null,
    subject: []const u8,

    pub fn deinit(self: Match) void {
        pcre2.pcre2_match_data_free_8(self.data);
    }

    pub fn capture(self: Match, name: []const u8) ?[]const u8 {
        return if (self.re.nameToIndex(name)) |index| self.captureN(index) else null;
    }

    pub fn captureN(self: Match, n: usize) ?[]const u8 {
        if (n > self.captures_len) return null;
        const ovector = pcre2.pcre2_get_ovector_pointer_8(self.data);
        return self.subject[ovector[2 * n]..ovector[2 * n + 1]];
    }

    pub fn next(self: *Match) anyerror!bool {
        if (self.ovector == null) {
            self.ovector = pcre2.pcre2_get_ovector_pointer_8(self.data);
            return true;
        }

        const subject_len = self.subject.len;
        var options: u32 = 0; //  Normally no options
        var start_offset: usize = self.ovector.?[1]; //  Start at end of previous match

        // If the previous match was for an empty string, we are finished if we are
        // at the end of the subject. Otherwise, arrange to run another match at the
        // same point to see if a non-empty match can be found.
        if (self.ovector.?[0] == self.ovector.?[1]) {
            if (self.ovector.?[0] == subject_len) return false;
            options = pcre2.PCRE2_NOTEMPTY_ATSTART | pcre2.PCRE2_ANCHORED;
        } else {
            // If the previous match was not an empty string, there is one tricky case to
            // consider. If a pattern contains \K within a lookbehind assertion at the
            // start, the end of the matched string can be at the offset where the match
            // started. Without special action, this leads to a loop that keeps on matching
            // the same substring. We must detect this case and arrange to move the start on
            // by one character. The pcre2_get_startchar() function returns the starting
            // offset that was passed to pcre2_match().
            var startchar: usize = pcre2.pcre2_get_startchar_8(self.data);
            if (start_offset <= startchar) {
                if (startchar >= subject_len) return false; //  Reached end of subject.
                start_offset = startchar + 1; //  Advance by one character.

                //  If UTF-8, it may be more
                if (self.re.is_utf8) {
                    while (start_offset < subject_len) : (start_offset += 1) {
                        if ((self.subject[start_offset] & 0xc0) != 0x80) break;
                    }
                }
            }
        }

        //  Run the next matching operation
        self.captures_len = pcre2.pcre2_match_8(
            self.re.code, //  the compiled pattern
            self.subject.ptr, //  the subject string
            self.subject.len, //  the length of the subject
            start_offset, //  starting offset in the subject
            options, //  options
            self.data, //  block for storing the result
            null, //  use default match context
        );

        // This time, a result of NOMATCH isn't an error. If the value in "options"
        // is zero, it just means we have found all possible matches, so the loop ends.
        // Otherwise, it means we have failed to find a non-empty-string match at a
        // point where there was a previous empty-string match. In this case, we do what
        // Perl does: advance the matching position by one character, and continue. We
        // do this by setting the "end of previous match" offset, because that is picked
        // up at the top of the loop as the point at which to start again.
        //
        // There are two complications: (a) When CRLF is a valid newline sequence, and
        // the current position is just before it, advance by an extra byte. (b)
        // Otherwise we must ensure that we skip an entire UTF character if we are in
        // UTF mode.

        if (self.captures_len == pcre2.PCRE2_ERROR_NOMATCH) {
            if (options == 0) return false; //  All matches found
            self.ovector.?[1] = start_offset + 1; //  Advance one code unit

            if (self.re.is_crlf and //  If CRLF is a newline and
                start_offset < subject_len - 1 and //  we are at CRLF,
                self.subject[start_offset] == '\r' and
                self.subject[start_offset + 1] == '\n')
            {
                self.ovector.?[1] += 1; //  Advance by one more.
            } else if (self.re.is_utf8) {
                //  Otherwise, ensure we advance a whole UTF-8 character.
                while (self.ovector.?[1] < subject_len) {
                    if ((self.subject[self.ovector.?[1]] & 0xc0) != 0x80) break;
                    self.ovector.?[1] += 1;
                }
            }

            return self.next(); //  Go round the loop again
        }

        //  Other matching errors are not recoverable.
        if (self.captures_len < 0) {
            std.log.err("PCRE2 match error {}", .{self.captures_len});
            return error.RegexMatchError;
        }

        // The match succeeded, but the output vector wasn't big enough. This
        // should not happen.
        if (self.captures_len == 0) std.log.debug("ovector was not big enough for all the captured substrings.", .{});

        // We must guard against patterns such as /(?=.\K)/ that use \K in an
        // assertion to set the start of a match later than its end. In this
        // demonstration program, we just detect this case and give up. */
        if (self.ovector.?[0] > self.ovector.?[1]) return error.RegexStartAfterFinish;

        return true;
    }

    pub fn reset(self: *Match) void {
        //  Run the matching operation from the start.
        self.captures_len = pcre2.pcre2_match_8(
            self.re.code, //  the compiled pattern
            self.subject.ptr, //  the subject string
            self.subject.len, //  the length of the subject
            0, //  starting offset in the subject
            0, //  default options
            self.data, //  block for storing the result
            null, //  use default match context
        );

        self.ovector = null;
    }
};
