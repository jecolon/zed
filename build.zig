const std = @import("std");
const deps = @import("deps.zig");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zed", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.setTarget(target);
    deps.addAllTo(exe);
    //exe.strip = true;
    exe.single_threaded = true;
    //exe.addLibPath("/opt/homebrew/Cellar/pcre2/10.39/lib");
    //exe.addIncludePath("/opt/homebrew/Cellar/pcre2/10.39/include");
    exe.addIncludePath("libs/pcre2-10.39/src");
    //exe.linkSystemLibrary("pcre2-8");

    exe.addCSourceFiles(&.{
        "libs/pcre2-10.39/src/pcre2_auto_possess.c",
        "libs/pcre2-10.39/src/pcre2_chartables.c",
        "libs/pcre2-10.39/src/pcre2_compile.c",
        "libs/pcre2-10.39/src/pcre2_config.c",
        "libs/pcre2-10.39/src/pcre2_context.c",
        "libs/pcre2-10.39/src/pcre2_convert.c",
        "libs/pcre2-10.39/src/pcre2_dfa_match.c",
        "libs/pcre2-10.39/src/pcre2_error.c",
        "libs/pcre2-10.39/src/pcre2_extuni.c",
        "libs/pcre2-10.39/src/pcre2_find_bracket.c",
        "libs/pcre2-10.39/src/pcre2_jit_compile.c",
        "libs/pcre2-10.39/src/pcre2_maketables.c",
        "libs/pcre2-10.39/src/pcre2_match.c",
        "libs/pcre2-10.39/src/pcre2_match_data.c",
        "libs/pcre2-10.39/src/pcre2_newline.c",
        "libs/pcre2-10.39/src/pcre2_ord2utf.c",
        "libs/pcre2-10.39/src/pcre2_pattern_info.c",
        "libs/pcre2-10.39/src/pcre2_script_run.c",
        "libs/pcre2-10.39/src/pcre2_serialize.c",
        "libs/pcre2-10.39/src/pcre2_string_utils.c",
        "libs/pcre2-10.39/src/pcre2_study.c",
        "libs/pcre2-10.39/src/pcre2_substitute.c",
        "libs/pcre2-10.39/src/pcre2_substring.c",
        "libs/pcre2-10.39/src/pcre2_tables.c",
        "libs/pcre2-10.39/src/pcre2_ucd.c",
        "libs/pcre2-10.39/src/pcre2_valid_utf.c",
        "libs/pcre2-10.39/src/pcre2_xclass.c",
    }, &.{
        "-D",
        "PCRE2_CODE_UNIT_WIDTH=8",
        "-D",
        "HAVE_CONFIG_H",
        "-I",
        "libs/pcre2-10.39/src",
    });

    exe.linkLibC();
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest("src/main.zig");
    exe_tests.setTarget(target);
    exe_tests.setBuildMode(mode);
    deps.addAllTo(exe_tests);
    exe_tests.linkLibC();
    exe_tests.addLibPath("/opt/homebrew/Cellar/pcre2/10.39/lib");
    exe_tests.addIncludePath("/opt/homebrew/Cellar/pcre2/10.39/include");
    exe_tests.linkSystemLibrary("pcre2-8");

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&exe_tests.step);

    const compiler_exe = b.addExecutable("zedc", "src/zedc.zig");
    compiler_exe.setTarget(target);
    compiler_exe.setBuildMode(mode);
    compiler_exe.setTarget(target);
    compiler_exe.strip = true;
    compiler_exe.single_threaded = true;
    compiler_exe.install();
}
