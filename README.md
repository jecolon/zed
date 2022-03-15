# zed
The zed Programming Language is inspired by AWK in its main mode of operating on text files divided into records (usually lines), and delimited fields within
thos recoeds. Although inspired by AWK, zed is it's own language, with many other features adopted from many other dynamic languages. zed is written in 
[Zig](https://ziglang.org), which is an awesome systems-programming, low-level language that allows for really fast performance when executing zed code.

# Syntax
## Comments
zed comments start with `#` and continue until the next `#` or the end of the line (whichever comes first). There are no multi-line comments in zed.
```
# This is a comment.
1 + # inline too # 1
```
## Variables
Use `:=` to define variables. Aside from that, they work as you'd expect in any dynamic language.
```
foo := 1
bar := 2
baz := foo + bar
```

