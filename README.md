# zed
The zed Programming Language is inspired by AWK in its main mode of operating on text files divided into records (usually lines), and columns within
thos recoeds. Although inspired by AWK, zed is quite another language, with many features adopted from other dynamic languages. zed is written in 
[Zig](https://ziglang.org), which is an awesome systems-programming, low-level language that enables really high performance when executing zed code.

# The Language
## Comments
zed comments start with `#` and continue until the next `#` or the end of the line (whichever comes first). There are no multi-line comments in zed.
```
# This is a comment.
1 + # inline too # 1
```

## Predefined values
The usual suspects here:

```
true
false
nil
```

Yes, yes, I know... `nil` is evil...

## Variables
Use `:=` to define variables. Aside from that, they work as you'd expect in any dynamic language.

```
foo := 1
bar := 2
baz := foo + bar
```

## Numeric Types
There are three numeric types in zed, but being a dynamic language, you probably don't have to worry 
about it much.

```
float     := 3.14       # 64 bit floating point
sci_float := 3.14e+00   # Scientific notation
int       := -1981      # 32 bit signed; must start with + or -
uint      := 2112       # 32 bit unsigned
binary    := 0b0011     # binary literal
hex       := 0xaaff     # hexadecimal literal
octal     := 0o777      # octal literal
million   := 1_000_000  # underscores for readability; can be used in any of above literals.
```

Operations among equal numeric types produce the same type, otherwise floating point is the result type. If strings are 
involved in arithmetic operations, an attempt to parse the string as a float is made, if unsuccesful, the string is 
replaced with `0`. For example:

```
"1.2" + 3 == 4.2
"foo" + 3 == 3
```

## Strings
Use `"` to enclose strings. They must be valid UTF-8 text.

```
str := "Hi from zed! 😹\n"
```

The usual escapes work, plus escape syntax for Unicode code points.

```
str := "Tab: \t, Newline: \n, Return: \r, Quotes: \" Unicode: \u1F639"
```

### Interpolation
You can embed zed code in a string via interpolation between `{` and `}`. You can apply formatting to the 
resulting value using a subset of Zig's `std.fmt` formatting spec syntax between `#` and `#` right after the opening
`{` of an interpolation.

```
# str will be: "Padded with zeroes: 0000000002"
str := "Padded with zeroes: {#d:0>10# 1 + 1}"
```

#### Interpolation Format Specs
The format spec may contain placeholders following this format:

```
#[specifier]:[fill][alignment][width].[precision]#
```

Each word between `[` and `]` is a parameter you have to replace with something:

- *specifier* is a type-dependent formatting option that determines how a type should be formatted (see below)
- *fill* is a single character which is used to pad the formatted text
- *alignment* is one of the three characters `<`, `^` or `>`. they define if the text is *left*, *center*, or *right* aligned
- *width* is the total width of the field in characters
- *precision* specifies how many decimals a formatted number should have

The *specifier* has several options for types:

- `s`: strings
- `e`: output floating point value in scientific notation
- `d`: output numeric value in decimal notation

Note that the `d` specifier is not integer exclusive, when used with floating point numbers, it usually produces what 
you want to see.

## Lists
To create a list:

```
list := [1, 2, 3]
empty := []
```

## Maps
To create a map:

```
map := ["a": 1, "b": 2, "c": 3]
empty := [:]
```

Note that map keys have to evalutate to strings, i.e. literals, variables, or operations that produce a string result.

## Ranges
You define ranges like this:

```
0 ..< 10 # excludes 10
0 ..= 10 # includes 10
```

These are not too usefule without the iterative methods seen later on.

## Functions
Functions are created via literals:

```
func := { a, b => a + b }
func_2 := { => it + 1 } # implicit "it" param name for 1st arg
func_3 := { it + 1 }    # same as func_2
func_4 := { @0 + @1 }   # implicit "@N" param names for all args
```

### Implicit Parameter Names 
Every function gets a set of implicit parameter names defined in its scope. This allows you to define functions in a 
succint manner, which is useful in the context of methods that take functions as a predicate.

- `it`: The first argument passed to a call is bound to the name `it`.
- `@0`, `@1`, ... : Every argument is bound to one of these numeric names starting at `@0`.
- `index`: For list, map, and range methods, the index of the current element is bound to `index`.
- `key`, `value`: For map methods, the current entry key and value.
- `acc`: For the `reduce` methods, the passed-in accumulator.

Some examples:

```
adder := { a, b => a + b }      # explicit param names 
adder := { @0 + @1 }            # implicit @ params
addOne := { a => a + 1 }        # explicit first param name 
addOne := { it + 1 }            # implicit first param name 

[1, 2, 3].each() { v, i => print(i, v) }        # explicit list predicate param names
[1, 2, 3].each() { print(index, it) }           # implicit list predicate param names
[1, 2, 3].reduce(0) { a, v => a + v }           # explicit list reduce accumulator and value names
[1, 2, 3].reduce(0) { acc + it }                # implicit list reduce accumulator and value names

["a": 1, "b": 2].each() { k, v, i => print(i, k, v) }   # explicit map predicate param names
["a": 1, "b": 2].each() { print(index, key, value) }    # implicit map predicate param names

(3..<7).map() { v, i => v * i }         # explicit range param names
(3..<7).map() { it * index }            # implicit range param names; produces [0, 4, 10, 18]
```

## Conditionals
The usual suspects found in most dynamic languages. Note that for conditions the following truth table applies:

- *Booleans*: Wel... `true` is true, `false` is false
- *Numbers*: True if not `0`
- *Strings, lists, and maps*: True if length not `0`
- *Function*: True
- *Range*: True if upper bound - lower bound is not `0`
- *nil*: False
- *Anything else*: False

Here's some `if` conditionals:

```
if (1 > 2) a
if (1 > 2) a else b
if (1 > 2) { a + b } else { a - b }
```

zed has the *dreaded* ternary form:

```
a ? b : c
```

Since any expression can be evaluated for *truthyness*, Elvis is in the building:

```
a ?: b  # same as: if (a) a else b
        # also: a ? a : b
```

And even Elvis assignment:

```
a := nil
a ?= 1  # Since nil is not truthy, a is now 1
```

## Loops
Just the `while` and `do while` loops. Anything else can be done with the iterative methods on lists and maps.
Loop conditions adhere to the same truth table as conditonals in zed. `continue` and `break` work as expected.

```
i := 0
while (i < 10) {
    i += 1
    if (i == 2) continue
    print(i)
}

do {
    i += 1
    if (i == 8) break
    print(i)
} while (i < 10)        # will print i at least once
```

## Operators
Once again, nothing surprising here. The operators are shown here in descending level of precedence.

```
-foo ; !foo             # negative ; logical not

foo()                   # function call

foo.len()               # method select (dot)

foo[0]                  # subscript

a * b / c % d           # product ; quotient ; remainder ; repeat
"foo" ** 3

a + b - c               # sum ; difference ; concat
"foo" ++ "bar"

0..<10 ; 0..=10         # ranges 

a < b <= c > d          # comparison
e >= f

a and b                 # logical and

a or b                  # logical or

a ? b : c               # ternary

a ?: b                  # Elvis 

a = b ; a += b          # assignment 
a -= b ; a *= b
a /= b ; a %= b
a ?= b

a + b !> "out.txt" ; a + b +> "out.txt"  # output redirect (clobbers ; appends)
```

### Output Redirection
Noteworthy among the operators above are the two output redirection operators: `!>` and `+>`. With these, you can redirect 
what any expression produces to a file. `!>` overwrites (*clobbers*) any existing file with the given name, whereas `+>`
appends to any existing file. Both operators create the given file if it doesn't exist.

## File Processing Events
Similar to AWK's `BEGIN` and `END` blocks, zed provides several *event* blocks that let you execute code at precise 
points during file processing.

```
onInit {}    # executes once at program start.
onFile {}    # executes on new input file start.
onRec {}     # executes on new record start.
onExit {}    # executes once at program exit.
```

Any code outside of these event blocks is executed for every record of every input file.

## Record Ranges 
If you want to execute code for records that lie within starting and ending conditions, you can use *record ranges*.
Depending on the keywords used, record ranges can be exclusive or inclusive of the record that matches the ending condition.

```
# inclusive range of records:
select (@cols[0] == "<html>") ..= (@cols[0] == "</html>") { print(@rec) }

# same as previous line; default action is to print the current record:
select (@cols[0] == "<html>") ..= (@cols[0] == "</html>")

# exclusive range of records:
select (@cols[1] == "Product") ..< (@rec == "") { print(@cols[0], @cols[1]) }

# start range at first record (no start condition):
select ..< (@rec == "END") { print(@cols.mean()) }

# range until the end of input (no end condition):
select (@rnum == 10) { print(sqrt(@cols[2]) }

# you can nest record ranges
select (@rec == "<html>") ..= (@rec == "</html>") {
    select (@rec == "<body>") ..= (@rec == "</body>") {
        print(@cols[0])
    }
}
```

## Global variables
Here we differ a bit in what zed calls *Global* variables. These are predefined varibales that are always available
throughout program execution. They provide information about the current state of file processing. They're distinguished
easilly from other Variables given they start with `@`.

```
@file           # current input file name (read-only)
@rnum           # absolute current record number (read-only)
@frnum          # current record number, relative to current input file (read-only)
@rec            # current record 
@cols           # list of current record's columns (read-only)
@ics            # input column separator 
@irs            # input record separator 
@ocs            # output column separator
@ors            # output record separator
```

## Builtin Functions
Like AWK, zed comes with some builtin functions, mostly math, here they are:

```
atan2(0, -1)
cos(-1)
exp(5)
int(3.9)
log(3.14)
print("foo", @cols[0], 1 + 1)
rand(10)
sin(3.14)
sqrt(49)
```

## Builtin methods
In addition to the functions, zed provides some builtin methods, so called because they operate on specific objects
such as lists, maps, ranges, and strings.

```
# String methods
"foo".chars()                   # returns list of Unicode grapheme clusters
"foo".contains("o")
"foo".endsWith("oo")
"foo".indexOf("o")
"foo".lastIndexOf("o")
"foo".len()
"foo".startsWith("fo")
"foo".toLower()                 # returns new string
"foo".toUpper()                 # returns new string

# List methods
[1, 2, 3].contains("o")
[1, 2, 3].each() { print(it) }
[1, 2, 3].filter() { it > 1 }           # returns new list
[1, 2, 3].indexOf("o")
[1, 2, 3].lastIndexOf("o")
[1, 2, 3].len()
[1, 2, 3].map() { it * 2 }              # returns new list
[1, 2, 3].mean()
[1, 2, 3].median()
[1, 2, 3].mode()
[1, 2, 3].pop()                         # modifies list
[1, 2, 3].push(4)                       # modifies list
[1, 2, 3].reverse()                     # modifies list
[1, 2, 3].reduce(1) { acc * it }
[1, 2, 3].sortAsc()                     # modifies list
[1, 2, 3].sortDesc()                    # modifies list
[1, 2, 3].stdev()                       # Standard deviation

# Map methods
["a": 1, "b": 2].len()
["a": 1, "b": 2].keys()                 # returns list of keys
["a": 1, "b": 2].keysByValueAsc()       # returns list of keys, sorted by ascending values
["a": 1, "b": 2].keysByValueDesc()      # returns list of keys, sorted by descending values
["a": 1, "b": 2].values()               # returns list of values
["a": 1, "b": 2].each() { key, value, index => print("{key}: {value}") }

# Ranges
(0 ..< 10).each() { print(index, it) }
(0 ..< 10).filter() { it > 5 }          # returns a list
(0 ..< 10).map() { it * 2 }             # returns a list
(0 ..< 10).reduce(1) { acc * it }
```

## Compiling to Bytecode
zed is a bytecode interpreting virtual machine based language, allowing for ahead-of-time compilation of zed source code
to zed bytecode. zed source code must be in a file with the `.zed` extension and, once compiled, produces bytecode in a 
file with the same name except for a `.zbc` extension. The compiler executable is `zedc` and here's a sample run:

```
$ ls
your_program.zed
$ zedc your_program.zed
$ ls
your_program.zbc your_program.zed
```

You can now execute this bytecode with the `zed` command as shown next.

## Running zed Programs
zed can either compile and run `.zed` source files directly or just go straight to executing `.zbc` compiled bytecode
produced by `zedc` ahread-of-time. After the source or bytecode file name, you can list one or more input files to process
in the order as they appear on the command line. To complile and execute a `.zed` source file:

```
$ ls
your_data_1.csv your_data_2.csv your_program.zed
$ zed your_program.zed your_data_1.csv your_data_2.csv
Word            Count
---------------------
the.............    9
a...............    8
is..............    3
$ ls
your_data_1.csv your_data_2.csv your_program.zed
```

To execute a compiled `.zbc` file, the process is identical, except that the program file in the example would be 
`your_program.zbc` instead. Note that even when executing `.zbc` bytecode, you must have the `.zed` source code file 
in the same directory, this is used for producing contextual error messages showing the location in the source where 
the error occurred.

```
$ ls
your_data_1.csv your_data_2.csv your_program.zbc your_program.zed
$ zed your_program.zbc your_data_1.csv your_data_2.csv
Word            Count
---------------------
the.............    9
a...............    8
is..............    3
$ ls
your_data_1.csv your_data_2.csv your_program.zbc your_program.zed
```

## Sample Word Count Program 
We can't finish this README without the classic word counting example in zed. This program counts the space-separated
*words* and then presents them in descending order of occurrence.

```
# Only once at program start
onInit {
    @ics = " "          # Set input column separator. (default is ",")
    counts := [:]       # Initialize empty word counts map.
}

# Before processing each record
onRec {
    if (@rec) @rec = @rec.toLower()     # If the record isn't empty, lower case it.
}

# For each record (default input record separator (@irs) is "\n"
if (@rec) {                             # if the record isn't empty,
    @cols.each() { counts[it] += 1 }    # increment each column's count in counts map.
}

# Only once at program exit
onExit {
    print("{#s: <20# "Word"}   Count\n")        # Header
    print("-" ** 28 ++ "\n")                    # Uses string repeat (**) and concat (++)

    # Sort counts by descending value and print formatted keys and values.
    counts.keysByValueDesc().each() { print("{#s:.<20# it} {#d: >7# counts[it]}\n") }
}
```

