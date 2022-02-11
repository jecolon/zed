pub const Opcode = enum(u8) {
    add,
    call,
    constant,
    define,
    div,
    eq,
    func_return,
    gt,
    gte,
    jump,
    jump_false,
    jump_true,
    list,
    load,
    logic_and,
    logic_not,
    logic_or,
    lt,
    lte,
    map,
    mod,
    mul,
    negative,
    neq,
    pop,
    range,
    scope_in,
    scope_out,
    set,
    store,
    sub,
    subscript,

    pub const Def = struct {
        opcode: Opcode,
        bytes: u2,
    };

    pub fn fromInt(int: u8) ?Def {
        return switch (int) {
            0 => .{ .opcode = .add, .bytes = 1 },
            1 => .{ .opcode = .call, .bytes = 2 },
            2 => .{ .opcode = .constant, .bytes = 3 },
            3 => .{ .opcode = .define, .bytes = 1 },
            4 => .{ .opcode = .div, .bytes = 1 },
            5 => .{ .opcode = .eq, .bytes = 1 },
            6 => .{ .opcode = .func_return, .bytes = 1 },
            7 => .{ .opcode = .gt, .bytes = 1 },
            8 => .{ .opcode = .gte, .bytes = 1 },
            9 => .{ .opcode = .jump, .bytes = 3 },
            10 => .{ .opcode = .jump_false, .bytes = 3 },
            11 => .{ .opcode = .jump_true, .bytes = 3 },
            12 => .{ .opcode = .list, .bytes = 3 },
            13 => .{ .opcode = .load, .bytes = 1 },
            14 => .{ .opcode = .logic_and, .bytes = 1 },
            15 => .{ .opcode = .logic_not, .bytes = 1 },
            16 => .{ .opcode = .logic_or, .bytes = 1 },
            17 => .{ .opcode = .lt, .bytes = 1 },
            18 => .{ .opcode = .lte, .bytes = 1 },
            19 => .{ .opcode = .map, .bytes = 1 },
            20 => .{ .opcode = .mod, .bytes = 1 },
            21 => .{ .opcode = .mul, .bytes = 1 },
            22 => .{ .opcode = .negative, .bytes = 1 },
            23 => .{ .opcode = .neq, .bytes = 1 },
            24 => .{ .opcode = .pop, .bytes = 1 },
            25 => .{ .opcode = .range, .bytes = 2 },
            26 => .{ .opcode = .scope_in, .bytes = 2 },
            27 => .{ .opcode = .scope_out, .bytes = 2 },
            28 => .{ .opcode = .set, .bytes = 2 },
            29 => .{ .opcode = .store, .bytes = 2 },
            30 => .{ .opcode = .sub, .bytes = 1 },
            31 => .{ .opcode = .subscript, .bytes = 1 },
            else => null,
        };
    }
};

const Bytecode = @This();
