pub const Opcode = enum(u8) {
    add,
    constant,
    div,
    eq,
    jump,
    jump_false,
    jump_true,
    gt,
    gte,
    logic_and,
    logic_not,
    logic_or,
    lt,
    lte,
    mod,
    mul,
    negative,
    neq,
    pop,
    sub,
};

const Bytecode = @This();
