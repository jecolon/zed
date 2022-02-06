pub const Opcode = enum(u8) {
    add,
    constant,
    div,
    eq,
    gt,
    gte,
    logic_not,
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
