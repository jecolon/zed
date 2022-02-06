pub const Opcode = enum(u8) {
    constant,
    logic_not,
    negative,
    pop,
};

const Bytecode = @This();
