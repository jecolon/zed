pub const Opcode = enum(u8) {
    constant,
    pop,
};

const Bytecode = @This();
