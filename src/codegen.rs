use crate::instruction::{Instruction, InstructionKind, OpSize, RegSelector, ShiftAmount};

pub fn gen(insts: &[Instruction]) -> Vec<u16> {
    insts.into_iter().flat_map(|i| gen_inst(*i)).collect()
}

fn gen_inst(inst: Instruction) -> Vec<u16> {
    use InstructionKind::*;
    match *inst.kind() {
        Move { src, dst } => {
            // place a 1 bit everywhere we want to *not* move
            let skip_mask = src.selector().bits() ^ 0b1111;
            vec![op_from_parts(
                dst.reg().idx(),
                src.reg().idx(),
                skip_mask,
                opcode::MOVE,
            )]
        }
        Swizzle { reg } => {
            let bits = reg.selector().bits();
            let source = bits >> 4;
            let extra = bits & 0b1111;
            vec![op_from_parts(
                reg.reg().idx(),
                source,
                extra,
                opcode::SWIZZLE,
            )]
        }
        Load { mem, dst } => {
            let size = 4 - dst.selector().bits().count_ones() as u8;
            let scatter = u8::from(mem.scatter());
            let increment = u8::from(mem.increment());
            let extra = (size << 2) | (scatter << 1) | increment;
            vec![op_from_parts(
                dst.reg().idx(),
                mem.reg().idx(),
                extra,
                opcode::LOAD,
            )]
        }
        Store { src, mem } => {
            let size = 4 - src.selector().bits().count_ones() as u8;
            let scatter = u8::from(mem.scatter());
            let increment = u8::from(mem.increment());
            let extra = (size << 2) | (scatter << 1) | increment;
            // this uses src as the dest operand and mem as the source operand because
            // both load and store use the source as an address and dest as a value
            // https://github.com/Meisaka/MeiVM2/blob/cd687f44a11bc3a0f318dcb1badb23f1f8dce44f/vm.txt#L64-L65
            vec![op_from_parts(
                src.reg().idx(),
                mem.reg().idx(),
                extra,
                opcode::STORE,
            )]
        }
        WAdd { src, dst } => vec![op_from_parts(
            dst.idx(),
            src.reg().idx(),
            wselect_ops::WADD | src.selector().bits() << 2,
            opcode::WSELECT,
        )],
        WSub { src, dst } => vec![op_from_parts(
            dst.idx(),
            src.reg().idx(),
            wselect_ops::WSUB | src.selector().bits() << 2,
            opcode::WSELECT,
        )],
        WSwap { src, dst } => vec![op_from_parts(
            dst.idx(),
            src.reg().idx(),
            wselect_ops::WSWAP | src.selector().bits() << 2,
            opcode::WSELECT,
        )],
        WMove { src, dst } => vec![op_from_parts(
            dst.idx(),
            src.reg().idx(),
            wselect_ops::WMOVE | src.selector().bits() << 2,
            opcode::WSELECT,
        )],

        Add { size, src, dst } => vec![math_op(math_ops::ADD, size, src, dst)],
        AddSaturate { size, src, dst } => vec![math_op(math_ops::ADD_SAT, size, src, dst)],
        Sub { size, src, dst } => vec![math_op(math_ops::SUB, size, src, dst)],
        SubSaturate { size, src, dst } => vec![math_op(math_ops::SUB_SAT, size, src, dst)],
        SubRev { size, src, dst } => vec![math_op(math_ops::SUBREV, size, src, dst)],
        SubRevSaturate { size, src, dst } => vec![math_op(math_ops::SUBREV_SAT, size, src, dst)],
        CmpEq { size, src, dst } => vec![math_op(math_ops::CMPEQ, size, src, dst)],
        CmpNeq { size, src, dst } => vec![math_op(math_ops::CMPNEQ, size, src, dst)],

        ShiftLeft { size, dst, amount } => vec![shift_op(shift_ops::LEFT_SHIFT, size, dst, amount)],
        ShiftRightLogical { size, dst, amount } => {
            vec![shift_op(shift_ops::LOGICAL_RIGHT_SHIFT, size, dst, amount)]
        }
        ShiftRightArithmetic { size, dst, amount } => {
            vec![shift_op(
                shift_ops::ARITHMETIC_RIGHT_SHIFT,
                size,
                dst,
                amount,
            )]
        }
        RotateLeft { size, dst, amount } => {
            vec![shift_op(shift_ops::ROTATE_LEFT, size, dst, amount)]
        }
        RotateRight { size, dst, amount } => {
            vec![shift_op(shift_ops::ROTATE_RIGHT, size, dst, amount)]
        }

        BitAnd { src, dst } => vec![op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::AND,
            opcode::BITOP,
        )],
        BitOr { src, dst } => vec![op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::OR,
            opcode::BITOP,
        )],
        BitXor { src, dst } => vec![op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::XOR,
            opcode::BITOP,
        )],
        BitNand { src, dst } => vec![op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::NAND,
            opcode::BITOP,
        )],
        BitNor { src, dst } => vec![op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::NOR,
            opcode::BITOP,
        )],
        BitXnor { src, dst } => vec![op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::XNOR,
            opcode::BITOP,
        )],
        UnaryBitNot { dst } => vec![op_from_parts(dst.idx(), 0, bit_ops::NOT_DST, opcode::BITOP)],
        Raw { val } => vec![val],
    }
}

fn op_from_parts(dst: u8, src: u8, extra: u8, op: u8) -> u16 {
    (u16::from(dst) << 12) | (u16::from(src) << 8) | (u16::from(extra) << 4) | u16::from(op)
}

fn math_op(op: u8, size: OpSize, src: RegSelector, dst: RegSelector) -> u16 {
    let size = match size {
        OpSize::Byte => opcode::MATH8,
        OpSize::Word => opcode::MATH16,
    };
    op_from_parts(dst.idx(), src.idx(), op, size)
}

fn shift_op(op: u8, size: OpSize, dst: RegSelector, amount: ShiftAmount) -> u16 {
    let size = match size {
        OpSize::Byte => opcode::SHIFT8,
        OpSize::Word => opcode::SHIFT16,
    };
    let (op, src) = match amount {
        ShiftAmount::Register(reg) => (op, reg.idx()),
        ShiftAmount::Const(val, _) => (op | 0b1000, val),
    };
    op_from_parts(dst.idx(), src, op, size)
}

mod opcode {
    /// TODO: implement
    #[expect(dead_code, reason = "not yet implemented by the assembler")]
    pub(super) const SYSTEM: u8 = 0b0000;
    pub(super) const WSELECT: u8 = 0b0001;
    #[expect(dead_code, reason = "not yet used by the VM")]
    pub(super) const EXTRA2: u8 = 0b0010;
    #[expect(dead_code, reason = "not yet used by the VM")]
    pub(super) const EXTRA3: u8 = 0b0011;
    pub(super) const MOVE: u8 = 0b0100;
    pub(super) const SWIZZLE: u8 = 0b0101;
    pub(super) const LOAD: u8 = 0b0110;
    pub(super) const STORE: u8 = 0b0111;
    pub(super) const MATH8: u8 = 0b1000;
    pub(super) const MATH16: u8 = 0b1001;
    pub(super) const SHIFT8: u8 = 0b1010;
    pub(super) const SHIFT16: u8 = 0b1011;
    pub(super) const BITOP: u8 = 0b1100;
    /// TODO: implement
    #[expect(dead_code, reason = "not yet implemented by the assembler")]
    pub(super) const SPECOP: u8 = 0b1101;
    #[expect(dead_code, reason = "not yet used by the VM")]
    pub(super) const EXTRA14: u8 = 0b1110;
    #[expect(dead_code, reason = "not yet used by the VM")]
    pub(super) const EXTRA15: u8 = 0b1111;
}

mod wselect_ops {
    pub(super) const WMOVE: u8 = 0b0000;
    pub(super) const WSWAP: u8 = 0b0001;
    pub(super) const WADD: u8 = 0b0010;
    pub(super) const WSUB: u8 = 0b0011;
}

mod math_ops {
    pub(super) const ADD: u8 = 0x0;
    pub(super) const SUB: u8 = 0x1;
    pub(super) const SUBREV: u8 = 0x2;
    pub(super) const CMPEQ: u8 = 0x3;

    pub(super) const CMPNEQ: u8 = 0x7;
    pub(super) const ADD_SAT: u8 = 0x8;
    pub(super) const SUB_SAT: u8 = 0x9;
    pub(super) const SUBREV_SAT: u8 = 0xA;
}

mod shift_ops {
    pub(super) const LEFT_SHIFT: u8 = 0b0000;
    pub(super) const LOGICAL_RIGHT_SHIFT: u8 = 0b0001;
    pub(super) const ARITHMETIC_RIGHT_SHIFT: u8 = 0b0010;
    pub(super) const ROTATE_LEFT: u8 = 0b0011;
    pub(super) const ROTATE_RIGHT: u8 = 0b0111;
}

mod bit_ops {
    pub(super) const AND: u8 = 0b1000;
    pub(super) const OR: u8 = 0b1110;
    pub(super) const XOR: u8 = 0b0110;
    pub(super) const NAND: u8 = 0b0111;
    pub(super) const NOR: u8 = 0b0001;
    pub(super) const XNOR: u8 = 0b1001;
    pub(super) const NOT_DST: u8 = 0b0011;
}
