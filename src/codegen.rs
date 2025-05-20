use std::collections::HashMap;

use crate::{diag::Diagnostic, instruction::{Instruction, InstructionKind, OpSize, RegSelector, ShiftAmount}, lexer::Span};

pub fn gen(insts: &[Instruction]) -> Result<Vec<u16>, Diagnostic> {
    // insts.iter().flat_map(|i| gen_inst(*i)).collect()
    let mut code = Vec::new();
    let mut gis: Vec<(u16, GI)> = Vec::new();
    let mut addr = 0x40;

    let mut labels: HashMap<u64, u16> = HashMap::new();

    // println!("Instructions: {:#?}", insts);

    for inst in insts.iter() {
        let span = inst.span();
        let inst_code = gen_inst(inst.clone());

        // Collect all the labels
        for inst in inst_code.iter() {
            gis.push((addr, inst.clone()));
            match inst {
                GI::Label(label) => {
                    if labels.contains_key(label) {
                        return Err(Diagnostic::new(
                            format!("Duplicate label: {}", label),
                            span,
                        ));
                    }
                    labels.insert(*label, addr);
                },
                GI::JumpToLabel(_) => {
                    addr += 2;
                },
                GI::SetToLabel(_, _) => {
                    addr += 2;
                },
                _ => {
                    addr += 1;
                }
            }
        }

        // code.append(&mut inst_code);
        // addr += inst_code.len() as u16;
    }

    for (_addr, inst) in gis.iter() {
        match inst {
            GI::JumpToLabel(label) => {
                if let Some(addr) = labels.get(label) {
                    code.push(op_from_parts(
                        0xf,
                        0xf,
                        0x6,
                        opcode::MOVE,
                    ));
                    code.push(*addr);
                } else {
                    return Err(Diagnostic::new(
                        format!("Undefined label: {}", label),
                        Span::DUMMY,
                    ));
                }
            },
            GI::SetToLabel(dst, label) => {
                if let Some(addr) = labels.get(label) {
                    code.push(op_from_parts(
                        *dst,
                        0xf,
                        0x6,
                        opcode::MOVE,
                    ));
                    code.push(*addr);
                } else {
                    return Err(Diagnostic::new(
                        format!("Undefined label: {}", label),
                        Span::DUMMY,
                    ));
                }
            },
            GI::Instruction(inst) => {
                code.push(*inst);
            },
            _ => {}
        }
    }

    Ok(code)
}

#[derive(Debug, Clone)]
pub enum GI {
    Instruction(u16),
    Label(u64),
    JumpToLabel(u64),
    SetToLabel(u8, u64),
}

fn gen_inst(inst: Instruction) -> Vec<GI> {
    use InstructionKind::*;
    match *inst.kind() {
        Label { label } => {
            vec![GI::Label(label)]
        },
        LabelJump { label } => {
            vec![GI::JumpToLabel(label)]
        },
        LabelSet { dst, label } => {
            vec![GI::SetToLabel(dst.idx(), label)]
        },
        Nop => {
            vec![GI::Instruction(op_from_parts(
                0,
                0,
                1,
                opcode::SYSTEM,
            ))]
        }
        Halt => {
            vec![GI::Instruction(op_from_parts(
                0,
                0,
                0,
                opcode::SYSTEM,
            ))]
        }
        Sleep { ticks } => {
            vec![GI::Instruction(op_from_parts(
                0,
                ticks,
                1,
                opcode::SYSTEM,
            ))]
        }
        Sleep8L { src } => {
            vec![GI::Instruction(op_from_parts(
                1,
                src.idx(),
                1,
                opcode::SYSTEM,
            ))]
        }
        Sleep8H { src } => {
            vec![GI::Instruction(op_from_parts(
                2,
                src.idx(),
                1,
                opcode::SYSTEM,
            ))]
        }
        Sleep16 { src } => {
            vec![GI::Instruction(op_from_parts(
                3,
                src.idx(),
                1,
                opcode::SYSTEM,
            ))]
        }
        Move { src, dst } => {
            // place a 1 bit everywhere we want to *not* move
            let skip_mask = src.selector().bits() ^ 0b1111;
            vec![GI::Instruction(op_from_parts(
                dst.reg().idx(),
                src.reg().idx(),
                skip_mask,
                opcode::MOVE,
            ))]
        }
        Swizzle { reg } => {
            let bits = reg.selector().bits();
            let source = bits >> 4;
            let extra = bits & 0b1111;
            vec![GI::Instruction(op_from_parts(
                reg.reg().idx(),
                source,
                extra,
                opcode::SWIZZLE,
            ))]
        }
        Load { mem, dst } => {
            let size = 4 - dst.selector().bits().count_ones() as u8;
            let scatter = u8::from(mem.scatter());
            let increment = u8::from(mem.increment());
            let extra = (size << 2) | (scatter << 1) | increment;
            vec![GI::Instruction(op_from_parts(
                dst.reg().idx(),
                mem.reg().idx(),
                extra,
                opcode::LOAD,
            ))]
        }
        Store { src, mem } => {
            let size = 4 - src.selector().bits().count_ones() as u8;
            let scatter = u8::from(mem.scatter());
            let increment = u8::from(mem.increment());
            let extra = (size << 2) | (scatter << 1) | increment;
            // this uses src as the dest operand and mem as the source operand because
            // both load and store use the source as an address and dest as a value
            // https://github.com/Meisaka/MeiVM2/blob/cd687f44a11bc3a0f318dcb1badb23f1f8dce44f/vm.txt#L64-L65
            vec![GI::Instruction(op_from_parts(
                src.reg().idx(),
                mem.reg().idx(),
                extra,
                opcode::STORE,
            ))]
        }
        WAdd { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.reg().idx(),
            wselect_ops::WADD | src.selector().bits() << 2,
            opcode::WSELECT,
        ))],
        WSub { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.reg().idx(),
            wselect_ops::WSUB | src.selector().bits() << 2,
            opcode::WSELECT,
        ))],
        WSwap { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.reg().idx(),
            wselect_ops::WSWAP | src.selector().bits() << 2,
            opcode::WSELECT,
        ))],
        WMove { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.reg().idx(),
            wselect_ops::WMOVE | src.selector().bits() << 2,
            opcode::WSELECT,
        ))],
        Add { size, src, dst } => vec![GI::Instruction(math_op(math_ops::ADD, size, src, dst))],
        Sub { size, src, dst } => vec![GI::Instruction(math_op(math_ops::SUB, size, src, dst))],
        RSub { size, src, dst } => vec![GI::Instruction(math_op(math_ops::RSUB, size, src, dst))],
        Eq { size, src, dst } => vec![GI::Instruction(math_op(math_ops::EQ, size, src, dst))],
        Carry { size, src, dst } => vec![GI::Instruction(math_op(math_ops::CARRY, size, src, dst))],
        LessU { size, src, dst } => vec![GI::Instruction(math_op(math_ops::LESS_U, size, src, dst))],
        GreaterU { size, src, dst } => vec![GI::Instruction(math_op(math_ops::GREATER_U, size, src, dst))],
        NotEq { size, src, dst } => vec![GI::Instruction(math_op(math_ops::NOTEQ, size, src, dst))],
        AddSaturate { size, src, dst } => vec![GI::Instruction(math_op(math_ops::ADD_SAT, size, src, dst))],
        SubSaturate { size, src, dst } => vec![GI::Instruction(math_op(math_ops::SUB_SAT, size, src, dst))],
        SubRevSaturate { size, src, dst } => vec![GI::Instruction(math_op(math_ops::RSUB_SAT, size, src, dst))],
        GreaterEqU { size, src, dst } => vec![GI::Instruction(math_op(math_ops::GREATER_EQ_U, size, src, dst))],
        AddOver { size, src, dst } => vec![GI::Instruction(math_op(math_ops::ADD_OVER, size, src, dst))],
        SubOver { size, src, dst } => vec![GI::Instruction(math_op(math_ops::SUB_OVER, size, src, dst))],
        RSubOver { size, src, dst } => vec![GI::Instruction(math_op(math_ops::RSUB_OVER, size, src, dst))],
        LessEqU { size, src, dst } => vec![GI::Instruction(math_op(math_ops::LESS_EQ_U, size, src, dst))],
        HorizontalAdd { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            spec_ops::HORIZONTAL_ADD,
            opcode::SPECOP,
        ))],
        MultiplySaturate { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            spec_ops::MULTIPLY_SAT,
            opcode::SPECOP,
        ))],
        MultiplyLow { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            spec_ops::MULTIPLY_LOW,
            opcode::SPECOP,
        ))],
        MultiplyHigh { src, dst }  => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            spec_ops::MULTIPLY_HIGH,
            opcode::SPECOP,
        ))],
        Divide { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            spec_ops::DIVIDE,
            opcode::SPECOP,
        ))],
        ReciprocalDivide { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            spec_ops::RECIPROCAL_DIVIDE,
            opcode::SPECOP,
        ))],
        ShiftLeft { size, dst, amount } => vec![GI::Instruction(shift_op(shift_ops::LEFT_SHIFT, size, dst, amount))],
        ShiftRightLogical { size, dst, amount } => {
            vec![GI::Instruction(shift_op(shift_ops::LOGICAL_RIGHT_SHIFT, size, dst, amount))]
        }
        ShiftRightArithmetic { size, dst, amount } => {
            vec![GI::Instruction(shift_op(
                shift_ops::ARITHMETIC_RIGHT_SHIFT,
                size,
                dst,
                amount,
            ))]
        }
        RotateLeft { size, dst, amount } => {
            vec![GI::Instruction(shift_op(shift_ops::ROTATE_LEFT, size, dst, amount))]
        }
        RotateRight { size, dst, amount } => {
            vec![GI::Instruction(shift_op(shift_ops::ROTATE_RIGHT, size, dst, amount))]
        }
        And { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::AND,
            opcode::BITOP,
        ))],
        Or { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::OR,
            opcode::BITOP,
        ))],
        Xor { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::XOR,
            opcode::BITOP,
        ))],
        Nand { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::NAND,
            opcode::BITOP,
        ))],
        Nor { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::NOR,
            opcode::BITOP,
        ))],
        XNor { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::XNOR,
            opcode::BITOP,
        ))],
        NotDst { dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            0,
            bit_ops::NOT_DST,
            opcode::BITOP
        ))],
        NotSrc { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::NOT_SRC,
            opcode::BITOP
        ))],
        SrcAndNotDst { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::SRC_AND_NOT_DST,
            opcode::BITOP
        ))],
        NotSrcAndDst { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::NOT_SRC_AND_DST,
            opcode::BITOP
        ))],
        SrcOrNotDst { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::SRC_OR_NOT_DST,
            opcode::BITOP
        ))],
        NotSrcOrDst { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::NOT_SRC_OR_DST,
            opcode::BITOP
        ))],
        All { dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            0,
            bit_ops::ALL,
            opcode::BITOP
        ))],
        One { dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            0,
            bit_ops::ONE,
            opcode::BITOP
        ))],
        Swap { src, dst } => vec![GI::Instruction(op_from_parts(
            dst.idx(),
            src.idx(),
            bit_ops::SWAP,
            opcode::BITOP,
        ))],
        Raw { val } => vec![GI::Instruction(val)],
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
    pub(super) const SPECOP: u8 = 0b1101;
    /// TODO: implement
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
    pub(super) const RSUB: u8 = 0x2;
    pub(super) const EQ: u8 = 0x3;
    pub(super) const CARRY: u8 = 0x4;
    pub(super) const LESS_U: u8 = 0x5;
    pub(super) const GREATER_U: u8 = 0x6;
    pub(super) const NOTEQ: u8 = 0x7;
    pub(super) const ADD_SAT: u8 = 0x8;
    pub(super) const SUB_SAT: u8 = 0x9;
    pub(super) const RSUB_SAT: u8 = 0xA;
    pub(super) const GREATER_EQ_U: u8 = 0xB;
    pub(super) const ADD_OVER: u8 = 0xC;
    pub(super) const SUB_OVER: u8 = 0xD;
    pub(super) const RSUB_OVER: u8 = 0xE;
    pub(super) const LESS_EQ_U: u8 = 0xF;
}

mod shift_ops {
    pub(super) const LEFT_SHIFT: u8 = 0b0000;
    pub(super) const LOGICAL_RIGHT_SHIFT: u8 = 0b0001;
    pub(super) const ARITHMETIC_RIGHT_SHIFT: u8 = 0b0010;
    pub(super) const ROTATE_LEFT: u8 = 0b0011;
    pub(super) const ROTATE_RIGHT: u8 = 0b0111;
}

mod bit_ops {
    // pub(super) const AND: u8 = 0b1000;
    // pub(super) const OR: u8 = 0b1110;
    // pub(super) const XOR: u8 = 0b0110;
    // pub(super) const NAND: u8 = 0b0111;
    // pub(super) const NOR: u8 = 0b0001;
    // pub(super) const XNOR: u8 = 0b1001;
    // pub(super) const NOT_DST: u8 = 0b0011;

    pub(super) const ONE: u8 = 0x0;
    pub(super) const ALL: u8 = 0xf;

    pub(super) const AND: u8 = 0x8;
    pub(super) const OR:  u8 = 0xe;
    pub(super) const XOR: u8 = 0x6;

    pub(super) const NAND: u8 = 0x7;
    pub(super) const NOR:  u8 = 0x1;
    pub(super) const XNOR: u8 = 0x9;

    pub(super) const SWAP: u8 = 0xA;

    pub(super) const NOT_SRC: u8 = 0x5;
    pub(super) const NOT_DST: u8 = 0x3;

    pub(super) const SRC_AND_NOT_DST: u8 = 0x2;
    pub(super) const NOT_SRC_AND_DST: u8 = 0x4;
    pub(super) const SRC_OR_NOT_DST: u8 = 0xB;
    pub(super) const NOT_SRC_OR_DST: u8 = 0xD;
}

mod spec_ops {
    pub(super) const HORIZONTAL_ADD: u8 = 0b0000;
    pub(super) const MULTIPLY_SAT: u8 = 0b0001;
    pub(super) const MULTIPLY_LOW: u8 = 0b0010;
    pub(super) const MULTIPLY_HIGH: u8 = 0b0011;
    pub(super) const DIVIDE: u8 = 0b0100;
    pub(super) const RECIPROCAL_DIVIDE: u8 = 0b0101;
}
