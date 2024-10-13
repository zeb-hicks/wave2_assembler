use std::fmt;

/// the maximum index per type of register
pub const MAX_REG_IDX: u8 = 7;

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Move {
        src: SetRegSelector,
        dst: SetRegSelector,
    },
    Swizzle {
        reg: SwizzleRegSelector,
    },
    Load {
        mem: MemoryOperand,
        dst: SetRegSelector,
    },
    Store {
        src: SetRegSelector,
        mem: MemoryOperand,
    },
    Add {
        size: OpSize,
        lhs: RegSelector,
        rhs: RegSelector,
    },
    AddSaturate {
        size: OpSize,
        lhs: RegSelector,
        rhs: RegSelector,
    },
    /// rhs = lhs - rhs
    Sub {
        size: OpSize,
        lhs: RegSelector,
        rhs: RegSelector,
    },
    /// rhs = lhs - rhs
    SubSaturate {
        size: OpSize,
        lhs: RegSelector,
        rhs: RegSelector,
    },
    /// rhs = rhs - lhs
    SubRev {
        size: OpSize,
        lhs: RegSelector,
        rhs: RegSelector,
    },
    /// rhs = rhs - lhs
    SubRevSaturate {
        size: OpSize,
        lhs: RegSelector,
        rhs: RegSelector,
    },

    CmpEq {
        size: OpSize,
        lhs: RegSelector,
        rhs: RegSelector,
    },
    CmpCarry {
        size: OpSize,
        lhs: RegSelector,
        rhs: RegSelector,
    },
    CmpCarryRev {
        size: OpSize,
        lhs: RegSelector,
        rhs: RegSelector,
    },

    // =================
    // SHIFTS
    // =================
    ShiftLeft {
        size: OpSize,
        dst: RegSelector,
        amount: ShiftAmount,
    },
    ShiftRightLogical {
        size: OpSize,
        dst: RegSelector,
        amount: ShiftAmount,
    },
    ShiftRightArithmetic {
        size: OpSize,
        dst: RegSelector,
        amount: ShiftAmount,
    },
    RotateLeft {
        size: OpSize,
        dst: RegSelector,
        amount: ShiftAmount,
    },
    RotateRight {
        size: OpSize,
        dst: RegSelector,
        amount: ShiftAmount,
    },

    // =================
    // BITOPS
    // =================
    BitAnd {
        src: RegSelector,
        dst: RegSelector,
    },
    BitOr {
        src: RegSelector,
        dst: RegSelector,
    },
    BitXor {
        src: RegSelector,
        dst: RegSelector,
    },
    BitNand {
        src: RegSelector,
        dst: RegSelector,
    },

    BitNor {
        src: RegSelector,
        dst: RegSelector,
    },
    BitXnor {
        src: RegSelector,
        dst: RegSelector,
    },
    UnaryBitNot {
        dst: RegSelector,
    },
    // TODO: System, SpecOp
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpSize {
    Byte,
    Word,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftAmount {
    Register(RegSelector),
    // INVARIANT: 0<= val <= 15
    Const(u8),
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct RegSelector {
    idx: u8,
}

const DATA_IDX_OFFSET: u8 = 8;
impl RegSelector {
    pub fn new_const(idx: u8) -> Self {
        assert!(idx <= MAX_REG_IDX);
        Self { idx }
    }

    pub fn new_gpr(idx: u8) -> Self {
        assert!(idx <= MAX_REG_IDX);
        Self {
            idx: idx + DATA_IDX_OFFSET,
        }
    }

    /// gets the index of the register for codegen
    pub fn idx(&self) -> u8 {
        self.idx
    }
}

impl fmt::Debug for RegSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.idx {
            n @ 0..=7 => write!(f, "c{}", n),
            n @ 8..=14 => write!(f, "r{}", n - DATA_IDX_OFFSET),
            15 => write!(f, "ri"),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct MemoryOperand {
    reg: RegSelector,
    /// true to use each word in the register as an operand, false to use only the first
    scatter: bool,
    increment: bool,
}

impl MemoryOperand {
    pub fn new(reg: RegSelector, scatter: bool, increment: bool) -> Self {
        Self {
            reg,
            scatter,
            increment,
        }
    }

    pub fn reg(&self) -> RegSelector {
        self.reg
    }

    pub fn scatter(&self) -> bool {
        self.scatter
    }

    pub fn increment(&self) -> bool {
        self.increment
    }
}

impl fmt::Debug for MemoryOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:?}", self.reg)?;

        if self.scatter {
            write!(f, ".xyzw]")?;
        } else {
            write!(f, ".x]")?;
        }

        if self.increment {
            write!(f, "+")?;
        }

        Ok(())
    }
}

#[derive(Copy, Clone)]
pub struct SetRegSelector {
    reg: RegSelector,
    selector: SetSelector,
}

impl SetRegSelector {
    pub fn new(reg: RegSelector, selector: SetSelector) -> Self {
        Self { reg, selector }
    }

    pub fn reg(&self) -> RegSelector {
        self.reg
    }

    pub fn selector(&self) -> SetSelector {
        self.selector
    }
}

impl fmt::Debug for SetRegSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}.{:?}", self.reg, self.selector)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
/// bitflags for which elements of a vector are being selected
pub struct SetSelector(u8);

impl SetSelector {
    pub fn empty() -> Self {
        Self(0)
    }

    pub fn from_bits(bits: u8) -> Self {
        assert!(bits <= 0b1111);
        Self(bits)
    }

    /// sets the specified element in the selector, returning whether
    /// that element was already set
    pub fn set(&mut self, idx: u8) -> bool {
        let idx = idx & 0b11;
        let set = self.0 & (1 << idx) != 0;
        self.0 |= 1 << idx;
        set
    }

    /// gets the bits set in the selector
    pub fn bits(&self) -> u8 {
        self.0
    }

    pub fn x(&self) -> bool {
        self.0 & 0b0001 != 0
    }
    pub fn y(&self) -> bool {
        self.0 & 0b0010 != 0
    }
    pub fn z(&self) -> bool {
        self.0 & 0b0100 != 0
    }
    pub fn w(&self) -> bool {
        self.0 & 0b1000 != 0
    }

    /*
    /// gets the number of selected elements in the selector
    fn count(&self) -> u8 {
        self.0.count_ones() as u8
    }
    */
}

impl fmt::Debug for SetSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == 0 {
            return write!(f, "<none>");
        }

        if self.0 & 0b0001 != 0 {
            write!(f, "x")?;
        }
        if self.0 & 0b0010 != 0 {
            write!(f, "y")?;
        }
        if self.0 & 0b0100 != 0 {
            write!(f, "z")?;
        }
        if self.0 & 0b1000 != 0 {
            write!(f, "w")?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
/// an ordered selector of elements from a vector
/// the low 2 bits correspond to the first selected element index and so on
pub struct SwizzleSelector(u8);

impl SwizzleSelector {
    pub fn empty() -> Self {
        Self(0)
    }

    pub fn set(&mut self, offset: u8, selected: u8) {
        let shift = (offset & 0b11) * 2;
        // set the bits at the position to 0, then set them to the correct value
        self.0 &= !(0b11 << shift);
        self.0 |= (selected & 0b11) << shift;
    }

    pub fn bits(&self) -> u8 {
        self.0
    }
}

impl fmt::Debug for SwizzleSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elem_name = |idx: u8| match idx & 0b11 {
            0b00 => 'x',
            0b01 => 'y',
            0b10 => 'z',
            0b11 => 'w',
            _ => unreachable!(),
        };
        write!(
            f,
            "{}{}{}{}",
            elem_name(self.0 & 0b00000011),
            elem_name((self.0 & 0b00001100) >> 2),
            elem_name((self.0 & 0b00110000) >> 4),
            elem_name((self.0 & 0b11000000) >> 6)
        )
    }
}

#[derive(Copy, Clone)]
pub struct SwizzleRegSelector {
    reg: RegSelector,
    selector: SwizzleSelector,
}

impl SwizzleRegSelector {
    pub fn new(reg: RegSelector, selector: SwizzleSelector) -> Self {
        Self { reg, selector }
    }

    pub fn reg(&self) -> RegSelector {
        self.reg
    }

    pub fn selector(&self) -> SwizzleSelector {
        self.selector
    }
}

impl fmt::Debug for SwizzleRegSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}.{:?}", self.reg, self.selector)
    }
}
