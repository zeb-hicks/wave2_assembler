use std::fmt;

use crate::lexer::Span;

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    kind: InstructionKind,
    span: Span,
}

impl Instruction {
    pub fn new(kind: InstructionKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &InstructionKind {
        &self.kind
    }

    #[allow(dead_code)]
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Copy)]
pub enum InstructionKind {
    Nop,
    Halt,
    Sleep { ticks: u8, },
    Sleep8L { src: RegSelector, },
    Sleep8H { src: RegSelector, },
    Sleep16 { src: RegSelector, },
    Move { src: SetRegSelector, dst: SetRegSelector, },
    Swizzle { reg: SwizzleRegSelector, },
    Load { mem: MemoryOperand, dst: SetRegSelector, },
    Store { src: SetRegSelector, mem: MemoryOperand, },
    /// dst.x = src.*
    WMove { src: SwizzleRegSelector, dst: RegSelector, },
    /// dst.x <> src.*
    WSwap { src: SwizzleRegSelector, dst: RegSelector, },
    /// dst.x = dst.x + src.*
    WAdd { src: SwizzleRegSelector, dst: RegSelector, },
    /// dst.x = dst.x - src.*
    WSub { src: SwizzleRegSelector, dst: RegSelector, },

    /// dst = src + dst
    Add { size: OpSize, src: RegSelector, dst: RegSelector, },
    /// dst = src - dst
    Sub { size: OpSize, src: RegSelector, dst: RegSelector, },
    /// dst = dst - src
    RSub { size: OpSize, src: RegSelector, dst: RegSelector, },
    /// dst = src == dst
    Eq { size: OpSize, src: RegSelector, dst: RegSelector, },
    /// dst = Carry(src + dst)
    Carry { size: OpSize, src: RegSelector, dst: RegSelector, },
    /// dst = Carry(src - dst)
    LessU { size: OpSize, src: RegSelector, dst: RegSelector, },
    /// dst = Carry(dst - src)
    GreaterU { size: OpSize, src: RegSelector, dst: RegSelector, },
    /// dst = src != dst
    NotEq { size: OpSize, src: RegSelector, dst: RegSelector, },
    /// dst = src + dst
    AddSaturate { size: OpSize, src: RegSelector, dst: RegSelector, },
    /// dst = src - dst
    SubSaturate { size: OpSize, src: RegSelector, dst: RegSelector, },
    /// dst = dst - src
    SubRevSaturate { size: OpSize, src: RegSelector, dst: RegSelector, },
    // dst = Carry(src - dest)
    GreaterEqU { size: OpSize, src: RegSelector, dst: RegSelector, },
    // dst = Over(src + dest)
    AddOver { size: OpSize, src: RegSelector, dst: RegSelector, },
    // dst = Over(src - dest)
    SubOver { size: OpSize, src: RegSelector, dst: RegSelector, },
    // dst = Over(dest - src)
    RSubOver { size: OpSize, src: RegSelector, dst: RegSelector, },
    // dst = Carry(dest - src)
    LessEqU { size: OpSize, src: RegSelector, dst: RegSelector, },

    // =================
    // SHIFTS
    // =================
    ShiftLeft { size: OpSize, dst: RegSelector, amount: ShiftAmount, },
    ShiftRightLogical { size: OpSize, dst: RegSelector, amount: ShiftAmount, },
    ShiftRightArithmetic { size: OpSize, dst: RegSelector, amount: ShiftAmount, },
    RotateLeft { size: OpSize, dst: RegSelector, amount: ShiftAmount, },
    RotateRight { size: OpSize, dst: RegSelector, amount: ShiftAmount, },

    // =================
    // SPEC OPS
    // =================
    HorizontalAdd { src: RegSelector, dst: RegSelector, },
    MultiplySaturate { src: RegSelector, dst: RegSelector, },
    MultiplyLow { src: RegSelector, dst: RegSelector, },
    MultiplyHigh { src: RegSelector, dst: RegSelector, },
    Divide { src: RegSelector, dst: RegSelector, },
    ReciprocalDivide { src: RegSelector, dst: RegSelector, },

    // =================
    // BITOPS
    // =================
    All { dst: RegSelector, },
    One { dst: RegSelector, },
    Swap { src: RegSelector, dst: RegSelector, },
    NotSrc { src: RegSelector, dst: RegSelector, },
    NotDst { dst: RegSelector, },
    SrcAndNotDst { src: RegSelector, dst: RegSelector, },
    NotSrcAndDst { src: RegSelector, dst: RegSelector, },
    SrcOrNotDst { src: RegSelector, dst: RegSelector, },
    NotSrcOrDst { src: RegSelector, dst: RegSelector, },
    And { src: RegSelector, dst: RegSelector, },
    Or { src: RegSelector, dst: RegSelector, },
    Xor { src: RegSelector, dst: RegSelector, },
    Nand { src: RegSelector, dst: RegSelector, },
    Nor { src: RegSelector, dst: RegSelector, },
    XNor { src: RegSelector, dst: RegSelector, },

    LabelDefinition { label: u64 },
    LabelJump { label: u64 },
    LabelLiteral { label: u64 },

    Raw { val: u16, }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpSize {
    Byte,
    Word,
}

#[derive(Debug, Clone, Copy)]
pub enum ShiftAmount {
    Register(RegSelector),
    // INVARIANT: 0<= val <= 15
    Const(u8, Span),
}

impl ShiftAmount {
    pub fn span(&self) -> Span {
        match self {
            ShiftAmount::Register(reg_selector) => reg_selector.span(),
            ShiftAmount::Const(_, span) => *span,
        }
    }
}

#[derive(Copy, Clone)]
pub struct RegSelector {
    idx: u8,
    span: Span,
}

/// the maximum index per type of register
pub const MAX_REG_IDX: u8 = 7;
const DATA_IDX_OFFSET: u8 = 8;
impl RegSelector {
    pub fn new_const(idx: u8, span: Span) -> Self {
        assert!(idx <= MAX_REG_IDX);
        Self { idx, span }
    }

    pub fn new_gpr(idx: u8, span: Span) -> Self {
        assert!(idx <= MAX_REG_IDX);
        Self {
            idx: idx + DATA_IDX_OFFSET,
            span,
        }
    }

    /// gets the index of the register for codegen
    pub fn idx(&self) -> u8 {
        self.idx
    }

    pub fn span(&self) -> Span {
        self.span
    }

    /// returns `true` if the register is a const register, otherwise `false``.
    #[allow(dead_code)]
    pub fn is_const(&self) -> bool {
        self.idx <= MAX_REG_IDX
    }

    /// returns `true` if the register is a general purpose register, otherwise `false`.
    /// this method considers `ri` (`r15`) to be a general purpose register
    /// because it is writable.
    pub fn is_gpr(&self) -> bool {
        MAX_REG_IDX < self.idx && self.idx <= DATA_IDX_OFFSET + MAX_REG_IDX
    }
}

impl fmt::Debug for RegSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RegSelector")
            .field(
                "reg",
                &match self.idx {
                    n @ 0..=7 => format!("c{n}"),
                    n @ 8..=14 => format!("r{}", n - DATA_IDX_OFFSET),
                    #[allow(clippy::useless_format)]
                    15 => format!("ri"),
                    _ => unreachable!(),
                },
            )
            .field("span", &self.span)
            .finish()
    }
}

impl fmt::Display for RegSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.idx {
            n @ 0..=7 => write!(f, "c{n}"),
            n @ 8..=14 => write!(f, "r{}", n - DATA_IDX_OFFSET),
            15 => write!(f, "ri"),
            _ => unreachable!(),
        }
    }
}

impl PartialEq for RegSelector {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl Eq for RegSelector {}

#[derive(Copy, Clone)]
/// bitflags for which elements of a vector are being selected
pub struct SetSelector(u8, Span);

impl SetSelector {
    pub fn empty(span: Span) -> Self {
        Self(0, span)
    }

    pub fn from_bits(bits: u8, span: Span) -> Self {
        assert!(bits <= 0b1111);
        Self(bits, span)
    }

    /// sets the specified element in the selector, returning whether
    /// that element was already set
    pub fn set(&mut self, idx: u8) -> bool {
        assert!(idx < 4);
        let set = self.0 & (1 << idx) != 0;
        self.0 |= 1 << idx;
        set
    }

    /// gets the bits set in the selector
    pub fn bits(&self) -> u8 {
        self.0
    }

    pub fn span(&self) -> Span {
        self.1
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

impl PartialEq for SetSelector {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for SetSelector {}

impl fmt::Debug for SetSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SetSelector")
            .field_with("selector", |f| {
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
            })
            .field("span", &self.span())
            .finish()
    }
}

#[derive(Copy, Clone)]
/// an ordered selector of elements from a vector
/// the low 2 bits correspond to the first selected element index and so on
pub struct SwizzleSelector(u8, Span);

impl SwizzleSelector {
    pub fn new(bits: u8, span: Span) -> Self {
        Self(bits, span)
    }

    pub fn empty(span: Span) -> Self {
        Self(0, span)
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

    pub fn span(&self) -> Span {
        self.1
    }
}

impl PartialEq for SwizzleSelector {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for SwizzleSelector {}

impl fmt::Debug for SwizzleSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SwizzleSelector")
            .field("selector", {
                let elem_name = |idx: u8| match idx & 0b11 {
                    0b00 => 'x',
                    0b01 => 'y',
                    0b10 => 'z',
                    0b11 => 'w',
                    _ => unreachable!(),
                };
                &format_args!(
                    "{}{}{}{}",
                    elem_name(self.0 & 0b00000011),
                    elem_name((self.0 & 0b00001100) >> 2),
                    elem_name((self.0 & 0b00110000) >> 4),
                    elem_name((self.0 & 0b11000000) >> 6)
                )
            })
            .field("span", &self.span())
            .finish()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct MemoryOperand {
    reg: RegSelector,
    /// true to use each word in the register as an operand, false to use only the first
    scatter: bool,
    increment: bool,
    span: Span,
}

impl MemoryOperand {
    pub fn new(reg: RegSelector, scatter: bool, increment: bool, span: Span) -> Self {
        Self {
            reg,
            scatter,
            increment,
            span,
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

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Copy, Clone, Debug)]
pub struct SetRegSelector {
    reg: RegSelector,
    selector: SetSelector,
    span: Span,
}

impl SetRegSelector {
    pub fn new(reg: RegSelector, selector: SetSelector, span: Span) -> Self {
        Self {
            reg,
            selector,
            span,
        }
    }

    pub fn reg(&self) -> RegSelector {
        self.reg
    }

    pub fn selector(&self) -> SetSelector {
        self.selector
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Copy, Clone, Debug)]
pub struct SwizzleRegSelector {
    reg: RegSelector,
    selector: SwizzleSelector,
    span: Span,
}

impl SwizzleRegSelector {
    pub fn new(reg: RegSelector, selector: SwizzleSelector, span: Span) -> Self {
        Self {
            reg,
            selector,
            span,
        }
    }

    pub fn reg(&self) -> RegSelector {
        self.reg
    }

    pub fn selector(&self) -> SwizzleSelector {
        self.selector
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
