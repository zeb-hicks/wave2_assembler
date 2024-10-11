use color_eyre::Section;
use eyre::{bail, eyre, Context};
use log::*;

use crate::{
    instruction::{
        Instruction, MemoryOperand, OpSize, RegSelector, SetRegSelector, SetSelector, ShiftAmount,
        SwizzleRegSelector, SwizzleSelector, MAX_REG_IDX,
    },
    lexer::{Lexer, Token, TokenKind},
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut lexer = Lexer::new(src);
        let current = lexer.next_token();
        Self { lexer, current }
    }

    pub fn parse_inst(&mut self) -> eyre::Result<Option<Instruction>> {
        if self.current.kind() == &TokenKind::EoF {
            return Ok(None);
        }

        let inst = self.expect_ident()?;

        let inst = match inst.to_lowercase().as_str() {
            "mov" => self.parse_move()?,
            "store" => self.parse_store()?,
            "load" => self.parse_load()?,
            "swizzle" => self.parse_swizzle()?,
            "add" => self.parse_add(AddMode::Normal)?,
            "add_sat" => self.parse_add(AddMode::Saturate)?,
            "sub" => self.parse_sub(SubMode::Normal)?,
            "sub_sat" => self.parse_sub(SubMode::Saturate)?,
            "subrev" => self.parse_sub(SubMode::RevNormal)?,
            "subrev_sat" => self.parse_sub(SubMode::RevSaturate)?,
            "cmpeq" => self.parse_cmp(CmpMode::Eq)?,
            "cmpc" => self.parse_cmp(CmpMode::Carry)?,
            "cmpc_rev" => self.parse_cmp(CmpMode::RevCarry)?,
            "lsl" | "asl" => self.parse_lsl()?,
            "rol" => self.parse_rol()?,
            "asr" => self.parse_asr()?,
            "lsr" => self.parse_lsr()?,
            "ror" => self.parse_ror()?,
            "and" => self.parse_and()?,
            "or" => self.parse_or()?,
            "xor" => self.parse_xor()?,
            "nand" => self.parse_nand()?,
            "nor" => self.parse_nor()?,
            "xnor" => self.parse_xnor()?,
            "not" => self.parse_unary_not()?,
            _ => bail!("invalid instruction `{}`", inst),
        };
        Ok(Some(inst))
    }

    fn parse_move(&mut self) -> eyre::Result<Instruction> {
        self.bump();
        let src = self.parse_set_reg(false)?;
        if !self.eat(&TokenKind::Comma) {
            bail!("expected a , between src and dst");
        }
        let dst = self.parse_set_reg(false)?;

        if dst.selector() != src.selector() {
            bail!("src and dst for move must select the same elements");
        }

        Ok(Instruction::Move { dst, src })
    }

    fn parse_store(&mut self) -> eyre::Result<Instruction> {
        self.bump();
        let src = self.parse_set_reg(true)?;

        if !self.eat(&TokenKind::Comma) {
            bail!("missing comma between src and dst");
        }
        let mem = self.parse_mem_operand()?;

        Ok(Instruction::Store { src, mem })
    }

    fn parse_load(&mut self) -> eyre::Result<Instruction> {
        self.bump();

        let mem = self.parse_mem_operand()?;
        if !self.eat(&TokenKind::Comma) {
            bail!("missing comma between src and dst");
        }
        let dst = self.parse_set_reg(true)?;

        Ok(Instruction::Load { mem, dst })
    }

    fn parse_swizzle(&mut self) -> eyre::Result<Instruction> {
        self.bump();

        let reg = self.parse_swizzle_reg()?;
        Ok(Instruction::Swizzle { reg })
    }

    fn parse_add(&mut self, mode: AddMode) -> eyre::Result<Instruction> {
        let (size, lhs, rhs) = self.parse_math_common()?;
        let inst = match mode {
            AddMode::Normal => Instruction::Add { size, lhs, rhs },
            AddMode::Saturate => Instruction::AddSaturate { size, lhs, rhs },
            AddMode::CheckCarry => todo!("add check carry"),
            AddMode::CheckSign => todo!("add check sign"),
        };
        Ok(inst)
    }

    fn parse_sub(&mut self, mode: SubMode) -> eyre::Result<Instruction> {
        let (size, lhs, rhs) = self.parse_math_common()?;
        let inst = match mode {
            SubMode::Normal => Instruction::Sub { size, lhs, rhs },
            SubMode::Saturate => Instruction::SubSaturate { size, lhs, rhs },
            // reverse the lhs and rhs: subrev.w rhs, lhs
            SubMode::RevNormal => Instruction::SubRev {
                size,
                lhs: rhs,
                rhs: lhs,
            },
            SubMode::RevSaturate => Instruction::SubRevSaturate {
                size,
                lhs: rhs,
                rhs: lhs,
            },
        };
        Ok(inst)
    }

    fn parse_cmp(&mut self, mode: CmpMode) -> eyre::Result<Instruction> {
        let (size, lhs, rhs) = self.parse_math_common()?;
        let inst = match mode {
            CmpMode::Eq => Instruction::CmpEq { size, lhs, rhs },
            CmpMode::Carry => Instruction::CmpCarry { size, lhs, rhs },
            // reverse parses cmpc_rev.w rhs, lhs
            CmpMode::RevCarry => Instruction::CmpCarryRev {
                size,
                lhs: rhs,
                rhs: lhs,
            },
        };
        Ok(inst)
    }

    fn parse_lsl(&mut self) -> eyre::Result<Instruction> {
        let (size, dst, amount) = self.parse_shift_common()?;
        Ok(Instruction::ShiftLeft { size, dst, amount })
    }

    fn parse_rol(&mut self) -> eyre::Result<Instruction> {
        let (size, dst, amount) = self.parse_shift_common()?;
        Ok(Instruction::RotateLeft { size, dst, amount })
    }

    fn parse_lsr(&mut self) -> eyre::Result<Instruction> {
        let (size, dst, amount) = self.parse_shift_common()?;
        Ok(Instruction::ShiftRightLogical { size, dst, amount })
    }

    fn parse_asr(&mut self) -> eyre::Result<Instruction> {
        let (size, dst, amount) = self.parse_shift_common()?;
        Ok(Instruction::ShiftRightArithmetic { size, dst, amount })
    }

    fn parse_ror(&mut self) -> eyre::Result<Instruction> {
        let (size, dst, amount) = self.parse_shift_common()?;
        Ok(Instruction::RotateRight { size, dst, amount })
    }

    fn parse_and(&mut self) -> eyre::Result<Instruction> {
        let (src, dst) = self.parse_bitop_common()?;
        Ok(Instruction::BitAnd { src, dst })
    }

    fn parse_or(&mut self) -> eyre::Result<Instruction> {
        let (src, dst) = self.parse_bitop_common()?;
        Ok(Instruction::BitOr { src, dst })
    }

    fn parse_xor(&mut self) -> eyre::Result<Instruction> {
        let (src, dst) = self.parse_bitop_common()?;
        Ok(Instruction::BitXor { src, dst })
    }

    fn parse_nand(&mut self) -> eyre::Result<Instruction> {
        let (src, dst) = self.parse_bitop_common()?;
        Ok(Instruction::BitNand { src, dst })
    }

    fn parse_nor(&mut self) -> eyre::Result<Instruction> {
        let (src, dst) = self.parse_bitop_common()?;
        Ok(Instruction::BitNor { src, dst })
    }

    fn parse_xnor(&mut self) -> eyre::Result<Instruction> {
        let (src, dst) = self.parse_bitop_common()?;
        Ok(Instruction::BitXnor { src, dst })
    }

    fn parse_unary_not(&mut self) -> eyre::Result<Instruction> {
        self.bump();
        let dst = self.parse_reg()?;
        Ok(Instruction::UnaryBitNot { dst })
    }

    // =======================
    // utilities
    // =======================

    fn parse_math_common(&mut self) -> eyre::Result<(OpSize, RegSelector, RegSelector)> {
        self.bump();

        if !self.eat(&TokenKind::Dot) {
            bail!("math operands need a `.b` or `.w` to specify size");
        }
        let size = match self
            .expect_ident()
            .note("math operands need a `.b` or `.w` to specify size")?
            .as_str()
        {
            "b" => OpSize::Byte,
            "w" => OpSize::Word,
            _ => bail!("math operands need a `.b` or `.w` to specify size"),
        };
        self.bump();

        let lhs = self.parse_reg()?;
        if !self.eat(&TokenKind::Comma) {
            bail!("missing comma between lhs and rhs");
        }
        let rhs = self.parse_reg()?;
        Ok((size, lhs, rhs))
    }

    fn parse_shift_common(&mut self) -> eyre::Result<(OpSize, RegSelector, ShiftAmount)> {
        self.bump();

        if !self.eat(&TokenKind::Dot) {
            bail!("math operands need a `.b` or `.w` to specify size");
        }
        let size = match self
            .expect_ident()
            .note("math operands need a `.b` or `.w` to specify size")?
            .as_str()
        {
            "b" => OpSize::Byte,
            "w" => OpSize::Word,
            _ => bail!("math operands need a `.b` or `.w` to specify size"),
        };
        self.bump();

        // shifts are odd in that they are dst = dst <shift> amount
        let dst = self.parse_reg()?;

        if !self.eat(&TokenKind::Comma) {
            bail!("missing comma between lhs and rhs");
        }

        let amount = match self.current.kind() {
            TokenKind::Ident(_) => ShiftAmount::Register(self.parse_reg()?),
            TokenKind::Number(num) => {
                let num = *num;
                self.bump();

                if num > 0b1111 {
                    bail!("shift amount must not be greater than 15");
                }
                ShiftAmount::Const(num as u8)
            }
            _ => bail!("expected a register or constant number as a shift amount"),
        };

        Ok((size, dst, amount))
    }

    fn parse_bitop_common(&mut self) -> eyre::Result<(RegSelector, RegSelector)> {
        self.bump();

        let lhs = self.parse_reg()?;
        if !self.eat(&TokenKind::Comma) {
            bail!("missing comma between lhs and rhs");
        }
        let rhs = self.parse_reg()?;
        Ok((lhs, rhs))
    }

    fn parse_mem_operand(&mut self) -> eyre::Result<MemoryOperand> {
        if !self.eat(&TokenKind::LeftBracket) {
            bail!("missing `[` before memory operand");
        }
        let set = self.parse_set_reg(true)?;

        let selector = set.selector();
        let x_only = selector.x() && !selector.y() && !selector.z() && !selector.w();
        let all = selector.x() && selector.y() && selector.z() && selector.w();
        if !(x_only || all) {
            bail!("memory operands must use either reg.x or reg.xyzw");
        }

        if !self.eat(&TokenKind::RightBracket) {
            bail!("missing `]` after memory operand");
        }

        let increment = self.eat(&TokenKind::Plus);

        Ok(MemoryOperand::new(set.reg(), x_only, increment))
    }

    fn parse_reg(&mut self) -> eyre::Result<RegSelector> {
        let name = self.expect_ident()?;
        let reg = if name.starts_with('r') {
            let idx = &name[1..];
            let idx = match idx.parse::<u8>() {
                Ok(idx) if idx <= MAX_REG_IDX => idx,
                _ => bail!("invalid register index {}", idx),
            };
            RegSelector::new_gpr(idx)
        } else if name.starts_with('c') {
            let idx = &name[1..];
            let idx = match idx.parse::<u8>() {
                Ok(idx) if idx <= MAX_REG_IDX => idx,
                _ => bail!("invalid register index {}", idx),
            };
            RegSelector::new_const(idx)
        } else {
            bail!("invalid register name {}", name)
        };
        self.bump();
        Ok(reg)
    }

    fn parse_set_reg(&mut self, seq: bool) -> eyre::Result<SetRegSelector> {
        let reg = self.parse_reg()?;
        if !self.eat(&TokenKind::Dot) {
            bail!("expected a `.` after a register name");
        }
        let selector = self.parse_selector(seq)?;

        Ok(SetRegSelector::new(reg, selector))
    }

    fn parse_swizzle_reg(&mut self) -> eyre::Result<SwizzleRegSelector> {
        let reg = self.parse_reg()?;
        if !self.eat(&TokenKind::Dot) {
            bail!("expected a `.` after a register name");
        }
        let selector = self.parse_swizzle_selector()?;

        Ok(SwizzleRegSelector::new(reg, selector))
    }

    fn parse_selector(&mut self, seq: bool) -> eyre::Result<SetSelector> {
        let select_str = self.expect_ident()?.to_ascii_lowercase();
        self.bump();
        if select_str.len() > 4 {
            bail!("too many selectors in {}", select_str);
        }

        let selector = if seq {
            match select_str.as_str() {
                "xyzw" => SetSelector::from_bits(0b1111),
                "xyz" => SetSelector::from_bits(0b0111),
                "xy" => SetSelector::from_bits(0b0011),
                "x" => SetSelector::from_bits(0b0001),
                _ => bail!(
                    "selector {} must contain sequential elements starting from `x`",
                    select_str
                ),
            }
        } else {
            let mut selector = SetSelector::empty();
            for c in select_str.chars() {
                match c {
                    'x' => {
                        if selector.set(0b00) {
                            bail!("element `x` already present in unordered selector");
                        }
                    }
                    'y' => {
                        if selector.set(0b01) {
                            bail!("element `y` already present in unordered selector");
                        }
                    }
                    'z' => {
                        if selector.set(0b10) {
                            bail!("element `z` already present in unordered selector");
                        }
                    }
                    'w' => {
                        if selector.set(0b11) {
                            bail!("element `w` already present in unordered selector");
                        }
                    }
                    _ => bail!("invalid register selector {}", select_str),
                }
            }
            selector
        };

        Ok(selector)
    }

    fn parse_swizzle_selector(&mut self) -> eyre::Result<SwizzleSelector> {
        let select_str = self.expect_ident()?.to_ascii_lowercase();
        self.bump();
        let mut selector = SwizzleSelector::empty();
        let mut idx = 0;
        for c in select_str.chars() {
            if idx > 3 {
                bail!("too many selectors in {}", select_str);
            }
            match c {
                'x' => selector.set(idx, 0b00),
                'y' => selector.set(idx, 0b01),
                'z' => selector.set(idx, 0b10),
                'w' => selector.set(idx, 0b11),
                _ => bail!("invalid register selector {}", select_str),
            }
            idx += 1;
        }

        Ok(selector)
    }

    fn bump(&mut self) {
        let current = self.lexer.next_token();
        self.current = current;
    }

    fn expect_ident(&self) -> eyre::Result<String> {
        match self.current.kind() {
            TokenKind::Ident(s) => Ok(s.clone()),
            other => bail!("expected identifier, found `{}`", other),
        }
    }

    /// expects the current token to be a specific kind, and eats it if it matches
    /// returns true if the token was eaten, false otherwise
    fn eat(&mut self, kind: &TokenKind) -> bool {
        if self.current.kind() == kind {
            self.bump();
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AddMode {
    Normal,
    Saturate,
    CheckCarry,
    CheckSign,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SubMode {
    Normal,
    Saturate,
    RevNormal,
    RevSaturate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CmpMode {
    Eq,
    Carry,
    RevCarry,
}
