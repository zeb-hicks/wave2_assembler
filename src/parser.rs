use log::*;

use crate::instruction::Instruction;
use crate::lexer::Span;
use crate::{
    diag::{Context, Diagnostic},
    instruction::{
        InstructionKind, MemoryOperand, OpSize, RegSelector, SetRegSelector, SetSelector,
        ShiftAmount, SwizzleRegSelector, SwizzleSelector, MAX_REG_IDX,
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

    pub fn parse_inst(&mut self, ctx: &mut Context) -> Result<Option<Instruction>, ()> {
        // eat all newlines before an instruction to ignore empty lines
        // whitespace is ignored entirely, so it does not need to be considered
        while matches!(self.current.kind(), TokenKind::Newline) {
            self.bump();
        }

        if self.current.kind() == &TokenKind::EoF {
            return Ok(None);
        }

        if self.current.kind() == &TokenKind::Literal {
            // Literals are inserted directly into the instruction 
            self.bump();
        }

        if matches!(self.current.kind(), TokenKind::Raw(_)) {
            return Ok(Some(self.parse_literal_raw(ctx)?));
        }

        let span_start = self.current.span();
        let mut inner = || {
            let inst = self.expect_ident().map_err(|d| ctx.add_diag(d))?;

            let inst = match inst.to_lowercase().as_str() {
                "mov" => self.parse_move(ctx)?,
                "swizzle" => self.parse_swizzle(ctx)?,

                "wmove" => self.parse_wselect(ctx, WSelectMode::Move)?,
                "wswap" => self.parse_wselect(ctx, WSelectMode::Swap)?,
                "wadd" => self.parse_wselect(ctx, WSelectMode::Add)?,
                "wsub" => self.parse_wselect(ctx, WSelectMode::Sub)?,

                // =========
                // math ops
                // =========
                "add" => self.parse_add(ctx, AddMode::Normal)?,
                "add_sat" => self.parse_add(ctx, AddMode::Saturate)?,
                "sub" => self.parse_sub(ctx, SubMode::Normal)?,
                "sub_sat" => self.parse_sub(ctx, SubMode::Saturate)?,
                "cmpeq" => self.parse_cmp(ctx, CmpMode::Eq)?,
                "cmpneq" => self.parse_cmp(ctx, CmpMode::Neq)?,

                // ==========
                // shift ops
                // ==========
                "lsl" | "asl" => self.parse_lsl(ctx)?,
                "rol" => self.parse_rol(ctx)?,
                "asr" => self.parse_asr(ctx)?,
                "lsr" => self.parse_lsr(ctx)?,
                "ror" => self.parse_ror(ctx)?,

                // ============
                // bitwise ops
                // ============
                "and" => self.parse_and(ctx),
                "or" => self.parse_or(ctx),
                "xor" => self.parse_xor(ctx),
                "nand" => self.parse_nand(ctx),
                "nor" => self.parse_nor(ctx),
                "xnor" => self.parse_xnor(ctx),
                "not" => self.parse_unary_not(ctx),

                _ => {
                    ctx.add_diag(Diagnostic::new(
                        format!("invalid instruction `{}`", inst),
                        self.current.span(),
                    ));
                    return Err(());
                }
            };
            Ok(Some(inst))
        };

        let ret = inner();
        // eat until newline to prevent cascading errors
        if ctx.had_errs() {
            while !matches!(self.current.kind(), TokenKind::Newline | TokenKind::EoF) {
                self.bump();
            }
        }

        if ret.is_err() {
            warn!("parsing encountered \"fatal\" error starting at {:?}, this probably should not happen", span_start);
        }

        ret
    }

    fn parse_literal_raw(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        // self.bump();

        match self.current.kind() {
            TokenKind::Raw(val) => {
                let val = *val;
                self.bump();
                Ok(Instruction::new(
                    InstructionKind::Raw { val },
                    Span::between(span_start, self.current.span()),
                ))
            },
            _ => {
                ctx.add_diag(Diagnostic::new(
                    String::from(format!("expected raw value, got {:?}", self.current.kind())),
                    self.current.span(),
                ));

                Ok(Instruction::new(
                    InstructionKind::Raw { val: 0 },
                    Span::between(span_start, self.current.span()),
                ))
            }
        }
        
        // let val = match self.current.kind() {
        //     TokenKind::Raw(val) => *val,
        //     _ => {
        //         ctx.add_diag(Diagnostic::new(
        //             String::from(format!("expected raw value, got {:?}", self.current.kind())),
        //             self.current.span(),
        //         ));
        //         return Err(());
        //     }
        // };
        // return Ok(Instruction::new(
        //     InstructionKind::Raw { val: val },
        //     Span::between(span_start, self.current.span()),
        // ));
    }

    fn parse_move(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        self.bump();

        let dst = self.parse_move_operand(ctx)?;

        if !self.eat(&TokenKind::Comma) {
            ctx.add_diag(Diagnostic::new(
                String::from("missing comma between dst and src"),
                self.current.span(),
            ));
            // allow recovery by not returning
        }

        let src = self.parse_move_operand(ctx)?;

        match (src, dst) {
            (LoadStoreOp::RegOp(src), LoadStoreOp::RegOp(dst)) => {
                // for register-to-register moves, the selected source elements must be the same
                // as the selected destination elements.
                // while it could be reasonable to allow the source to only be a *subset* of the
                // destination (as in `move r0.xyzw, r1.xyz`), exact equality expresses the
                // same thing but with less room for error.
                if dst.selector() != src.selector() {
                    // this is not critical to fail on, it's mostly for clarity in writing
                    ctx.add_diag(Diagnostic::new(
                        String::from("lhs and rhs of move must select the same elements"),
                        Span::between(dst.span(), src.span()),
                    ));
                }

                // the dst must be a writable register
                if !dst.reg().is_gpr() {
                    ctx.add_diag(Diagnostic::new(
                        format!("expected dst to be a writable register, got {}", dst.reg()),
                        dst.span(),
                    ));
                }

                Ok(Instruction::new(
                    InstructionKind::Move { src, dst },
                    Span::between(span_start, src.span()),
                ))
            }
            (LoadStoreOp::MemOp(mem), LoadStoreOp::RegOp(dst)) => {
                // the dst must be a writable register
                if !dst.reg().is_gpr() {
                    ctx.add_diag(Diagnostic::new(
                        format!("expected dst to be a writable register, got {}", dst.reg()),
                        dst.span(),
                    ));
                }

                Ok(Instruction::new(
                    InstructionKind::Load { mem, dst },
                    Span::between(span_start, mem.span()),
                ))
            }
            (LoadStoreOp::RegOp(src), LoadStoreOp::MemOp(mem)) => Ok(Instruction::new(
                InstructionKind::Store { src, mem },
                Span::between(span_start, src.span()),
            )),

            // mem-to-mem moves do not exist
            (LoadStoreOp::MemOp(_), LoadStoreOp::MemOp(_)) => todo!(),
        }
    }

    fn parse_swizzle(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        self.bump();

        let dst = self.parse_swizzle_reg(ctx)?;
        // the dst must be a writable register
        if !dst.reg().is_gpr() {
            ctx.add_diag(Diagnostic::new(
                format!("expected dst to be a writable register, got {}", dst.reg()),
                dst.span(),
            ));
        }
        Ok(Instruction::new(
            InstructionKind::Swizzle { reg: dst },
            Span::between(span_start, dst.span()),
        ))
    }

    fn parse_wselect(&mut self, ctx: &mut Context, mode: WSelectMode) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        self.bump();

        let dst = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        if !self.eat(&TokenKind::Comma) {
            ctx.add_diag(Diagnostic::new(
                String::from("missing comma between dst and src"),
                self.current.span(),
            ));
            // allow recovery by not returning
        }

        let src = self.parse_swizzle_single_reg(ctx)?;
        // the src must be a readable register
        if !src.reg().is_gpr() && !src.reg().is_const() {
            ctx.add_diag(Diagnostic::new(
                format!("expected src to be a readable register, got {}", src.reg()),
                src.span(),
            ));
        }

        let kind = match mode {
            WSelectMode::Move => InstructionKind::WMove { src, dst },
            WSelectMode::Swap => InstructionKind::WSwap { src, dst },
            WSelectMode::Add => InstructionKind::WAdd { src, dst },
            WSelectMode::Sub => InstructionKind::WSub { src, dst },
        };

        Ok(Instruction::new(
            kind,
            Span::between(span_start, src.span()),
        ))
    }

    fn parse_add(&mut self, ctx: &mut Context, mode: AddMode) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        let (size, dst, lhs, rhs) = self.parse_math_common(ctx)?;

        // select the correct src based on which other register
        // was not the dst
        //
        // we can do this because the order does not matter, since + is commutative
        // so there's no need to select a "reversed" instruction kind
        let src = if dst == lhs {
            // dst = lhs + rhs
            // dst = dst + src
            rhs
        } else if dst == rhs {
            // dst = lhs + rhs
            // dst = src + dst
            lhs
        } else {
            // add could not be encoded in only two registers, error
            ctx.add_diag(Diagnostic::new(
                String::from("add must be of the form `dst = dst + src` or `dst = src + dst`"),
                Span::between(span_start, rhs.span()),
            ));

            // pick lhs to be the src arbitrarily
            // to create a dummy instruction for further parsing
            lhs
        };

        let kind = match mode {
            AddMode::Normal => InstructionKind::Add { size, src, dst },
            AddMode::Saturate => InstructionKind::AddSaturate { size, src, dst },
        };

        Ok(Instruction::new(
            kind,
            Span::between(span_start, rhs.span()),
        ))
    }

    fn parse_sub(&mut self, ctx: &mut Context, mode: SubMode) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        let (size, dst, lhs, rhs) = self.parse_math_common(ctx)?;

        let kind = if dst == lhs {
            // dst = lhs - rhs
            // dst = dst - rhs
            match mode {
                SubMode::Normal => InstructionKind::SubRev {
                    size,
                    src: rhs,
                    dst,
                },
                SubMode::Saturate => InstructionKind::SubRevSaturate {
                    size,
                    src: rhs,
                    dst,
                },
            }
        } else if dst == rhs {
            // dst = lhs - rhs
            // dst = lhs - dst
            match mode {
                SubMode::Normal => InstructionKind::Sub {
                    size,
                    src: lhs,
                    dst,
                },
                SubMode::Saturate => InstructionKind::SubSaturate {
                    size,
                    src: lhs,
                    dst,
                },
            }
        } else {
            // sub could not be encoded in only two registers, error
            ctx.add_diag(Diagnostic::new(
                String::from("sub must be of the form `dst = dst - src` or `dst = src - dst`"),
                Span::between(span_start, rhs.span()),
            ));

            // generate a dummy sub instruction to allow for further parsing
            InstructionKind::Sub {
                size,
                src: lhs,
                dst: rhs,
            }
        };

        Ok(Instruction::new(
            kind,
            Span::between(span_start, rhs.span()),
        ))
    }

    fn parse_cmp(&mut self, ctx: &mut Context, mode: CmpMode) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        let (size, dst, lhs, rhs) = self.parse_math_common(ctx)?;

        let kind = if dst == lhs {
            // dst = lhs cmp rhs
            // dst = dst cmp src
            let src = rhs;

            match mode {
                CmpMode::Eq => InstructionKind::CmpEq { size, src, dst },
                CmpMode::Neq => InstructionKind::CmpNeq { size, src, dst },
            }
        } else if dst == rhs {
            // dst = lhs cmp rhs
            // dst = src cmp dst
            let src = lhs;

            match mode {
                CmpMode::Eq => InstructionKind::CmpEq { size, src, dst },
                CmpMode::Neq => InstructionKind::CmpNeq { size, src, dst },
            }
        } else {
            // cmp could not be encoded in only two registers, error
            ctx.add_diag(Diagnostic::new(
                String::from("compare instructions must be of the form `dst = dst <cmp> src` or `dst = src <cmp> dst`"),
                Span::between(span_start, rhs.span()),
            ));

            // generate a dummy instruction to allow for further parsing
            InstructionKind::CmpEq {
                size,
                src: lhs,
                dst: rhs,
            }
        };

        Ok(Instruction::new(
            kind,
            Span::between(span_start, rhs.span()),
        ))
    }

    fn parse_lsl(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        let (size, dst, amount) = self.parse_shift_common(ctx)?;
        Ok(Instruction::new(
            InstructionKind::ShiftLeft { size, dst, amount },
            Span::between(span_start, amount.span()),
        ))
    }

    fn parse_rol(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        let (size, dst, amount) = self.parse_shift_common(ctx)?;
        Ok(Instruction::new(
            InstructionKind::RotateLeft { size, dst, amount },
            Span::between(span_start, amount.span()),
        ))
    }

    fn parse_lsr(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        let (size, dst, amount) = self.parse_shift_common(ctx)?;
        Ok(Instruction::new(
            InstructionKind::ShiftRightLogical { size, dst, amount },
            Span::between(span_start, amount.span()),
        ))
    }

    fn parse_asr(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        let (size, dst, amount) = self.parse_shift_common(ctx)?;
        Ok(Instruction::new(
            InstructionKind::ShiftRightArithmetic { size, dst, amount },
            Span::between(span_start, amount.span()),
        ))
    }

    fn parse_ror(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        let (size, dst, amount) = self.parse_shift_common(ctx)?;
        Ok(Instruction::new(
            InstructionKind::RotateRight { size, dst, amount },
            Span::between(span_start, amount.span()),
        ))
    }

    fn parse_and(&mut self, ctx: &mut Context) -> Instruction {
        let span_start = self.current.span();
        let (dst, src) = self.parse_bitop_common(ctx);
        Instruction::new(
            InstructionKind::BitAnd { src, dst },
            Span::between(span_start, src.span()),
        )
    }

    fn parse_or(&mut self, ctx: &mut Context) -> Instruction {
        let span_start = self.current.span();
        let (dst, src) = self.parse_bitop_common(ctx);
        Instruction::new(
            InstructionKind::BitOr { src, dst },
            Span::between(span_start, src.span()),
        )
    }

    fn parse_xor(&mut self, ctx: &mut Context) -> Instruction {
        let span_start = self.current.span();
        let (dst, src) = self.parse_bitop_common(ctx);
        Instruction::new(
            InstructionKind::BitXor { src, dst },
            Span::between(span_start, src.span()),
        )
    }

    fn parse_nand(&mut self, ctx: &mut Context) -> Instruction {
        let span_start = self.current.span();
        let (dst, src) = self.parse_bitop_common(ctx);
        Instruction::new(
            InstructionKind::BitNand { src, dst },
            Span::between(span_start, src.span()),
        )
    }

    fn parse_nor(&mut self, ctx: &mut Context) -> Instruction {
        let span_start = self.current.span();
        let (dst, src) = self.parse_bitop_common(ctx);
        Instruction::new(
            InstructionKind::BitNor { src, dst },
            Span::between(span_start, src.span()),
        )
    }

    fn parse_xnor(&mut self, ctx: &mut Context) -> Instruction {
        let span_start = self.current.span();
        let (dst, src) = self.parse_bitop_common(ctx);
        Instruction::new(
            InstructionKind::BitXnor { src, dst },
            Span::between(span_start, src.span()),
        )
    }

    fn parse_unary_not(&mut self, ctx: &mut Context) -> Instruction {
        let span_start = self.current.span();
        self.bump();
        let mut was_err = false;
        let dst = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });
        // the dst must be a writable register
        if !was_err && !dst.is_gpr() {
            ctx.add_diag(Diagnostic::new(
                format!("expected dst to be a writable register, got {}", dst),
                dst.span(),
            ));
        }

        Instruction::new(
            InstructionKind::UnaryBitNot { dst },
            Span::between(span_start, dst.span()),
        )
    }

    // =======================
    // utilities
    // =======================

    fn parse_math_common(
        &mut self,
        ctx: &mut Context,
    ) -> Result<(OpSize, RegSelector, RegSelector, RegSelector), ()> {
        let span_start = self.current.span();
        self.bump();

        if !self.eat(&TokenKind::Dot) {
            ctx.add_diag(Diagnostic::new(
                String::from("math operands need a `.b` or `.w` to specify size"),
                self.current.span(),
            ));
            return Err(());
        }
        let size = self.expect_ident().map_err(|d| {
            ctx.add_diag(d.with_note(String::from(
                "math operands need a `.b` or `.w` to specify size",
            )));
        })?;

        let size = match size.as_str() {
            "b" => OpSize::Byte,
            "w" => OpSize::Word,
            _ => {
                ctx.add_diag(Diagnostic::new(
                    String::from("math operands need a `.b` or `.w` to specify size"),
                    self.current.span(),
                ));
                return Err(());
            }
        };
        self.bump();

        let mut was_reg_err = true;
        let dst = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });
        if !self.eat(&TokenKind::Comma) {
            ctx.add_diag(Diagnostic::new(
                String::from("missing comma between dst and lhs"),
                self.current.span(),
            ));
            // allow recovery by not returning
        }

        let lhs = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });
        if !self.eat(&TokenKind::Comma) {
            if matches!(self.current.kind(), TokenKind::Newline) {
                // the user probably assumed this was a two argument math op of the form dst, src
                // but it actually is three, dst, lhs, rhs
                ctx.add_diag(Diagnostic::new(
                    String::from("math operands are of the form `op dst, lhs, rhs`"),
                    Span::between(span_start, self.current.span()),
                ));
                // do not recover
                return Err(());
            } else {
                ctx.add_diag(Diagnostic::new(
                    String::from("missing comma between lhs and rhs"),
                    self.current.span(),
                ));
                // allow recovery by not returning
            }
        }

        let rhs = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        // the dst must be a writable register
        if !was_reg_err && !dst.is_gpr() {
            ctx.add_diag(Diagnostic::new(
                format!("expected dst to be a writable register, got {}", dst),
                dst.span(),
            ));
        }

        Ok((size, dst, lhs, rhs))
    }

    fn parse_shift_common(
        &mut self,
        ctx: &mut Context,
    ) -> Result<(OpSize, RegSelector, ShiftAmount), ()> {
        self.bump();

        if !self.eat(&TokenKind::Dot) {
            ctx.add_diag(Diagnostic::new(
                String::from("math operands need a `.b` or `.w` to specify size"),
                self.current.span(),
            ));
            return Err(());
        }
        let size = self.expect_ident().map_err(|d| {
            ctx.add_diag(d.with_note(String::from(
                "math operands need a `.b` or `.w` to specify size",
            )));
        })?;

        let size = match size.as_str() {
            "b" => OpSize::Byte,
            "w" => OpSize::Word,
            _ => {
                ctx.add_diag(Diagnostic::new(
                    String::from("math operands need a `.b` or `.w` to specify size"),
                    self.current.span(),
                ));
                return Err(());
            }
        };
        self.bump();

        let mut was_reg_err = false;
        // if there was an error parsing the dst register, use a dummy selector
        // to allow parsing to continue
        let dst = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        if !self.eat(&TokenKind::Comma) {
            ctx.add_diag(Diagnostic::new(
                String::from("missing comma between lhs and rhs"),
                self.current.span(),
            ));
            // allow recovery by not returning
        }

        let amount = match self.current.kind() {
            TokenKind::Ident(_) => ShiftAmount::Register(self.parse_reg().unwrap_or_else(|d| {
                ctx.add_diag(d);
                was_reg_err = true;
                // use a dummy selector to allow recovery
                RegSelector::new_gpr(0, Span::DUMMY)
            })),
            TokenKind::Number(num) => {
                let span = self.current.span();
                let num = *num;
                self.bump();

                if num > 0b1111 {
                    ctx.add_diag(Diagnostic::new(
                        String::from("shift amount must not be greater than 15"),
                        span,
                    ));
                    // dummy value for recovery
                    ShiftAmount::Const(0, Span::DUMMY)
                } else {
                    ShiftAmount::Const(num as u8, span)
                }
            }
            _ => {
                ctx.add_diag(Diagnostic::new(
                    String::from("expected a register or constant number as a shift amout"),
                    self.current.span(),
                ));
                return Err(());
            }
        };

        // the dst must be a writable register
        if !was_reg_err && !dst.is_gpr() {
            ctx.add_diag(Diagnostic::new(
                format!("expected dst to be a writable register, got {}", dst),
                dst.span(),
            ));
        }

        Ok((size, dst, amount))
    }

    /// parses a bitwise operation of the form `OP dst, src`.
    /// this does not need to have three arguments because all bitwise operations
    /// can be executed with the lhs and rhs in either order.
    /// returns (dst, src)
    fn parse_bitop_common(&mut self, ctx: &mut Context) -> (RegSelector, RegSelector) {
        self.bump();

        let mut was_reg_err = false;
        let dst = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        if !self.eat(&TokenKind::Comma) {
            ctx.add_diag(Diagnostic::new(
                String::from("missing comma between dst and src"),
                self.current.span(),
            ));
            // allow recovery by not returning
        }

        let src = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        // the dst must be a writable register
        if !was_reg_err && !dst.is_gpr() {
            ctx.add_diag(Diagnostic::new(
                format!("expected dst to be a writable register, got {}", dst),
                dst.span(),
            ));
        }

        (dst, src)
    }

    fn parse_move_operand(&mut self, ctx: &mut Context) -> Result<LoadStoreOp, ()> {
        let span_start = self.current.span();
        let is_mem = self.eat(&TokenKind::LeftBracket);

        // memory operands must contain their selector in order starting with X
        // since they must be precisely x or xyzw
        // but register operands can be any ordered sequence of selectors
        let select_mode = if is_mem {
            SelectorMode::SquentialXStart
        } else {
            SelectorMode::Ordered
        };

        // we can exit early here and the higher levels will eat until
        // end of line to prevent cascading errors
        let set = self.parse_set_reg(ctx, select_mode)?;

        if is_mem {
            let selector = set.selector();
            let x_only = selector.x() && !selector.y() && !selector.z() && !selector.w();
            let all = selector.x() && selector.y() && selector.z() && selector.w();
            if !(x_only || all) {
                ctx.add_diag(Diagnostic::new(
                    String::from("memory operands must use either reg.x or reg.xyzw"),
                    selector.span(),
                ));
                // recover with the invalid selector
            }

            let increment = self.eat(&TokenKind::Plus);

            if !self.eat(&TokenKind::RightBracket) {
                ctx.add_diag(Diagnostic::new(
                    String::from("missing `]` after memory operand"),
                    self.current.span(),
                ));
                // it's probably just missing, recover
            }

            let span_end = self.current.span();
            Ok(LoadStoreOp::MemOp(MemoryOperand::new(
                set.reg(),
                all,
                increment,
                Span::between(span_start, span_end),
            )))
        } else {
            Ok(LoadStoreOp::RegOp(set))
        }
    }

    fn parse_reg(&mut self) -> Result<RegSelector, Diagnostic> {
        let span = self.current.span();
        let name = self.expect_ident().map_err(|d| {
            Diagnostic::new(
                format!("expected register name got {}", self.current.kind()),
                d.span(),
            )
        })?;
        let reg = if name.starts_with('r') {
            let idx = &name[1..];
            // ri is an alias for r7
            if idx == "i" {
                RegSelector::new_gpr(7, span)
            } else {
                let idx = match idx.parse::<u8>() {
                    Ok(idx) if idx <= MAX_REG_IDX => idx,
                    _ => {
                        return Err(Diagnostic::new(
                            format!("invalid register `{}`", idx),
                            self.current.span(),
                        )
                        .with_note(format!("maximum register index is {}", MAX_REG_IDX)))
                    }
                };
                RegSelector::new_gpr(idx, span)
            }
        } else if name.starts_with('c') {
            let idx = &name[1..];
            let idx = match idx.parse::<u8>() {
                Ok(idx) if idx <= MAX_REG_IDX => idx,
                _ => {
                    return Err(Diagnostic::new(
                        format!("invalid register `{}`", idx),
                        self.current.span(),
                    )
                    .with_note(format!("maximum register index is {}", MAX_REG_IDX)))
                }
            };
            RegSelector::new_const(idx, span)
        } else {
            return Err(Diagnostic::new(
                format!("invalid register `{}`", name),
                self.current.span(),
            ));
        };
        self.bump();
        Ok(reg)
    }

    fn parse_set_reg(
        &mut self,
        ctx: &mut Context,
        select_mode: SelectorMode,
    ) -> Result<SetRegSelector, ()> {
        let reg = self.parse_reg().map_err(|d| {
            ctx.add_diag(d);
        })?;

        // if !self.eat(&TokenKind::Dot) {
        //     // Assume xyzw as default
        //     ctx.add_diag(Diagnostic::new(
        //         String::from("expected a `.` between a register name and its element selector"),
        //         self.current.span(),
        //     ));
        //     return Err(());
        // }

        let selector;
        if self.eat(&TokenKind::Dot) {
            selector = self.parse_set_selector(ctx, select_mode)?;
        } else {
            // Assume xyzw as default
            selector = SetSelector::from_bits(0b1111, reg.span());
        }

        Ok(SetRegSelector::new(
            reg,
            selector,
            Span::between(reg.span(), selector.span()),
        ))
    }

    fn parse_swizzle_reg(&mut self, ctx: &mut Context) -> Result<SwizzleRegSelector, ()> {
        let reg = self.parse_reg().map_err(|d| {
            ctx.add_diag(d);
        })?;

        if !self.eat(&TokenKind::Dot) {
            ctx.add_diag(Diagnostic::new(
                String::from("expected a `.` between a register name and its element selector"),
                self.current.span(),
            ));
            return Err(());
        }

        let selector = self.parse_swizzle_selector(ctx)?;

        Ok(SwizzleRegSelector::new(
            reg,
            selector,
            Span::between(reg.span(), selector.span()),
        ))
    }

    fn parse_swizzle_single_reg(&mut self, ctx: &mut Context) -> Result<SwizzleRegSelector, ()> {
        let reg = self.parse_reg().map_err(|d| {
            ctx.add_diag(d);
        })?;

        if !self.eat(&TokenKind::Dot) {
            ctx.add_diag(Diagnostic::new(
                String::from("expected a `.` between a register name and its element selector"),
                self.current.span(),
            ));
            return Err(());
        }

        let selector = self.parse_swizzle_single(ctx)?;

        Ok(SwizzleRegSelector::new(
            reg,
            selector,
            Span::between(reg.span(), selector.span()),
        ))
    }

    fn parse_set_selector(
        &mut self,
        ctx: &mut Context,
        select_mode: SelectorMode,
    ) -> Result<SetSelector, ()> {
        let select_str = self
            .expect_ident()
            .unwrap_or_else(|d| {
                ctx.add_diag(d.with_note(String::from("expected a register selector")));
                String::new()
            })
            .to_ascii_lowercase();
        let ident_span = self.current.span();

        self.bump();
        if select_str.len() > 4 {
            ctx.add_diag(Diagnostic::new(
                String::from("too many selectors"),
                ident_span,
            ));
            return Err(());
        }

        let selector = match select_mode {
            SelectorMode::Ordered => {
                let mut selector = SetSelector::empty(ident_span);
                let mut last_idx = -1;

                for c in select_str.chars() {
                    let idx: i32 = match c {
                        'x' => 0b00,
                        'y' => 0b01,
                        'z' => 0b10,
                        'w' => 0b11,
                        _ => {
                            ctx.add_diag(Diagnostic::new(
                                format!("invalid register selector {}", select_str),
                                ident_span,
                            ));
                            return Err(());
                        }
                    };

                    // an element must either be first (last was -1) or preceded by the
                    // previous element in order
                    if last_idx == -1 || last_idx == (idx - 1) {
                        if selector.set(idx as u8) {
                            // NOTE: duplicate selectors are not an immediate return, just ignored for recovery
                            ctx.add_diag(Diagnostic::new(
                                format!("`{}` already present in selector", c),
                                ident_span,
                            ));
                        }
                    } else {
                        ctx.add_diag(Diagnostic::new(
                            String::from("register selector must have its elements in order"),
                            ident_span,
                        ));
                    }

                    last_idx = idx;
                }
                selector
            }
            SelectorMode::SquentialXStart => match select_str.as_str() {
                "xyzw" => SetSelector::from_bits(0b1111, ident_span),
                "xyz" => SetSelector::from_bits(0b0111, ident_span),
                "xy" => SetSelector::from_bits(0b0011, ident_span),
                "x" => SetSelector::from_bits(0b0001, ident_span),
                _ => {
                    ctx.add_diag(Diagnostic::new(
                        String::from("selector must contain sequential elements starting with `x`"),
                        ident_span,
                    ));
                    return Err(());
                }
            },
        };

        Ok(selector)
    }

    fn parse_swizzle_selector(&mut self, ctx: &mut Context) -> Result<SwizzleSelector, ()> {
        let select_str = self
            .expect_ident()
            .unwrap_or_else(|d| {
                ctx.add_diag(d.with_note(String::from("expected a register selector")));
                String::new()
            })
            .to_ascii_lowercase();
        let ident_span = self.current.span();

        self.bump();
        if select_str.len() > 4 {
            ctx.add_diag(Diagnostic::new(
                String::from("too many selectors"),
                ident_span,
            ));
            return Err(());
        }

        let mut selector = SwizzleSelector::empty(ident_span);
        for (idx, c) in select_str.chars().enumerate() {
            match c {
                'x' => selector.set(idx as u8, 0b00),
                'y' => selector.set(idx as u8, 0b01),
                'z' => selector.set(idx as u8, 0b10),
                'w' => selector.set(idx as u8, 0b11),
                _ => {
                    ctx.add_diag(Diagnostic::new(
                        String::from("invalid register selector"),
                        ident_span,
                    ));
                    return Err(());
                }
            }
        }

        Ok(selector)
    }

    fn parse_swizzle_single(&mut self, ctx: &mut Context) -> Result<SwizzleSelector, ()> {
        let select_str = self
            .expect_ident()
            .unwrap_or_else(|d| {
                ctx.add_diag(d.with_note(String::from("expected a register selector")));
                String::new()
            })
            .to_ascii_lowercase();
        let ident_span = self.current.span();

        self.bump();
        if select_str.len() > 1 {
            ctx.add_diag(Diagnostic::new(
                String::from("too many selectors"),
                ident_span,
            ));
            return Err(());
        }

        let mut selector = SwizzleSelector::empty(ident_span);
        for (idx, c) in select_str.chars().enumerate() {
            match c {
                'x' => selector.set(idx as u8, 0b00),
                'y' => selector.set(idx as u8, 0b01),
                'z' => selector.set(idx as u8, 0b10),
                'w' => selector.set(idx as u8, 0b11),
                _ => {
                    ctx.add_diag(Diagnostic::new(
                        String::from("invalid register selector"),
                        ident_span,
                    ));
                    return Err(());
                }
            }
        }

        Ok(selector)
    }

    fn bump(&mut self) {
        let current = self.lexer.next_token();
        self.current = current;
    }

    fn expect_ident(&self) -> Result<String, Diagnostic> {
        match self.current.kind() {
            TokenKind::Ident(s) => Ok(s.clone()),
            other => Err(Diagnostic::new(
                format!("expected identifier, found `{}`", other),
                self.current.span(),
            )),
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SubMode {
    Normal,
    Saturate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CmpMode {
    Eq,
    Neq,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SelectorMode {
    /// the selector must list the elements it selects in order, but may
    /// start with any selector
    Ordered,
    /// selector must start with an X selector and contain sequential selectors
    SquentialXStart,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WSelectMode {
    Move,
    Swap,
    Add,
    Sub,
}

#[derive(Debug, Clone, Copy)]
enum LoadStoreOp {
    MemOp(MemoryOperand),
    RegOp(SetRegSelector),
}
