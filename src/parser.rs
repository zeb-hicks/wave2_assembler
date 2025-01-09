use log::{debug, warn};

use crate::lexer::Span;
use crate::{
    diag::{Context, Diagnostic},
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

    pub fn parse_inst(&mut self, ctx: &mut Context) -> Result<Option<Instruction>, ()> {
        // eat all newlines before an instruction to ignore empty lines
        // whitespace is ignored entirely, so it does not need to be considered
        while matches!(self.current.kind(), TokenKind::Newline) {
            self.bump();
        }

        if self.current.kind() == &TokenKind::EoF {
            return Ok(None);
        }

        let span_start = self.current.span();
        let mut inner = || {
            let inst = self.expect_ident().map_err(|d| ctx.add_diag(d))?;

            let inst = match inst.to_lowercase().as_str() {
                "mov" => self.parse_move(ctx)?,
                "swizzle" => self.parse_swizzle(ctx)?,
                "add" => self.parse_add(ctx, AddMode::Normal)?,
                "add_sat" => self.parse_add(ctx, AddMode::Saturate)?,
                "sub" => self.parse_sub(ctx, SubMode::Normal)?,
                "sub_sat" => self.parse_sub(ctx, SubMode::Saturate)?,
                "cmpeq" => self.parse_cmp(ctx, CmpMode::Eq)?,
                "cmpneq" => self.parse_cmp(ctx, CmpMode::Neq)?,
                "lsl" | "asl" => self.parse_lsl(ctx)?,
                "rol" => self.parse_rol(ctx)?,
                "asr" => self.parse_asr(ctx)?,
                "lsr" => self.parse_lsr(ctx)?,
                "ror" => self.parse_ror(ctx)?,
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

    fn parse_move(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
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
                        self.current.span(),
                    ));
                }

                Ok(Instruction::Move { src, dst })
            }
            (LoadStoreOp::MemOp(mem), LoadStoreOp::RegOp(dst)) => {
                Ok(Instruction::Load { mem, dst })
            }
            (LoadStoreOp::RegOp(src), LoadStoreOp::MemOp(mem)) => {
                Ok(Instruction::Store { src, mem })
            }

            // mem-to-mem moves do not exist
            (LoadStoreOp::MemOp(_), LoadStoreOp::MemOp(_)) => todo!(),
        }
    }

    fn parse_swizzle(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        self.bump();

        let reg = self.parse_swizzle_reg(ctx)?;
        Ok(Instruction::Swizzle { reg })
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
                Span::between(span_start, self.current.span()),
            ));

            // pick lhs to be the src arbitrarily
            // to create a dummy instruction for further parsing
            lhs
        };

        let inst = match mode {
            AddMode::Normal => Instruction::Add { size, src, dst },
            AddMode::Saturate => Instruction::AddSaturate { size, src, dst },
        };

        Ok(inst)
    }

    fn parse_sub(&mut self, ctx: &mut Context, mode: SubMode) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        let (size, dst, lhs, rhs) = self.parse_math_common(ctx)?;

        let inst = if dst == lhs {
            // dst = lhs - rhs
            // dst = dst - rhs
            match mode {
                SubMode::Normal => Instruction::SubRev {
                    size,
                    src: rhs,
                    dst,
                },
                SubMode::Saturate => Instruction::SubRevSaturate {
                    size,
                    src: rhs,
                    dst,
                },
            }
        } else if dst == rhs {
            // dst = lhs - rhs
            // dst = lhs - dst
            match mode {
                SubMode::Normal => Instruction::Sub {
                    size,
                    src: lhs,
                    dst,
                },
                SubMode::Saturate => Instruction::SubSaturate {
                    size,
                    src: lhs,
                    dst,
                },
            }
        } else {
            // sub could not be encoded in only two registers, error
            ctx.add_diag(Diagnostic::new(
                String::from("sub must be of the form `dst = dst - src` or `dst = src - dst`"),
                Span::between(span_start, self.current.span()),
            ));

            // generate a dummy sub instruction to allow for further parsing
            Instruction::Sub {
                size,
                src: lhs,
                dst: rhs,
            }
        };

        Ok(inst)
    }

    fn parse_cmp(&mut self, ctx: &mut Context, mode: CmpMode) -> Result<Instruction, ()> {
        let span_start = self.current.span();
        let (size, dst, lhs, rhs) = self.parse_math_common(ctx)?;

        let inst = if dst == lhs {
            // dst = lhs cmp rhs
            // dst = dst cmp src
            let src = rhs;

            match mode {
                CmpMode::Eq => Instruction::CmpEq { size, src, dst },
                CmpMode::Neq => Instruction::CmpNeq { size, src, dst },
            }
        } else if dst == rhs {
            // dst = lhs cmp rhs
            // dst = src cmp dst
            let src = lhs;

            match mode {
                CmpMode::Eq => Instruction::CmpEq { size, src, dst },
                CmpMode::Neq => Instruction::CmpNeq { size, src, dst },
            }
        } else {
            // cmp could not be encoded in only two registers, error
            ctx.add_diag(Diagnostic::new(
                String::from("compare instructions must be of the form `dst = dst <cmp> src` or `dst = src <cmp> dst`"),
                Span::between(span_start, self.current.span()),
            ));

            // generate a dummy instruction to allow for further parsing
            Instruction::CmpEq {
                size,
                src: lhs,
                dst: rhs,
            }
        };

        Ok(inst)
    }

    fn parse_lsl(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let (size, dst, amount) = self.parse_shift_common(ctx)?;
        Ok(Instruction::ShiftLeft { size, dst, amount })
    }

    fn parse_rol(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let (size, dst, amount) = self.parse_shift_common(ctx)?;
        Ok(Instruction::RotateLeft { size, dst, amount })
    }

    fn parse_lsr(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let (size, dst, amount) = self.parse_shift_common(ctx)?;
        Ok(Instruction::ShiftRightLogical { size, dst, amount })
    }

    fn parse_asr(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let (size, dst, amount) = self.parse_shift_common(ctx)?;
        Ok(Instruction::ShiftRightArithmetic { size, dst, amount })
    }

    fn parse_ror(&mut self, ctx: &mut Context) -> Result<Instruction, ()> {
        let (size, dst, amount) = self.parse_shift_common(ctx)?;
        Ok(Instruction::RotateRight { size, dst, amount })
    }

    fn parse_and(&mut self, ctx: &mut Context) -> Instruction {
        let (src, dst) = self.parse_bitop_common(ctx);
        Instruction::BitAnd { src, dst }
    }

    fn parse_or(&mut self, ctx: &mut Context) -> Instruction {
        let (src, dst) = self.parse_bitop_common(ctx);
        Instruction::BitOr { src, dst }
    }

    fn parse_xor(&mut self, ctx: &mut Context) -> Instruction {
        let (src, dst) = self.parse_bitop_common(ctx);
        Instruction::BitXor { src, dst }
    }

    fn parse_nand(&mut self, ctx: &mut Context) -> Instruction {
        let (src, dst) = self.parse_bitop_common(ctx);
        Instruction::BitNand { src, dst }
    }

    fn parse_nor(&mut self, ctx: &mut Context) -> Instruction {
        let (src, dst) = self.parse_bitop_common(ctx);
        Instruction::BitNor { src, dst }
    }

    fn parse_xnor(&mut self, ctx: &mut Context) -> Instruction {
        let (src, dst) = self.parse_bitop_common(ctx);
        Instruction::BitXnor { src, dst }
    }

    fn parse_unary_not(&mut self, ctx: &mut Context) -> Instruction {
        self.bump();
        let dst = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0)
        });
        Instruction::UnaryBitNot { dst }
    }

    // =======================
    // utilities
    // =======================

    fn parse_math_common(
        &mut self,
        ctx: &mut Context,
    ) -> Result<(OpSize, RegSelector, RegSelector, RegSelector), ()> {
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

        let dst = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0)
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
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0)
        });
        if !self.eat(&TokenKind::Comma) {
            if matches!(self.current.kind(), TokenKind::Newline) {
                // the user probably assumed this was a two argument math op of the form dst, src
                // but it actually is three, dst, lhs, rhs
                ctx.add_diag(Diagnostic::new(
                    String::from("math operands are of the form `op dst, lhs, rhs`"),
                    self.current.span(),
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
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0)
        });
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

        // shifts are odd in that they are dst = dst <shift> amount
        // if there was an error parsing the dst register, use a dummy selector
        // to allow parsing to continue
        let dst = self.parse_reg().unwrap_or(RegSelector::new_gpr(0));

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
                // use a dummy selector to allow recovery
                RegSelector::new_gpr(0)
            })),
            TokenKind::Number(num) => {
                let num = *num;
                self.bump();

                if num > 0b1111 {
                    ctx.add_diag(Diagnostic::new(
                        String::from("shift amount must not be greater than 15"),
                        self.current.span(),
                    ));
                    // dummy value for recovery
                    ShiftAmount::Const(0)
                } else {
                    ShiftAmount::Const(num as u8)
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

        Ok((size, dst, amount))
    }

    fn parse_bitop_common(&mut self, ctx: &mut Context) -> (RegSelector, RegSelector) {
        self.bump();

        let lhs = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0)
        });

        if !self.eat(&TokenKind::Comma) {
            ctx.add_diag(Diagnostic::new(
                String::from("missing comma between lhs and rhs"),
                self.current.span(),
            ));
            // allow recovery by not returning
        }

        let rhs = self.parse_reg().unwrap_or_else(|d| {
            ctx.add_diag(d);
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0)
        });

        (lhs, rhs)
    }

    fn parse_move_operand(&mut self, ctx: &mut Context) -> Result<LoadStoreOp, ()> {
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
                    self.current.span(),
                ));
                // recover with the invalid selector
            }

            if is_mem && !self.eat(&TokenKind::RightBracket) {
                ctx.add_diag(Diagnostic::new(
                    String::from("missing `]` after memory operand"),
                    self.current.span(),
                ));
                // it's probably just missing, recover
            }

            let increment = self.eat(&TokenKind::Plus);
            Ok(LoadStoreOp::MemOp(MemoryOperand::new(
                set.reg(),
                all,
                increment,
            )))
        } else {
            Ok(LoadStoreOp::RegOp(set))
        }
    }

    fn parse_reg(&mut self) -> Result<RegSelector, Diagnostic> {
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
                RegSelector::new_gpr(7)
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
                RegSelector::new_gpr(idx)
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
            RegSelector::new_const(idx)
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

        if !self.eat(&TokenKind::Dot) {
            ctx.add_diag(Diagnostic::new(
                String::from("expected a `.` between a register name and its element selector"),
                self.current.span(),
            ));
            return Err(());
        }

        let selector = self.parse_selector(ctx, select_mode)?;

        Ok(SetRegSelector::new(reg, selector))
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

        Ok(SwizzleRegSelector::new(reg, selector))
    }

    fn parse_selector(
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
            SelectorMode::NoRestrict => {
                let mut selector = SetSelector::empty();
                for c in select_str.chars() {
                    // NOTE: duplicate selectors are not an immediate return, just ignored for recovery
                    match c {
                        'x' => {
                            if selector.set(0b00) {
                                ctx.add_diag(Diagnostic::new(
                                    String::from("`x` already present in unordered selector"),
                                    ident_span,
                                ));
                            }
                        }
                        'y' => {
                            if selector.set(0b01) {
                                ctx.add_diag(Diagnostic::new(
                                    String::from("`y` already present in unordered selector"),
                                    ident_span,
                                ));
                            }
                        }
                        'z' => {
                            if selector.set(0b10) {
                                ctx.add_diag(Diagnostic::new(
                                    String::from("`z` already present in unordered selector"),
                                    ident_span,
                                ));
                            }
                        }
                        'w' => {
                            if selector.set(0b11) {
                                ctx.add_diag(Diagnostic::new(
                                    String::from("`w` already present in unordered selector"),
                                    ident_span,
                                ));
                            }
                        }
                        _ => {
                            ctx.add_diag(Diagnostic::new(
                                format!("invalid register selector {}", select_str),
                                ident_span,
                            ));
                            return Err(());
                        }
                    }
                }
                selector
            }
            SelectorMode::Ordered => {
                let mut selector = SetSelector::empty();
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
                "xyzw" => SetSelector::from_bits(0b1111),
                "xyz" => SetSelector::from_bits(0b0111),
                "xy" => SetSelector::from_bits(0b0011),
                "x" => SetSelector::from_bits(0b0001),
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
        let mut selector = SwizzleSelector::empty();
        let mut idx = 0;
        for c in select_str.chars() {
            if idx > 3 {
                ctx.add_diag(Diagnostic::new(
                    String::from("too many selectors"),
                    ident_span,
                ));
                return Err(());
            }
            match c {
                'x' => selector.set(idx, 0b00),
                'y' => selector.set(idx, 0b01),
                'z' => selector.set(idx, 0b10),
                'w' => selector.set(idx, 0b11),
                _ => {
                    ctx.add_diag(Diagnostic::new(
                        String::from("invalid register selector"),
                        ident_span,
                    ));
                    return Err(());
                }
            }
            idx += 1;
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
    /// the selector can contain any elements in any order
    /// this is used for swizzle, for example
    NoRestrict,
    /// the selector must list the elements it selects in order, but may
    /// start with any selector
    Ordered,
    /// selector must start with an X selector and contain sequential selectors
    SquentialXStart,
}

#[derive(Debug, Clone, Copy)]
enum LoadStoreOp {
    MemOp(MemoryOperand),
    RegOp(SetRegSelector),
}

impl LoadStoreOp {
    fn is_reg(&self) -> bool {
        matches!(self, Self::RegOp(_))
    }

    fn is_mem(&self) -> bool {
        matches!(self, Self::MemOp(_))
    }
}
