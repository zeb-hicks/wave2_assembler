use std::hash::{DefaultHasher, Hash, Hasher};

use log::*;

use crate::instruction::Instruction;
use crate::lexer::Span;
use crate::{
    diag::{Context, Diagnostic},
    instruction::{
        InstructionKind, MemoryOperand, OpSize, RegSelector, SetRegSelector, SetSelector,
        ShiftAmount, SwizzleRegSelector, SwizzleSelector, MAX_REG_IDX,
    },
    lexer::{Lexer, Token, LexerToken},
};

pub enum ParserSection {
    Memory,
    Code
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
    section: ParserSection,
}

pub fn hash_label(label: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    label.hash(&mut hasher);
    hasher.finish()
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Result<Self, Diagnostic> {
        let mut lexer = Lexer::new(src);
        let current = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => {
                return Err(err);
            }
        };
        let section = ParserSection::Code;
        Ok(Self { lexer, current, section })
    }

    fn parse_section(&mut self, ctx: &mut Context) -> Result<(), Diagnostic> {
        let section_label = self.expect_ident().map_err(|d| ctx.add_diag(d));
        let section_str = section_label.clone().map(|s| s.to_lowercase());
        match section_str.as_deref() {
            Ok("code") => {
                self.section = ParserSection::Code;
                self.bump(ctx);
            }
            Ok("memory") => {
                self.section = ParserSection::Memory;
                self.bump(ctx);
            }
            _ => {
                self.bump(ctx);
                ctx.add_diag(Diagnostic::new(
                    format!("Invalid section declaration {:?}", section_label),
                    self.current.span()
                ));
            }
        }
        return Ok(());
    }

    pub fn parse_inst(&mut self, ctx: &mut Context) -> Result<Vec<Instruction>, Diagnostic> {
        // eat all newlines before an instruction to ignore empty lines
        // whitespace is ignored entirely, so it does not need to be considered
        while matches!(self.current.kind(), LexerToken::Newline) {
            self.bump(ctx);
        }

        if self.current.kind() == &LexerToken::EoF {
            return Ok(vec![]);
        }

        if self.current.kind() == &LexerToken::Dot {
            // Start of a section definition
            self.bump(ctx);
            self.parse_section(ctx)?;
        }

        while matches!(self.section, ParserSection::Memory) {
            match self.current.kind() {
                LexerToken::Number(_) |
                LexerToken::Ident(_) |
                LexerToken::Newline => {
                    self.bump(ctx);
                }
                LexerToken::Dot => {
                    self.bump(ctx);
                    self.parse_section(ctx)?;
                }
                _ => {
                    ctx.add_diag(Diagnostic::new(
                        format!("Invalid token in memory region"),
                        self.current.span()
                    ));
                    return Ok(vec![]);
                }
            }
        }

        while matches!(self.current.kind(), LexerToken::Newline) {
            self.bump(ctx);
        }

        if self.current.kind() == &LexerToken::Literal {
            // Literals are inserted directly into the instruction
            self.bump(ctx);
        }

        if matches!(self.current.kind(), LexerToken::LabelDef(_)) {
            let label = match self.expect_label() {
                Ok(label) => label,
                Err(e) => {
                    return Err(e);
                }
            };
            let span = self.current.span();

            self.bump(ctx);

            return Ok(vec![Instruction::new(
                InstructionKind::LabelDefinition { label: hash_label(&label) },
                span,
            )]);
        }

        if matches!(self.current.kind(), LexerToken::Raw(_)) {
            return Ok(vec![self.parse_literal_raw(ctx)?]);
        }

        let span_start = self.current.span();
        let mut inner = || {
            let inst = self.expect_ident().map_err(|d| ctx.add_diag(d))?;

            let inst = match inst.to_lowercase().as_str() {
                "mov" | "move" => self.parse_move(ctx)?,
                "swi" | "swizzle" => self.parse_swizzle(ctx)?,

                "wmov" | "wmove" => self.parse_wselect(ctx, WSelectMode::Move)?,
                "wswap" => self.parse_wselect(ctx, WSelectMode::Swap)?,
                "wadd" => self.parse_wselect(ctx, WSelectMode::Add)?,
                "wsub" => self.parse_wselect(ctx, WSelectMode::Sub)?,

                // =========
                // math ops
                // =========
                "add"  => self.parse_add(ctx, AddMode::Normal)?,
                "adds" => self.parse_add(ctx, AddMode::Saturate)?,
                "sub"  => self.parse_sub(ctx, SubMode::Normal)?,
                "subs" => self.parse_sub(ctx, SubMode::Saturate)?,
                "eq" | "equ" => self.parse_cmp(ctx, CmpMode::Eq)?,
                "ne" | "neq" => self.parse_cmp(ctx, CmpMode::Neq)?,

                "carry" => self.parse_carry(ctx)?,

                "lt" => self.parse_cmp(ctx, CmpMode::LessU)?,
                "gt" => self.parse_cmp(ctx, CmpMode::GreaterU)?,

                "le" | "lte" => self.parse_cmp(ctx, CmpMode::LessEqU)?,
                "ge" | "gte" => self.parse_cmp(ctx, CmpMode::GreaterEqU)?,

                "ado" | "addo" | "addover" => self.parse_add(ctx, AddMode::Over)?,
                "suo" | "subo" | "subover" => self.parse_sub(ctx, SubMode::Over)?,
                "rso" | "rsuo" | "rsubo" | "rsubover" => self.parse_sub(ctx, SubMode::ROver)?,

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

                // All { dst: RegSelector, },
                "all" => self.parse_bit_op_unary(ctx, BitOpMode::All)?,
                // Zero { dst: RegSelector, },
                "zero" => self.parse_zero(ctx)?,
                // One { dst: RegSelector, },
                "one" => self.parse_bit_op_unary(ctx, BitOpMode::One)?,
                // Swap { src: RegSelector, dst: RegSelector, },
                "swp" | "swap" => self.parse_bit_op(ctx, BitOpMode::Swap)?,
                // NotSrc { src: RegSelector, dst: RegSelector, },
                "nsrc" | "notsrc" => self.parse_bit_op(ctx, BitOpMode::NotSrc)?,
                // NotDst { dst: RegSelector, },
                "ndst" | "notdst" | "notdest" => self.parse_bit_op_unary(ctx, BitOpMode::NotDst)?,
                // SrcAndNotDst { src: RegSelector, dst: RegSelector, },
                "sand" | "srcandnotdst" => self.parse_bit_op(ctx, BitOpMode::SrcAndNotDst)?,
                // NotSrcAndDst { src: RegSelector, dst: RegSelector, },
                "nsad" | "notsrcanddst" => self.parse_bit_op(ctx, BitOpMode::NotSrcAndDst)?,
                // SrcOrNotDst { src: RegSelector, dst: RegSelector, },
                "sond" | "srcornotdst" => self.parse_bit_op(ctx, BitOpMode::SrcOrNotDst)?,
                // NotSrcOrDst { src: RegSelector, dst: RegSelector, },
                "nsod" | "notsrcordst" => self.parse_bit_op(ctx, BitOpMode::NotSrcOrDst)?,
                // And { src: RegSelector, dst: RegSelector, },
                "and" => self.parse_bit_op(ctx, BitOpMode::And)?,
                // Or { src: RegSelector, dst: RegSelector, },
                "or" => self.parse_bit_op(ctx, BitOpMode::Or)?,
                // Xor { src: RegSelector, dst: RegSelector, },
                "xor" => self.parse_bit_op(ctx, BitOpMode::Xor)?,
                // Nand { src: RegSelector, dst: RegSelector, },
                "nand" => self.parse_bit_op(ctx, BitOpMode::Xand)?,
                // Nor { src: RegSelector, dst: RegSelector, },
                "nor" => self.parse_bit_op(ctx, BitOpMode::Nor)?,
                // XNor { src: RegSelector, dst: RegSelector, },
                "xnor" => self.parse_bit_op(ctx, BitOpMode::Xnor)?,

                // ============
                // special ops
                // ============
                "hadd" => self.parse_spec(ctx, SpecOpMode::HorizontalAdd)?,
                "mul" | "mults" | "multisat" => self.parse_spec(ctx, SpecOpMode::MultiplySaturate)?,
                "mlo" | "multl" | "multlow" => self.parse_spec(ctx, SpecOpMode::MultiplyLow)?,
                "mhi" | "multh" | "multhigh" => self.parse_spec(ctx, SpecOpMode::MultiplyHigh)?,
                "div" | "divide" => self.parse_spec(ctx, SpecOpMode::Divide)?,
                "rdiv" | "rdivide" => self.parse_spec(ctx, SpecOpMode::ReciprocalDivide)?,

                // ==========
                // system ops
                // ==========
                "nop" => self.parse_nop(ctx),
                "hlt" | "halt" => self.parse_halt(ctx),
                "slp" | "sleep" => self.parse_sleep(ctx),

                // ==========
                // shorthand ops
                // ==========
                "skip" | "skip1" => self.parse_skip(ctx, 1),
                "skip2" => self.parse_skip(ctx, 2),
                "skip3" => self.parse_skip(ctx, 3),
                "skip4" => self.parse_skip(ctx, 4),
                "set" | "set1" => self.parse_set(ctx, 1),
                "set2" => self.parse_set(ctx, 2),
                "set3" => self.parse_set(ctx, 3),
                "set4" => self.parse_set(ctx, 4),
                "jmp" | "jump" => self.parse_jump(ctx),
                "je" | "jeq" | "jc" | "jcp" => self.parse_cjump(ctx, ConditionalJumpMode::Equal),
                "jne" | "jnc" | "jcc" => self.parse_cjump(ctx, ConditionalJumpMode::NotEqual),

                _ => {
                    ctx.add_diag(Diagnostic::new(
                        format!("invalid instruction `{inst}`"),
                        self.current.span(),
                    ));
                    return Err(());
                }
            };
            Ok(inst)
        };

        let ret = inner();
        // eat until newline to prevent cascading errors
        if ctx.had_errs() {
            while !matches!(self.current.kind(), LexerToken::Newline | LexerToken::EoF) {
                self.bump(ctx);
            }
        }

        if ret.is_err() {
            warn!("parsing encountered \"fatal\" error starting at {span_start:?}, this probably should not happen");
        }

        let ret = match ret {
            Ok(inst) => inst,
            Err(_) => vec![],
        };

        Ok(ret)
    }

    fn parse_zero(&mut self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        self.bump(ctx);
        let span_start = self.current.span();

        let dst = match self.parse_reg(ctx) {
            Ok(dst) => dst,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("expected a register for zero instruction"),
                    self.current.span(),
                ));
                // use a dummy selector to allow recovery
                RegSelector::new_gpr(0, Span::DUMMY)
            }
        };

        let kind = InstructionKind::Sub { size: OpSize::Word, src: dst, dst: dst };

        let span = match Span::between(span_start, self.current.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("bad span for Zero instruction"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            kind,
            span
        )])
    }

    fn parse_skip(&mut self, ctx: &mut Context, count: usize) -> Vec<Instruction> {
        self.bump(ctx);

        vec![Instruction::new(InstructionKind::Raw { val: match count {
            1 => 0x0fd6,
            2 => 0x0f96,
            3 => 0x0f56,
            4 => 0x0f16,
            _ => {
                ctx.add_diag(Diagnostic::new(
                    format!("invalid skip count `{count}`"),
                    self.current.span(),
                ));
                0x0fd6 // Default to skip1 so parsing can continue
            }
        } }, self.current.span())]
    }

    fn parse_set(&mut self, ctx: &mut Context, count: usize) -> Vec<Instruction> {
        self.bump(ctx);

        // First operand must be a writeable register
        let dst = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        // Make sure it's writeable
        if !dst.is_gpr() {
            ctx.add_diag(Diagnostic::new(
                format!("expected dst to be a writable register, got {}", dst.idx()),
                dst.span(),
            ));
        }

        // Construct the store instruction (load reg.xyzw, [ri.x+])
        let reg = (dst.idx() as u16) << 12;
        let mut ivec = vec![Instruction::new(InstructionKind::Raw { val: match count {
            1 => 0xfd6 | reg,
            2 => 0xf96 | reg,
            3 => 0xf56 | reg,
            4 => 0xf16 | reg,
            _ => {
                ctx.add_diag(Diagnostic::new(
                    format!("invalid set count `{count}`"),
                    self.current.span(),
                ));
                0xfd6 | reg // Default to set1 so parsing can continue
            }
        } }, self.current.span())];

        // Parse in the literal values
        for _ in 0..count {
            if !self.eat(ctx, &LexerToken::Comma) {
                ctx.add_diag(Diagnostic::new(
                    format!("missing comma between parameters"),
                    self.current.span(),
                ));
            }
            match self.current.kind() {
                LexerToken::Label(label) => {
                    ivec.push(
                        Instruction::new(
                            InstructionKind::LabelLiteral { label: hash_label(label) },
                            self.current.span(),
                        )
                    );
                },
                LexerToken::Number(v) => {
                    let v = *v;
                    ivec.push(Instruction::new(
                        InstructionKind::Raw { val: v },
                        self.current.span(),
                    ));
                }
                _ => {
                    ctx.add_diag(Diagnostic::new(
                        format!("expected a number or label, got {:?}", self.current.kind()),
                        self.current.span(),
                    ));
                    continue;
                }
            }
            self.bump(ctx);
        }

        for v in &ivec {
            if matches!(v.kind(), InstructionKind::LabelLiteral { .. }) {
                self.bump(ctx);
            }
        }

        // Make sure we got the correct number of values
        if ivec.len() != count + 1 {
            ctx.add_diag(Diagnostic::new(
                format!("expected {count} values, got {}", ivec.len() - 1),
                self.current.span(),
            ));
        }

        ivec
    }

    fn parse_jump(&mut self, ctx: &mut Context) -> Vec<Instruction> {
        let span_start = self.current.span();

        // Jump instruction is actually a load ri.x, [ri.x+] >> !nnnn

        let mov = Instruction::new(
            InstructionKind::Load {
                mem: MemoryOperand::new(
                    RegSelector::new_gpr(7, self.current.span()),
                    false,
                    true,
                    self.current.span(),
                ),
                dst: SetRegSelector::new(
                    RegSelector::new_gpr(7, self.current.span()),
                    SetSelector::from_bits(0b1, self.current.span()),
                    self.current.span(),
                ),
            },
            self.current.span(),
        );

        self.bump(ctx);

        let k = match &self.current.kind() {
            LexerToken::Label(label) => {
                InstructionKind::LabelJump { label: hash_label(label) }
            },
            LexerToken::Number(num) => {
                InstructionKind::Raw { val: *num }
            },
            _ => {
                ctx.add_diag(Diagnostic::new(
                    String::from("expected a number as jump offset"),
                    self.current.span(),
                ));
                InstructionKind::Raw { val: 0 }
            }
        };

        self.bump(ctx);

        let span = match Span::between(span_start, self.current.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span for jump offset"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        let lit = Instruction::new(
            k,
            span,
        );

        vec![mov, lit]
    }

    fn parse_cjump(&mut self, ctx: &mut Context, mode: ConditionalJumpMode) -> Vec<Instruction> {
        let span_start = self.current.span();
        self.bump(ctx);

        let dst = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        let first = match mode {
            ConditionalJumpMode::Equal => {
                Instruction::new(
                    InstructionKind::NotDst { dst: dst.clone() },
                    dst.span(),
                )
            },
            ConditionalJumpMode::NotEqual => {
                Instruction::new(
                    InstructionKind::Nop,
                    dst.span(),
                )
            },
        };

        let sub = Instruction::new(
            InstructionKind::WSub {
                dst: RegSelector::new_gpr(7, dst.span()),
                // dst: RegSelector::new_gpr(7, dst.span()),
                src: SwizzleRegSelector::new(
                    dst.clone(),
                    SwizzleSelector::new(0, dst.span()),
                    dst.span(),
                ),
            },
            dst.span(),
        );

        self.bump(ctx);

        let skip = Instruction::new(
            InstructionKind::Raw {
                val: 0x0f96, // skip2
            },
            dst.span(),
        );

        let k = match &self.current.kind() {
            LexerToken::Label(label) => {
                InstructionKind::LabelJump { label: hash_label(label) }
            },
            LexerToken::Number(num) => {
                InstructionKind::Raw { val: *num }
            },
            _ => {
                ctx.add_diag(Diagnostic::new(
                    String::from("expected a valid jump offset"),
                    self.current.span(),
                ));
                InstructionKind::Raw { val: 0 }
            }
        };

        self.bump(ctx);

        let span = match Span::between(span_start, self.current.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span for jump offset"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        let lit = Instruction::new(
            k,
            span,
        );

        let mov = Instruction::new(
            InstructionKind::Load {
                mem: MemoryOperand::new(
                    RegSelector::new_gpr(7, self.current.span()),
                    false,
                    true,
                    self.current.span(),
                ),
                dst: SetRegSelector::new(
                    RegSelector::new_gpr(7, self.current.span()),
                    SetSelector::from_bits(0b1, self.current.span()),
                    self.current.span(),
                ),
            },
            self.current.span(),
        );

        vec![first, sub, skip, mov, lit]
    }

    fn parse_nop(&mut self, ctx: &mut Context) -> Vec<Instruction> {
        self.bump(ctx);
        vec![Instruction::new(InstructionKind::Nop, self.current.span())]
    }

    fn parse_halt(&mut self, ctx: &mut Context) -> Vec<Instruction> {
        self.bump(ctx);
        vec![Instruction::new(InstructionKind::Halt, self.current.span())]
    }

    fn parse_literal_raw(&mut self, ctx: &mut Context) -> Result<Instruction, Diagnostic> {
        let span_start = self.current.span();
        // self.bump(ctx);

        let raw = match self.current.kind() {
            LexerToken::Raw(val) => {
                let val = *val;
                self.bump(ctx);
                val
            },
            _ => {
                ctx.add_diag(Diagnostic::new(
                    format!("expected raw value, got {:?}", self.current.kind()),
                    self.current.span(),
                ));
                0
            }
        };

        let span = match Span::between(span_start, self.current.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span for raw value"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(Instruction::new(
            InstructionKind::Raw { val: raw },
            span,
        ))
    }

    fn parse_sleep(&mut self, ctx: &mut Context) -> Vec<Instruction> {
        self.bump(ctx);
        if !self.eat(ctx, &LexerToken::Dot) {
            let ticks = match self.current.kind() {
                LexerToken::Number(num) => {
                    let num = *num;
                    self.bump(ctx);
                    num as u8
                }
                _ => {
                    ctx.add_diag(Diagnostic::new(
                        String::from("expected a number as sleep ticks"),
                        self.current.span(),
                    ));
                    0u8
                }
            };
            return vec![Instruction::new(InstructionKind::Sleep { ticks }, self.current.span())];
        }

        let (byte, high) = match self.current.kind() {
            LexerToken::Ident(ident) => match ident.as_str() {
                "h" => (true, true),
                "l" => (true, false),
                "w" => (false, false),
                _ => {
                    ctx.add_diag(Diagnostic::new(
                        String::from("sleep from register needs a `.h`, `.l`, or `.w` to specify mode"),
                        self.current.span(),
                    ));
                    return vec![];
                }
            },
            _ => {
                ctx.add_diag(Diagnostic::new(
                    String::from("missing specifier `h`, `l`, or `w` after dot"),
                    self.current.span(),
                ));
                return vec![];
            }
        };

        self.bump(ctx);

        let reg = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        match (byte, high) {
            (true, true) => vec![Instruction::new(InstructionKind::Sleep8H { src: reg }, self.current.span())],
            (true, false) => vec![Instruction::new(InstructionKind::Sleep8L { src: reg }, self.current.span())],
            (false, _) => vec![Instruction::new(InstructionKind::Sleep16 { src: reg }, self.current.span())],
        }
    }

    fn parse_move(&mut self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        self.bump(ctx);

        let dst = self.parse_move_operand(ctx)?;

        if !self.eat(ctx, &LexerToken::Comma) {
            ctx.add_diag(Diagnostic::new(
                String::from("missing comma between dst and src"),
                self.current.span(),
            ));
            // allow recovery by not returning
        }

        let src = self.parse_move_operand(ctx)?;

        let (kind, span) = match (src, dst) {
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
                        Span::between(src.span(), dst.span()).unwrap_or(self.current.span()),
                    ));
                }

                // the dst must be a writable register
                if !dst.reg().is_gpr() {
                    ctx.add_diag(Diagnostic::new(
                        format!("expected dst to be a writable register, got {}", dst.reg()),
                        dst.span(),
                    ));
                }
                (InstructionKind::Move { src, dst }, src.span())
            }
            (LoadStoreOp::MemOp(mem), LoadStoreOp::RegOp(dst)) => {
                // the dst must be a writable register
                // if !dst.reg().is_gpr() {
                //     ctx.add_diag(Diagnostic::new(
                //         format!("expected dst to be a writable register, got {}", dst.reg()),
                //         dst.span(),
                //     ));
                // }

                (InstructionKind::Load { mem, dst }, mem.span())
            }
            (LoadStoreOp::RegOp(src), LoadStoreOp::MemOp(mem)) => {
                (InstructionKind::Store { src, mem }, src.span())
            },

            // mem-to-mem moves do not exist
            (LoadStoreOp::MemOp(_), LoadStoreOp::MemOp(_)) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("cannot move between two pointers directly"),
                    self.current.span(),
                ));
                return Err(());
            },
        };

        let span = match Span::between(span_start, span) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            kind,
            span,
        )])
    }

    fn parse_swizzle(&mut self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        self.bump(ctx);

        let dst = self.parse_swizzle_reg(ctx)?;
        // the dst must be a writable register
        if !dst.reg().is_gpr() {
            ctx.add_diag(Diagnostic::new(
                format!("expected dst to be a writable register, got {}", dst.reg()),
                dst.span(),
            ));
        }

        let span = match Span::between(span_start, dst.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            InstructionKind::Swizzle { reg: dst },
            span,
        )])
    }

    fn parse_wselect(&mut self, ctx: &mut Context, mode: WSelectMode) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        self.bump(ctx);

        let dst = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        if !self.eat(ctx, &LexerToken::Comma) {
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

        let span = match Span::between(span_start, src.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            kind,
            span,
        )])
    }

    fn parse_add(&mut self, ctx: &mut Context, mode: AddMode) -> Result<Vec<Instruction>, ()> {
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
                Span::between(span_start, rhs.span()).unwrap_or(Span::DUMMY),
            ));

            // pick lhs to be the src arbitrarily
            // to create a dummy instruction for further parsing
            lhs
        };

        let kind = match mode {
            AddMode::Normal => InstructionKind::Add { size, src, dst },
            AddMode::Saturate => InstructionKind::AddSaturate { size, src, dst },
            AddMode::Over => InstructionKind::AddOver { size, src, dst },
        };

        let span = match Span::between(span_start, rhs.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            kind,
            span,
        )])
    }

    fn parse_sub(&mut self, ctx: &mut Context, mode: SubMode) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        let (size, dst, lhs, rhs) = self.parse_math_common(ctx)?;

        let kind = if dst == lhs {
            // dst = lhs - rhs
            // dst = dst - rhs
            let src = rhs;
            match mode {
                SubMode::Normal => InstructionKind::RSub { size, src, dst},
                SubMode::Saturate => InstructionKind::SubRevSaturate { size, src, dst, },
                SubMode::Over => InstructionKind::SubOver { size, src, dst, },
                SubMode::ROver => InstructionKind::RSubOver { size, src, dst, },
            }
        } else if dst == rhs {
            // dst = lhs - rhs
            // dst = lhs - dst
            let src = lhs;
            match mode {
                SubMode::Normal => InstructionKind::Sub { size, src, dst, },
                SubMode::Saturate => InstructionKind::SubSaturate { size, src, dst, },
                SubMode::Over => InstructionKind::SubOver { size, src, dst, },
                SubMode::ROver => InstructionKind::RSubOver { size, src, dst, },
            }
        } else {
            // sub could not be encoded in only two registers, error
            ctx.add_diag(Diagnostic::new(
                String::from("sub must be of the form `dst = dst - src` or `dst = src - dst`"),
                Span::between(span_start, rhs.span()).unwrap_or(Span::DUMMY),
            ));

            // generate a dummy sub instruction to allow for further parsing
            InstructionKind::Sub {
                size,
                src: lhs,
                dst: rhs,
            }
        };

        let span = match Span::between(span_start, rhs.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            kind,
            span,
        )])
    }

    fn parse_carry(&mut self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        let (size, dst, lhs, rhs) = self.parse_math_common(ctx)?;

        let kind = if dst == lhs {
            let src = rhs;
            InstructionKind::Carry { size, src, dst }
        } else if dst == rhs {
            let src = lhs;
            InstructionKind::Carry { size, src, dst }
        } else {
            // generate a dummy instruction to allow for further parsing
            InstructionKind::Eq {
                size,
                src: lhs,
                dst: rhs,
            }
        };

        let span = match Span::between(span_start, rhs.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            kind,
            span,
        )])
    }

    fn parse_cmp(&mut self, ctx: &mut Context, mode: CmpMode) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        let (size, dst, lhs, rhs) = self.parse_math_common(ctx)?;

        let kind = if dst == lhs {
            // dst = lhs cmp rhs
            // dst = dst cmp src
            let src = rhs;

            match mode {
                CmpMode::Eq => InstructionKind::Eq { size, src, dst },
                CmpMode::Neq => InstructionKind::NotEq { size, src, dst },
                CmpMode::LessU => InstructionKind::LessU { size, src, dst },
                CmpMode::GreaterU => InstructionKind::GreaterU { size, src, dst },
                CmpMode::LessEqU => InstructionKind::LessEqU { size, src, dst },
                CmpMode::GreaterEqU => InstructionKind::GreaterEqU { size, src, dst },
            }
        } else if dst == rhs {
            // dst = lhs cmp rhs
            // dst = src cmp dst
            let src = lhs;

            match mode {
                CmpMode::Eq => InstructionKind::Eq { size, src, dst },
                CmpMode::Neq => InstructionKind::NotEq { size, src, dst },
                CmpMode::LessU => InstructionKind::LessU { size, src, dst },
                CmpMode::GreaterU => InstructionKind::GreaterU { size, src, dst },
                CmpMode::LessEqU => InstructionKind::LessEqU { size, src, dst },
                CmpMode::GreaterEqU => InstructionKind::GreaterEqU { size, src, dst },
            }
        } else {
            // cmp could not be encoded in only two registers, error
            ctx.add_diag(Diagnostic::new(
                String::from("compare instructions must be of the form `dst = dst <cmp> src` or `dst = src <cmp> dst`"),
                Span::between(span_start, rhs.span()).unwrap_or(Span::DUMMY),
            ));

            // generate a dummy instruction to allow for further parsing
            InstructionKind::Eq {
                size,
                src: lhs,
                dst: rhs,
            }
        };

        let span = match Span::between(span_start, rhs.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            kind,
            span,
        )])
    }

    fn parse_spec(&mut self, ctx: &mut Context, mode: SpecOpMode) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        self.bump(ctx);

        let lhs = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        if !self.eat(ctx, &LexerToken::Comma) {
            ctx.add_diag(Diagnostic::new(
                String::from("missing comma between lhs and rhs"),
                self.current.span(),
            ));
            // allow recovery by not returning
        }

        let rhs = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        let span = match Span::between(span_start, rhs.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![
            Instruction::new(
                match mode {
                    SpecOpMode::HorizontalAdd => InstructionKind::HorizontalAdd { src: lhs, dst: rhs },
                    SpecOpMode::MultiplySaturate => InstructionKind::MultiplySaturate { src: lhs, dst: rhs },
                    SpecOpMode::MultiplyLow => InstructionKind::MultiplyLow { src: lhs, dst: rhs },
                    SpecOpMode::MultiplyHigh => InstructionKind::MultiplyHigh { src: lhs, dst: rhs },
                    SpecOpMode::Divide => InstructionKind::Divide { src: lhs, dst: rhs },
                    SpecOpMode::ReciprocalDivide => InstructionKind::ReciprocalDivide { src: lhs, dst: rhs },
                },
                span,
            )
        ])
    }

    fn parse_lsl(&mut self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        let (size, dst, amount) = self.parse_shift_common(ctx)?;

        let span = match Span::between(span_start, amount.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            InstructionKind::ShiftLeft { size, dst, amount },
            span,
        )])
    }

    fn parse_rol(&mut self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        let (size, dst, amount) = self.parse_shift_common(ctx)?;

        let span = match Span::between(span_start, amount.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            InstructionKind::RotateLeft { size, dst, amount },
            span,
        )])
    }

    fn parse_lsr(&mut self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        let (size, dst, amount) = self.parse_shift_common(ctx)?;

        let span = match Span::between(span_start, amount.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            InstructionKind::ShiftRightLogical { size, dst, amount },
            span,
        )])
    }

    fn parse_asr(&mut self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        let (size, dst, amount) = self.parse_shift_common(ctx)?;

        let span = match Span::between(span_start, amount.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            InstructionKind::ShiftRightArithmetic { size, dst, amount },
            span,
        )])
    }

    fn parse_ror(&mut self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        let (size, dst, amount) = self.parse_shift_common(ctx)?;

        let span = match Span::between(span_start, amount.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            InstructionKind::RotateRight { size, dst, amount },
            span,
        )])
    }

    fn parse_bit_op(&mut self, ctx: &mut Context, mode: BitOpMode) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        let (dst, src) = self.parse_bitop_common(ctx)?;

        let kind = match mode {
            BitOpMode::And => InstructionKind::And { src, dst },
            BitOpMode::Or => InstructionKind::Or { src, dst },
            BitOpMode::Xor => InstructionKind::Xor { src, dst },

            BitOpMode::Xand => InstructionKind::Nand { src, dst },
            BitOpMode::Nor => InstructionKind::Nor { src, dst },
            BitOpMode::Xnor => InstructionKind::XNor { src, dst },

            BitOpMode::Swap => InstructionKind::Swap { src, dst },

            BitOpMode::NotSrc => InstructionKind::NotSrc { src, dst },

            BitOpMode::SrcAndNotDst => InstructionKind::SrcAndNotDst { src, dst },
            BitOpMode::NotSrcAndDst => InstructionKind::NotSrcAndDst { src, dst },
            BitOpMode::SrcOrNotDst => InstructionKind::SrcOrNotDst { src, dst },
            BitOpMode::NotSrcOrDst => InstructionKind::NotSrcOrDst { src, dst },
            _ => {
                ctx.add_diag(Diagnostic::new(
                    String::from("tried parsing a binary bitwise op as a unary one"),
                    self.current.span(),
                ));
                return Err(());
            }
        };

        let span = match Span::between(span_start, src.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            kind,
            span,
        )])
    }

    fn parse_bit_op_unary(&mut self, ctx: &mut Context, mode: BitOpMode) -> Result<Vec<Instruction>, ()> {
        let span_start = self.current.span();
        self.bump(ctx);

        if self.eat(ctx, &LexerToken::Dot) {
            ctx.add_diag(Diagnostic::new(
                String::from("bitwise operands do not take a size parameter"),
                self.current.span(),
            ));
            return Err(());
        }

        let mut was_err = false;
        let dst = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });
        // the dst must be a writable register
        if !was_err && !dst.is_gpr() {
            ctx.add_diag(Diagnostic::new(
                format!("expected dst to be a writable register, got {dst}"),
                dst.span(),
            ));
        }

        let kind = match mode {
            BitOpMode::NotDst => InstructionKind::NotDst { dst },
            BitOpMode::One => InstructionKind::One { dst },
            BitOpMode::All => InstructionKind::All { dst },
            _ => {
                ctx.add_diag(Diagnostic::new(
                    String::from("tried parsing a unary bitwise op as a binary one"),
                    self.current.span(),
                ));
                return Err(());
            }
        };

        let span = match Span::between(span_start, dst.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(vec![Instruction::new(
            kind,
            span,
        )])
    }

    // =======================
    // utilities
    // =======================

    fn parse_math_common(
        &mut self,
        ctx: &mut Context,
    ) -> Result<(OpSize, RegSelector, RegSelector, RegSelector), ()> {
        let span_start = self.current.span();
        self.bump(ctx);

        if !self.eat(ctx, &LexerToken::Dot) {
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
        self.bump(ctx);

        let mut was_reg_err = true;
        let dst = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });
        if !self.eat(ctx, &LexerToken::Comma) {
            ctx.add_diag(Diagnostic::new(
                String::from("missing comma between dst and lhs"),
                self.current.span(),
            ));
            // allow recovery by not returning
        }

        let lhs = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });
        if !self.eat(ctx, &LexerToken::Comma) {
            if matches!(self.current.kind(), LexerToken::Newline) {
                // the user probably assumed this was a two argument math op of the form dst, src
                // but it actually is three, dst, lhs, rhs
                ctx.add_diag(Diagnostic::new(
                    String::from("math operands are of the form `op dst, lhs, rhs`"),
                    Span::between(span_start, self.current.span()).unwrap_or(Span::DUMMY),
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

        let rhs = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        // the dst must be a writable register
        if !was_reg_err && !dst.is_gpr() {
            ctx.add_diag(Diagnostic::new(
                format!("expected dst to be a writable register, got {dst}"),
                dst.span(),
            ));
        }

        Ok((size, dst, lhs, rhs))
    }

    fn parse_shift_common(
        &mut self,
        ctx: &mut Context,
    ) -> Result<(OpSize, RegSelector, ShiftAmount), ()> {
        self.bump(ctx);

        if !self.eat(ctx, &LexerToken::Dot) {
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
        self.bump(ctx);

        let mut was_reg_err = false;
        // if there was an error parsing the dst register, use a dummy selector
        // to allow parsing to continue
        let dst = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        if !self.eat(ctx, &LexerToken::Comma) {
            ctx.add_diag(Diagnostic::new(
                String::from("missing comma between lhs and rhs"),
                self.current.span(),
            ));
            // allow recovery by not returning
        }

        let amount = match self.current.kind() {
            LexerToken::Ident(_) => ShiftAmount::Register(self.parse_reg(ctx).unwrap_or_else(|d| {
                ctx.add_diag(d);
                was_reg_err = true;
                // use a dummy selector to allow recovery
                RegSelector::new_gpr(0, Span::DUMMY)
            })),
            LexerToken::Number(num) => {
                let span = self.current.span();
                let num = *num;
                self.bump(ctx);

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
                format!("expected dst to be a writable register, got {dst}"),
                dst.span(),
            ));
        }

        Ok((size, dst, amount))
    }

    /// parses a bitwise operation of the form `OP dst, src`.
    /// this does not need to have three arguments because all bitwise operations
    /// can be executed with the lhs and rhs in either order.
    /// returns (dst, src)
    fn parse_bitop_common(&mut self, ctx: &mut Context) -> Result<(RegSelector, RegSelector), ()> {
        self.bump(ctx);

        if self.eat(ctx, &LexerToken::Dot) {
            ctx.add_diag(Diagnostic::new(
                String::from("bitwise operands do not take a size parameter"),
                self.current.span(),
            ));
            return Err(());
        }

        let mut was_reg_err = false;
        let dst = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        if !self.eat(ctx, &LexerToken::Comma) {
            ctx.add_diag(Diagnostic::new(
                String::from("missing comma between dst and src"),
                self.current.span(),
            ));
            // allow recovery by not returning
        }

        let src = self.parse_reg(ctx).unwrap_or_else(|d| {
            ctx.add_diag(d);
            was_reg_err = true;
            // use a dummy selector to allow recovery
            RegSelector::new_gpr(0, Span::DUMMY)
        });

        // the dst must be a writable register
        if !was_reg_err && !dst.is_gpr() {
            ctx.add_diag(Diagnostic::new(
                format!("expected dst to be a writable register, got {dst}"),
                dst.span(),
            ));
        }

        Ok((dst, src))
    }

    fn parse_move_operand(&mut self, ctx: &mut Context) -> Result<LoadStoreOp, ()> {
        let span_start = self.current.span();
        let is_mem = self.eat(ctx, &LexerToken::LeftBracket);

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

            let increment = self.eat(ctx, &LexerToken::Plus);

            if !self.eat(ctx, &LexerToken::RightBracket) {
                ctx.add_diag(Diagnostic::new(
                    String::from("missing `]` after memory operand"),
                    self.current.span(),
                ));
                // it's probably just missing, recover
            }

            let span = match Span::between(span_start, self.current.span()) {
                Ok(span) => span,
                Err(_) => {
                    ctx.add_diag(Diagnostic::new(
                        String::from("invalid span"),
                        self.current.span(),
                    ));
                    Span::DUMMY
                }
            };

            Ok(LoadStoreOp::MemOp(MemoryOperand::new(
                set.reg(),
                all,
                increment,
                span,
            )))
        } else {
            Ok(LoadStoreOp::RegOp(set))
        }
    }

    fn parse_reg(&mut self, ctx: &mut Context) -> Result<RegSelector, Diagnostic> {
        let span = self.current.span();
        let name = self.expect_ident().map_err(|d| {
            Diagnostic::new(
                format!("expected register name got {}", self.current.kind()),
                d.span(),
            )
        })?;
        let reg = if let Some(idx) = name.strip_prefix('r') {
            // ri is an alias for r7
            if idx == "i" {
                RegSelector::new_gpr(7, span)
            } else {
                let idx = match idx.parse::<u8>() {
                    Ok(idx) if idx <= MAX_REG_IDX => idx,
                    _ => {
                        return Err(Diagnostic::new(
                            format!("invalid register `{idx}`"),
                            self.current.span(),
                        )
                        .with_note(format!("maximum register index is {MAX_REG_IDX}")))
                    }
                };
                RegSelector::new_gpr(idx, span)
            }
        } else if let Some(idx) = name.strip_prefix('c') {
            let idx = match idx.parse::<u8>() {
                Ok(idx) if idx <= MAX_REG_IDX => idx,
                _ => {
                    return Err(Diagnostic::new(
                        format!("invalid register `{idx}`"),
                        self.current.span(),
                    )
                    .with_note(format!("maximum register index is {MAX_REG_IDX}")))
                }
            };
            RegSelector::new_const(idx, span)
        } else {
            return Err(Diagnostic::new(
                format!("invalid register `{name}`"),
                self.current.span(),
            ));
        };
        self.bump(ctx);
        Ok(reg)
    }

    fn parse_set_reg(
        &mut self,
        ctx: &mut Context,
        select_mode: SelectorMode,
    ) -> Result<SetRegSelector, ()> {
        let reg = self.parse_reg(ctx).map_err(|d| {
            ctx.add_diag(d);
        })?;

        // if !self.eat(ctx, &TokenKind::Dot) {
        //     // Assume xyzw as default
        //     ctx.add_diag(Diagnostic::new(
        //         String::from("expected a `.` between a register name and its element selector"),
        //         self.current.span(),
        //     ));
        //     return Err(());
        // }

        let selector = if self.eat(ctx, &LexerToken::Dot) {
            self.parse_set_selector(ctx, select_mode)?
        } else {
            // Assume xyzw as default
            SetSelector::from_bits(0b1111, reg.span())
        };

        let span = match Span::between(reg.span(), selector.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(SetRegSelector::new(
            reg,
            selector,
            span,
        ))
    }

    fn parse_swizzle_reg(&mut self, ctx: &mut Context) -> Result<SwizzleRegSelector, ()> {
        let reg = self.parse_reg(ctx).map_err(|d| {
            ctx.add_diag(d);
        })?;

        if !self.eat(ctx, &LexerToken::Dot) {
            ctx.add_diag(Diagnostic::new(
                String::from("expected a `.` between a register name and its element selector"),
                self.current.span(),
            ));
            return Err(());
        }

        let selector = self.parse_swizzle_selector(ctx)?;

        let span = match Span::between(reg.span(), selector.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(SwizzleRegSelector::new(
            reg,
            selector,
            span,
        ))
    }

    fn parse_swizzle_single_reg(&mut self, ctx: &mut Context) -> Result<SwizzleRegSelector, ()> {
        let reg = self.parse_reg(ctx).map_err(|d| {
            ctx.add_diag(d);
        })?;

        if !self.eat(ctx, &LexerToken::Dot) {
            ctx.add_diag(Diagnostic::new(
                String::from("expected a `.` between a register name and its element selector"),
                self.current.span(),
            ));
            return Err(());
        }

        let selector = self.parse_swizzle_single(ctx)?;

        let span = match Span::between(reg.span(), selector.span()) {
            Ok(span) => span,
            Err(_) => {
                ctx.add_diag(Diagnostic::new(
                    String::from("invalid span"),
                    self.current.span(),
                ));
                Span::DUMMY
            }
        };

        Ok(SwizzleRegSelector::new(
            reg,
            selector,
            span,
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

        self.bump(ctx);
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
                                format!("invalid register selector {select_str}"),
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
                                format!("`{c}` already present in selector"),
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

        self.bump(ctx);
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

        self.bump(ctx);
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

    fn bump(&mut self, ctx: &mut Context) {
        let current = match self.lexer.next_token() {
            Ok(token) => token,
            Err(err) => {
                ctx.add_diag(err);
                return;
            }
        };
        self.current = current;
    }

    fn expect_label(&self) -> Result<String, Diagnostic> {
        match self.current.kind() {
            LexerToken::Label(s) => Ok(s.clone()),
            LexerToken::LabelDef(s) => Ok(s.clone()),
            other => Err(Diagnostic::new(
                format!("expected label, found `{other}`"),
                self.current.span(),
            )),
        }
    }

    fn expect_ident(&self) -> Result<String, Diagnostic> {
        match self.current.kind() {
            LexerToken::Ident(s) => Ok(s.clone()),
            other => Err(Diagnostic::new(
                format!("expected identifier, found `{other}`"),
                self.current.span(),
            )),
        }
    }

    /// expects the current token to be a specific kind, and eats it if it matches
    /// returns true if the token was eaten, false otherwise
    fn eat(&mut self, ctx: &mut Context, kind: &LexerToken) -> bool {
        if self.current.kind() == kind {
            self.bump(ctx);
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
    Over,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SubMode {
    Normal,
    Saturate,
    Over,
    ROver,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CmpMode {
    Eq,
    Neq,
    GreaterU,
    LessU,
    GreaterEqU,
    LessEqU,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BitOpMode {
    One,
    All,

    And,
    Or,
    Xor,

    Xand,
    Nor,
    Xnor,

    Swap,

    NotSrc,
    NotDst,

    SrcAndNotDst,
    NotSrcAndDst,
    SrcOrNotDst,
    NotSrcOrDst,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SpecOpMode {
    HorizontalAdd,
    MultiplySaturate,
    MultiplyLow,
    MultiplyHigh,
    Divide,
    ReciprocalDivide,
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

#[derive(Debug, Clone, Copy)]
enum ConditionalJumpMode {
    Equal,
    NotEqual,
}
