use std::fmt::{UpperHex, LowerHex};

pub struct ArrayPrinter<'a>(pub &'a [u16]);

impl<'a> UpperHex for ArrayPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut it = self.0.iter();
        if let Some(item) = it.next() {
            write!(f, "{item:04X}")?;
        }
        for item in it {
            write!(f, " {item:04X}")?;
        }
        write!(f, "]")?;

        Ok(())
    }
}

pub struct CodePrinter<'a>(pub &'a [u16]);

impl<'a> LowerHex for CodePrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut count: usize = 0;
        let it = self.0.iter();
        for item in it {
            write!(f, "{item:04x}")?;
            count += 1;
            if count % 16 == 0 {
                #[allow(clippy::write_with_newline)]
                write!(f, "\n")?;
            } else {
                write!(f, " ")?;
            }
        }

        Ok(())
    }
}
