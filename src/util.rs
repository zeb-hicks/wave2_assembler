use std::fmt::UpperHex;

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
