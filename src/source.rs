use std::{fs, io, path::Path};

/// contains info about the source of a file
#[derive(Debug)]
pub struct Source {
    filename: String,
    src: String,
    /// the byte offset into the source that corresponds to the start of each line
    line_starts: Vec<u32>,
}

impl Source {
    pub fn new_from_file(path: impl AsRef<Path>) -> io::Result<Source> {
        let filename = path.as_ref().display().to_string();
        let src = fs::read_to_string(path)?;
        let line_starts = gen_lines(src.as_str());

        Ok(Self {
            filename,
            src,
            line_starts,
        })
    }

    pub fn filename(&self) -> &str {
        self.filename.as_str()
    }

    pub fn src(&self) -> &str {
        self.src.as_str()
    }

    pub fn line_starts(&self) -> &[u32] {
        self.line_starts.as_slice()
    }
}

fn gen_lines(src: &str) -> Vec<u32> {
    // the first line starts at offset 0
    let mut lines = vec![0_u32];
    let mut pos = 0;
    for c in src.chars() {
        if c == '\n' {
            // a line starts immediately after this line
            lines.push(pos + 1);
        }

        pos += c.len_utf8() as u32;
    }

    lines
}
