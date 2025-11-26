//! Source map generation for RustScript.
//! 
//! Author: Michael Lauzon
//! 
//! This module generates Source Map v3 files that map generated code (JavaScript
//! or WebAssembly) back to the original RustScript source. This enables debugging
//! generated code in browser developer tools with the original source.
//! 
//! Source maps use VLQ (Variable-Length Quantity) encoding for compact representation.

use serde::Serialize;

/// A Source Map v3 structure.
/// 
/// Source maps enable debugging of generated code by mapping locations in the
/// generated output back to the original source files.
#[derive(Serialize)]
pub struct SourceMap {
    /// Source map version (always 3).
    version: i32,
    /// The generated file this source map is for.
    file: String,
    /// List of original source file paths.
    sources: Vec<String>,
    /// List of symbol names (for renaming).
    names: Vec<String>,
    /// VLQ-encoded mapping data.
    mappings: String,
}

/// Generates source maps incrementally as code is generated.
/// 
/// # Example
/// 
/// ```ignore
/// let mut generator = SourceMapGenerator::new("output.js".to_string());
/// let source_idx = generator.add_source("input.rjsc".to_string());
/// generator.add_mapping(0, 0, source_idx, 0, 0);
/// let source_map = generator.generate();
/// ```
pub struct SourceMapGenerator {
    /// The output file name.
    file: String,
    /// Original source file paths.
    sources: Vec<String>,
    /// Mapping entries to be encoded.
    mappings: Vec<Mapping>,
}

/// A single mapping from generated code to original source.
struct Mapping {
    /// Line number in generated code (0-indexed).
    generated_line: u32,
    /// Column number in generated code (0-indexed).
    generated_column: u32,
    /// Index into the sources array.
    original_file_index: u32,
    /// Line number in original source (0-indexed).
    original_line: u32,
    /// Column number in original source (0-indexed).
    original_column: u32,
    /// Optional index into the names array.
    name_index: Option<u32>,
}

impl SourceMapGenerator {
    pub fn new(file: String) -> Self {
        Self {
            file,
            sources: Vec::new(),
            mappings: Vec::new(),
        }
    }

    pub fn add_source(&mut self, source_path: String) -> u32 {
        self.sources.push(source_path);
        (self.sources.len() - 1) as u32
    }

    pub fn add_mapping(
        &mut self,
        gen_line: u32,
        gen_col: u32,
        orig_file_idx: u32,
        orig_line: u32,
        orig_col: u32,
    ) {
        self.mappings.push(Mapping {
            generated_line: gen_line,
            generated_column: gen_col,
            original_file_index: orig_file_idx,
            original_line: orig_line,
            original_column: orig_col,
            name_index: None,
        });
    }

    /// Generates the final source map with VLQ-encoded mappings.
    /// 
    /// This method encodes all mappings using Variable-Length Quantity encoding
    /// and produces a complete Source Map v3 structure.
    pub fn generate(&self) -> SourceMap {
        let mut mappings = String::new();
        let mut last_gen_line = 0;
        let mut last_gen_col = 0;
        let mut last_source_idx = 0;
        let mut last_orig_line = 0;
        let mut last_orig_col = 0;
        let mut last_name_idx = 0;

        for (i, mapping) in self.mappings.iter().enumerate() {
            if i > 0 {
                if mapping.generated_line != last_gen_line {
                    for _ in 0..(mapping.generated_line - last_gen_line) {
                        mappings.push(';');
                    }
                    last_gen_col = 0;
                } else {
                    mappings.push(',');
                }
            }

            // Generated column
            self.encode_vlq(&mut mappings, (mapping.generated_column as i32) - (last_gen_col as i32));
            last_gen_col = mapping.generated_column;

            // Original source index
            self.encode_vlq(&mut mappings, (mapping.original_file_index as i32) - (last_source_idx as i32));
            last_source_idx = mapping.original_file_index;

            // Original line
            self.encode_vlq(&mut mappings, (mapping.original_line as i32) - (last_orig_line as i32));
            last_orig_line = mapping.original_line;

            // Original column
            self.encode_vlq(&mut mappings, (mapping.original_column as i32) - (last_orig_col as i32));
            last_orig_col = mapping.original_column;

            // Name index (optional)
            if let Some(name_idx) = mapping.name_index {
                self.encode_vlq(&mut mappings, (name_idx as i32) - (last_name_idx as i32));
                last_name_idx = name_idx;
            }
            
            last_gen_line = mapping.generated_line;
        }

        SourceMap {
            version: 3,
            file: self.file.clone(),
            sources: self.sources.clone(),
            names: Vec::new(),
            mappings,
        }
    }

    fn encode_vlq(&self, output: &mut String, value: i32) {
        let mut value = if value < 0 {
            ((-value) << 1) | 1
        } else {
            value << 1
        };

        loop {
            let mut digit = value & 0x1F;
            value >>= 5;
            if value > 0 {
                digit |= 0x20;
            }
            output.push(self.base64_char(digit));
            if value == 0 {
                break;
            }
        }
    }

    fn base64_char(&self, value: i32) -> char {
        const CHARS: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        CHARS[value as usize] as char
    }
}
