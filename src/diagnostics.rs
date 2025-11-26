//! Diagnostic reporting for RustScript.
//! 
//! Author: Michael Lauzon
//! 
//! This module provides high-quality error messages with source context,
//! similar to rustc's error reporting. It uses the codespan-reporting crate
//! to generate formatted diagnostics with colour and source snippets.

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use crate::ast::Span;

/// Manages source files and generates diagnostic messages.
/// 
/// The diagnostic manager tracks all source files in a compilation session
/// and can generate formatted error messages with source context.
/// 
/// # Example
/// 
/// ```ignore
/// let mut diagnostics = DiagnosticManager::new();
/// let file_id = diagnostics.add_file("main.rjsc".to_string(), source);
/// let error_msg = diagnostics.report_error(span, "Type mismatch");
/// ```
pub struct DiagnosticManager {
    /// Storage for source files and their contents.
    files: SimpleFiles<String, String>,
    /// List of file IDs that have been added.
    file_ids: Vec<usize>,
}

impl DiagnosticManager {
    pub fn new() -> Self {
        Self {
            files: SimpleFiles::new(),
            file_ids: Vec::new(),
        }
    }

    /// Adds a source file to the diagnostic manager.
    /// 
    /// Returns a file ID that can be used in span information.
    pub fn add_file(&mut self, name: String, source: String) -> usize {
        let file_id = self.files.add(name, source);
        self.file_ids.push(file_id);
        file_id
    }

    /// Generates a formatted error message with source context.
    /// 
    /// The error message includes:
    /// - The error message
    /// - The source file name and line number
    /// - A snippet of the source code with the error highlighted
    /// 
    /// # Arguments
    /// 
    /// * `span` - The location in source where the error occurred
    /// * `message` - A description of the error
    /// 
    /// # Returns
    /// 
    /// A formatted error message as a string
    pub fn report_error(&self, span: Span, message: &str) -> String {
        let diagnostic = Diagnostic::error()
            .with_message(message)
            .with_labels(vec![
                Label::primary(span.file_id, span.start..span.end)
                    .with_message("error occurred here"),
            ]);

        // Capture output to a buffer for WASM compatibility
        let mut writer = codespan_reporting::term::termcolor::NoColor::new(Vec::new());
        let config = codespan_reporting::term::Config::default();

        #[allow(deprecated)]
        codespan_reporting::term::emit(&mut writer, &config, &self.files, &diagnostic)
            .expect("Failed to emit diagnostic");
        
        String::from_utf8(writer.into_inner())
            .expect("Diagnostic output contained invalid UTF-8")
    }
}
