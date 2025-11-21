use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::rc::Rc;
use crate::ast::Span;

/// Manages source files and diagnostic reporting.
pub struct DiagnosticManager {
    files: SimpleFiles<String, String>,
    file_ids: Vec<usize>,
}

impl DiagnosticManager {
    pub fn new() -> Self {
        Self {
            files: SimpleFiles::new(),
            file_ids: Vec::new(),
        }
    }

    /// Add a source file to the manager.
    pub fn add_file(&mut self, name: String, source: String) -> usize {
        let file_id = self.files.add(name, source);
        self.file_ids.push(file_id);
        file_id
    }

    /// Report an error with a span and message.
    pub fn report_error(&self, span: Span, message: &str) -> String {
        let diagnostic = Diagnostic::error()
            .with_message(message)
            .with_labels(vec![
                Label::primary(span.file_id, span.start..span.end).with_message("here"),
            ]);

        // Capture output to a buffer for WASM compatibility/library usage.
        let mut writer = codespan_reporting::term::termcolor::NoColor::new(Vec::new());
        let config = codespan_reporting::term::Config::default();

        codespan_reporting::term::emit(&mut writer, &config, &self.files, &diagnostic).unwrap();
        
        String::from_utf8(writer.into_inner()).unwrap()
    }
}
