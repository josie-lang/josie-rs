//! `jsminify` — JavaScript minification for josie-web export pipeline.
//!
//! This module provides a pure-Rust JavaScript minifier designed for the
//! josie-web export pipeline. It transforms JavaScript source code to reduce
//! payload size while maintaining functional equivalence.
//!
//! # Implementation Stages
//!
//! The minifier is implemented in stages, each building on the previous:
//!
//! ## Stage 1: Strip Comments
//!
//! Removes `/* ... */` block comments and `// ...` line comments.
//! Does NOT strip comments inside string literals (`"..."`, `'...'`, `` `...` ``).
//!
//! ## Stage 2: Collapse Whitespace
//!
//! Reduces runs of whitespace (spaces, tabs, newlines) to a single space
//! where required by grammar, or nothing where not (around punctuation).
//!
//! ## Stage 3: Shorten Local Identifiers (NOT YET IMPLEMENTED)
//!
//! Renames `let`/`const`/`var` declared names that are purely local to short
//! names (`a`, `b`, ... `z`, `aa`, ...). Requires scope analysis.
//!
//! ## Stage 4: Remove Dead Branches (NOT YET IMPLEMENTED)
//!
//! Constant-condition branches like `if (false) { ... }` or `true ? a : b → a`.
//!
//! # Constraints
//!
//! - Pure Rust, no shell-out to `node`/`esbuild`/`terser`.
//! - Must not panic on any valid ES2015 input — return `Err` instead.
//! - If unsure about a construct, emit it unchanged (conservative fallback).
//! - `minify_js` must be deterministic: same input → same output always.

/// Parsing state for the comment-stripping state machine.
enum State {
    Normal,
    InString(char),
    AfterSlash,
    InStringEscape(char),
    InBlockCommentEnd,
    InBlockComment,
    InLineComment,
}

/// Strips comments from JavaScript source code.
///
/// This is Stage 1 of the minification pipeline.
pub fn strip_comments(input: &str) -> Result<String, String> {
    let mut output = String::with_capacity(input.len());
    let mut state = State::Normal;

    for ch in input.chars() {
        match state {
            State::Normal => match ch {
                '"' | '\'' | '`' => {
                    output.push(ch);
                    state = State::InString(ch);
                }
                '/' => {
                    output.push(ch);
                    state = State::AfterSlash;
                }
                '\n' | '\r' => {
                    output.push(ch);
                }
                _ => {
                    output.push(ch);
                }
            },
            State::AfterSlash => match ch {
                '*' => {
                    state = State::InBlockComment;
                    output.pop();
                }
                '/' => {
                    state = State::InLineComment;
                    output.pop();
                }
                _ => {
                    // Not a comment opener. Keep the current char; otherwise
                    // constructs like regex/division lose one byte and break syntax.
                    output.push(ch);
                    state = State::Normal;
                }
            },
            State::InString(quote) => {
                output.push(ch);
                if ch == '\\' {
                    state = State::InStringEscape(quote);
                } else if ch == quote {
                    state = State::Normal;
                }
            }
            State::InStringEscape(prev_quote) => {
                output.push(ch);
                state = State::InString(prev_quote);
            }
            State::InBlockComment => {
                if ch == '*' {
                    state = State::InBlockCommentEnd;
                }
            }
            State::InBlockCommentEnd => {
                if ch == '/' {
                    state = State::Normal;
                } else if ch != '*' {
                    state = State::InBlockComment;
                }
            }
            State::InLineComment => {
                if ch == '\n' || ch == '\r' {
                    output.push(ch);
                    state = State::Normal;
                }
            }
        }
    }

    if matches!(state, State::InBlockCommentEnd) {
        output.push('/');
    }

    Ok(output)
}

/// Collapses unnecessary whitespace in JavaScript source code.
///
/// This is Stage 2 of the minification pipeline.
pub fn collapse_whitespace(input: &str) -> Result<String, String> {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    let mut in_string = false;
    let mut string_char = ' ';
    let mut prev_char = '\0';
    let mut pending_space = false;

    while let Some(ch) = chars.next() {
        if in_string {
            out.push(ch);
            if ch == string_char && prev_char != '\\' {
                in_string = false;
            }
            prev_char = ch;
            continue;
        }

        if ch == '"' || ch == '\'' || ch == '`' {
            if pending_space {
                maybe_push_space(&mut out, ch);
                pending_space = false;
            }
            in_string = true;
            string_char = ch;
            out.push(ch);
            prev_char = ch;
            continue;
        }

        if ch.is_whitespace() {
            pending_space = true;
            prev_char = ch;
            continue;
        }

        if pending_space {
            maybe_push_space(&mut out, ch);
            pending_space = false;
        }

        out.push(ch);
        prev_char = ch;
    }

    Ok(out)
}

fn is_word_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_' || ch == '$'
}

fn maybe_push_space(out: &mut String, next_char: char) {
    let prev = out.chars().last();
    let Some(prev_char) = prev else {
        return;
    };

    let no_space_after: &[char] = &[
        '(', '[', '{', ',', ';', ':', '=', '+', '-', '*', '/', '%', '&', '|', '^', '!', '~',
        '<', '>', '?', '.',
    ];
    let no_space_before: &[char] = &[
        ')', ']', '}', ',', ';', ':', '=', '+', '-', '*', '/', '%', '&', '|', '^', '!', '~',
        '<', '>', '?', '.', '(',
    ];

    if no_space_after.contains(&prev_char) || no_space_before.contains(&next_char) {
        return;
    }

    // Avoid semantic merges like `a+++b` from `a + ++b`, or `a---b`.
    if (prev_char == '+' && next_char == '+') || (prev_char == '-' && next_char == '-') {
        out.push(' ');
        return;
    }

    if is_word_char(prev_char) && is_word_char(next_char) {
        out.push(' ');
    }
}

/// Minifies a JavaScript source string.
pub fn minify_js(input: &str) -> Result<String, String> {
    let no_comments = strip_comments(input)?;
    let minified = collapse_whitespace(&no_comments)?;
    Ok(minified)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_block_comments() {
        let input = "/* block comment */ var x = 1;";
        let output = strip_comments(input).unwrap();
        assert!(!output.contains("block comment"));
        assert!(output.contains("var x = 1;"));
    }

    #[test]
    fn test_strip_line_comments() {
        let input = "var x = 1; // line comment";
        let output = strip_comments(input).unwrap();
        assert!(!output.contains("line comment"));
        assert!(output.contains("var x = 1;"));
    }

    #[test]
    fn test_preserve_string_content() {
        let input = r#"var x = "/* not a comment */";"#;
        let output = strip_comments(input).unwrap();
        assert!(output.contains("/* not a comment */"));
    }

    #[test]
    fn test_preserve_template_literal() {
        let input = r#"var x = `// not a comment`;"#;
        let output = strip_comments(input).unwrap();
        assert!(output.contains("// not a comment"));
    }

    #[test]
    fn test_multiline_block_comment() {
        let input = "/*\n * Multi-line\n * comment\n */\nvar x = 1;";
        let output = strip_comments(input).unwrap();
        assert!(!output.contains("Multi-line"));
        assert!(output.contains("var x = 1;"));
    }

    #[test]
    fn test_collapse_whitespace() {
        let input = "function foo ( x ) { return x + 1 ; }";
        let output = collapse_whitespace(input).unwrap();
        assert_eq!(output, "function foo(x){return x+1;}");
    }

    #[test]
    fn test_preserve_spaces_in_strings() {
        let input = r#"var x = "hello world";"#;
        let output = collapse_whitespace(input).unwrap();
        assert!(output.contains("hello world"));
    }

    #[test]
    fn test_full_minify() {
        let input = r#"
            /* This is a comment */
            function test(x) {
                return x + 1;
            }
        "#;
        let output = minify_js(input).unwrap();
        assert!(!output.contains("This is a comment"));
        assert!(output.contains("function test(x){return x+1;}"));
    }

    #[test]
    fn test_strip_comments_keeps_char_after_non_comment_slash() {
        let input = r#"const a = x / y; const ok = /ab/.test("z");"#;
        let output = strip_comments(input).unwrap();
        assert!(output.contains("x / y"));
        assert!(output.contains("/ab/.test"));
    }
}
