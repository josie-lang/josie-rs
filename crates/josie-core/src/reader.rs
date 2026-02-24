use serde_json::{Value, json, Map};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub enum ReaderError {
    UnexpectedChar(char),
    UnterminatedString,
    UnbalancedParenthesis,
    InvalidNumber(String),
}

#[derive(Debug, Clone)]
enum Val {
    Literal(Value),
    Symbol(String),
}

pub fn read(input: &str) -> Result<Value, ReaderError> {
    let mut tokens = Tokenizer::new(input);
    let mut results = Vec::new();
    
    while let Some(token) = tokens.next_token()? {
        results.push(parse_token_wrapped(token, &mut tokens)?);
    }

    let final_vals: Vec<Value> = results.into_iter().map(|v| v_to_json_literal(v)).collect();

    if final_vals.len() == 1 {
        Ok(final_vals[0].clone())
    } else {
        let mut do_block = vec![json!("do")];
        do_block.extend(final_vals);
        Ok(Value::Array(do_block))
    }
}

pub fn read_program(input: &str) -> Result<Value, ReaderError> {
    let program_val = read(input)?;
    Ok(json!({
        "state": { "client": {}, "server": {} },
        "program": program_val
    }))
}

fn v_to_json_expr(v: Val) -> Value {
    match v {
        Val::Literal(v) => v,
        Val::Symbol(s) => {
            if s == "true" { json!(true) }
            else if s == "false" { json!(false) }
            else if s == "null" { json!(null) }
            else { json!(["var", s]) }
        }
    }
}

fn v_to_json_literal(v: Val) -> Value {
    match v {
        Val::Literal(v) => v,
        Val::Symbol(s) => {
            if s == "true" { json!(true) }
            else if s == "false" { json!(false) }
            else if s == "null" { json!(null) }
            else { json!(s) }
        }
    }
}

fn parse_token_wrapped(first: Token, tokens: &mut Tokenizer) -> Result<Val, ReaderError> {
    match first {
        Token::LParen => {
            let mut list = Vec::new();
            let mut is_first = true;
            
            while let Some(t) = tokens.peek_token()? {
                if t == Token::RParen {
                    tokens.next_token()?;
                    return Ok(Val::Literal(Value::Array(list)));
                }
                
                let next_t = tokens.next_token()?.unwrap();
                let val_wrapped = parse_token_wrapped(next_t, tokens)?;
                
                let op_name = list.first().and_then(|v| v.as_str()).unwrap_or("");
                let pos = list.len();

                let final_val = match val_wrapped {
                    Val::Symbol(s) => {
                        if is_first {
                            json!(s)
                        } else {
                            // Keyword check
                            if s == "true" { json!(true) }
                            else if s == "false" { json!(false) }
                            else if s == "null" { json!(null) }
                            else {
                                // Context aware wrapping
                                let is_literal_pos = (op_name == "set" || op_name == "def" || op_name == "call") && pos == 1;
                                if is_literal_pos {
                                    json!(s)
                                } else {
                                    json!(["var", s])
                                }
                            }
                        }
                    }
                    Val::Literal(v) => v,
                };

                list.push(final_val);
                is_first = false;
            }
            Err(ReaderError::UnbalancedParenthesis)
        }
        Token::RParen => Err(ReaderError::UnbalancedParenthesis),
        Token::LBracket => {
            let mut list = Vec::new();
            while let Some(t) = tokens.peek_token()? {
                if t == Token::RBracket {
                    tokens.next_token()?;
                    return Ok(Val::Literal(Value::Array(list)));
                }
                let next_t = tokens.next_token()?.unwrap();
                list.push(v_to_json_expr(parse_token_wrapped(next_t, tokens)?));
            }
            Err(ReaderError::UnbalancedParenthesis)
        }
        Token::RBracket => Err(ReaderError::UnbalancedParenthesis),
        Token::LBrace => {
            let mut map = Map::new();
            while let Some(t) = tokens.next_token()? {
                match t {
                    Token::RBrace => return Ok(Val::Literal(Value::Object(map))),
                    Token::Symbol(key) | Token::String(key) => {
                        if let Some(Token::Symbol(s)) = tokens.peek_token()? {
                            if s == ":" { tokens.next_token()?; }
                        }
                        let val_token = tokens.next_token()?.ok_or(ReaderError::UnexpectedChar('}'))?;
                        let val = v_to_json_expr(parse_token_wrapped(val_token, tokens)?);
                        map.insert(key, val);
                    }
                    _ => return Err(ReaderError::UnexpectedChar('?')),
                }
            }
            Err(ReaderError::UnexpectedChar('}'))
        }
        Token::RBrace => Err(ReaderError::UnexpectedChar('}')),
        Token::String(s) => Ok(Val::Literal(json!(s))),
        Token::Number(n) => Ok(Val::Literal(json!(n))),
        Token::Symbol(s) => Ok(Val::Symbol(s)),
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Token {
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    String(String),
    Number(f64),
    Symbol(String),
}

struct Tokenizer<'a> {
    chars: Peekable<Chars<'a>>,
    peeked: Option<Token>,
}

impl<'a> Tokenizer<'a> {
    fn new(input: &'a str) -> Self {
        Self { chars: input.chars().peekable(), peeked: None }
    }

    fn peek_token(&mut self) -> Result<Option<Token>, ReaderError> {
        if self.peeked.is_none() {
            self.peeked = self.next_token_inner()?;
        }
        Ok(self.peeked.clone())
    }

    fn next_token(&mut self) -> Result<Option<Token>, ReaderError> {
        if let Some(t) = self.peeked.take() {
            return Ok(Some(t));
        }
        self.next_token_inner()
    }

    fn next_token_inner(&mut self) -> Result<Option<Token>, ReaderError> {
        loop {
            while let Some(&c) = self.chars.peek() {
                if c.is_whitespace() || c == ',' || c == ':' {
                    self.chars.next();
                    continue;
                }
                break;
            }

            if let Some(&'#') = self.chars.peek() {
                // Skip comment line
                while let Some(c) = self.chars.next() {
                    if c == '\n' { break; }
                }
                continue;
            }
            break;
        }

        let c = match self.chars.next() {
            Some(c) => c,
            None => return Ok(None),
        };

        match c {
            '(' => Ok(Some(Token::LParen)),
            ')' => Ok(Some(Token::RParen)),
            '{' => Ok(Some(Token::LBrace)),
            '}' => Ok(Some(Token::RBrace)),
            '[' => Ok(Some(Token::LBracket)),
            ']' => Ok(Some(Token::RBracket)),
            '\'' | '"' => {
                let quote = c;
                let mut s = String::new();
                let mut escaped = false;
                while let Some(nc) = self.chars.next() {
                    if escaped {
                        s.push(nc);
                        escaped = false;
                    } else if nc == '\\' {
                        escaped = true;
                    } else if nc == quote {
                        return Ok(Some(Token::String(s)));
                    } else {
                        s.push(nc);
                    }
                }
                Err(ReaderError::UnterminatedString)
            }
            _ if c.is_digit(10) || (c == '-' && self.chars.peek().is_some_and(|&nc| nc.is_digit(10))) => {
                let mut s = c.to_string();
                while let Some(&nc) = self.chars.peek() {
                    if nc.is_digit(10) || nc == '.' {
                        s.push(self.chars.next().unwrap());
                    } else { break; }
                }
                let n = s.parse::<f64>().map_err(|_| ReaderError::InvalidNumber(s))?;
                Ok(Some(Token::Number(n)))
            }
            _ => {
                let mut s = c.to_string();
                while let Some(&nc) = self.chars.peek() {
                    if nc.is_whitespace() || nc == '(' || nc == ')' || nc == '{' || nc == '}' || nc == '[' || nc == ']' || nc == ',' || nc == ':' {
                        break;
                    }
                    s.push(self.chars.next().unwrap());
                }
                Ok(Some(Token::Symbol(s)))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_read() {
        let input = "(+ 1 2)";
        let val = read(input).unwrap();
        assert_eq!(val, json!(["+", 1.0, 2.0]));
    }

    #[test]
    fn test_symbol_var_wrapping() {
        let input = "(set client.count (+ client.count 1))";
        let val = read(input).unwrap();
        assert_eq!(val, json!(["set", "client.count", ["+", ["var", "client.count"], 1.0]]));
    }

    #[test]
    fn test_nested_objects() {
        let input = "(call api.post \"/url\" { status: \"ok\", val: (+ x 1) })";
        let val = read(input).unwrap();
        assert_eq!(val, json!([
            "call", "api.post", "/url",
            {
                "status": "ok",
                "val": ["+", ["var", "x"], 1.0]
            }
        ]));
    }
}
