use std::{char, usize};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Location {
    colmun: i32,
    line: i32,
    index: usize,
}

impl Location {
    /// ファイルの行番号と列番号、および現在のインデックスを更新する
    fn increment(&self, is_newline: bool) -> Location {
        if is_newline {
            Location {
                index: self.index + 1,
                colmun: 0,
                line: self.line + 1,
            }
        } else {
            Location {
                index: self.index + 1,
                colmun: self.colmun + 1,
                line: self.line,
            }
        }
    }

    /// 現在の行をダンプし、現在の列へのテキスト・ポインタとメッセージを表示する
    pub fn debug<S: Into<String>>(&self, raw: &[char], msg: S) -> String {
        let mut line = 0;
        let mut line_str = String::new();
        for c in raw {
            if *c == '\n' {
                line += 1;

                if !line_str.is_empty() {
                    break;
                }

                continue;
            }

            if self.line == line {
                line_str.push_str(&c.to_string());
            }
        }

        let space = " ".repeat(self.colmun as usize);
        format!("{}\n\n{}\n{}^ Near here", msg.into(), line_str, space)
    }
}

const KEYWORDS: [&str; 6] = ["function", "end", "if", "then", "local", "return"];
const SYMBOL: [&str; 5] = [";", "=", "(", ")", ","];
const OPERATORS: [&str; 3] = ["+", "-", "<"];

/// 字句解析後の最小単位はトークン
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Identifier,
    Symbol,
    Keyword,
    Number,
    Operator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub value: String,
    pub kind: TokenKind,
    pub location: Location,
}

pub fn lex(raw: &[char]) -> Result<Vec<Token>, String> {
    let mut location = Location {
        colmun: 0,
        index: 0,
        line: 0,
    };
    let size = raw.len();
    let mut tokens = vec![];

    'outer: while location.index < size {
        location = consume_whitespace(raw, location);
        if location.index == size {
            break;
        }

        for lexer in [
            lex_keyword,
            lex_identifier,
            lex_symbol,
            lex_number,
            lex_operator,
        ] {
            let ret = lexer(raw, location);
            if let Some((t, next_location)) = ret {
                location = next_location;
                tokens.push(t);
                continue 'outer;
            }
        }

        return Err(location.debug(raw, "Unrecognized character while lexing"));
    }

    Ok(tokens)
}

/// スペース or 改行があった時に新しい Location を返す
fn consume_whitespace(raw: &[char], location: Location) -> Location {
    let mut c = raw[location.index];
    let mut next_loc = location;
    while [' ', '\n', '\r', '\t'].contains(&c) {
        next_loc = next_loc.increment(c == '\n');
        if next_loc.index == raw.len() {
            break;
        }
        c = raw[next_loc.index];
    }
    next_loc
}

/// アルファベット文字、数字、アンダースコアの集合
/// 数字で始めることはできない
fn lex_identifier(raw: &[char], location: Location) -> Option<(Token, Location)> {
    let mut value = String::new();
    let mut next_loc = location;
    let mut c = raw[location.index];
    while c.is_alphanumeric() || c == '_' {
        value.push_str(&c.to_string());
        next_loc = next_loc.increment(false);
        c = raw[next_loc.index]
    }

    match value.chars().nth(0) {
        Some(head) => {
            if head.is_digit(10) {
                return None;
            }
        }
        None => {
            return None;
        }
    };

    Some((
        Token {
            value,
            kind: TokenKind::Identifier,
            location,
        },
        next_loc,
    ))
}

fn lex_symbol(raw: &[char], location: Location) -> Option<(Token, Location)> {
    for symbol in SYMBOL {
        let c = raw[location.index];
        let next_loc = location.increment(false);
        if symbol == c.to_string() {
            return Some((
                Token {
                    value: symbol.to_string(),
                    location,
                    kind: TokenKind::Symbol,
                },
                next_loc,
            ));
        }
    }
    None
}

/// 予約語
fn lex_keyword(raw: &[char], location: Location) -> Option<(Token, Location)> {
    let mut value = String::new();
    let mut c = raw[location.index];
    let mut next_loc = location;
    while c.is_alphanumeric() {
        value.push(c);
        next_loc = next_loc.increment(false);
        c = raw[next_loc.index];
    }

    if KEYWORDS.map(|v| v.to_string()).contains(&value) {
        Some((
            Token {
                value,
                location,
                kind: TokenKind::Keyword,
            },
            next_loc,
        ))
    } else {
        None
    }
}

/// 小数点以下の桁が表示されなくなるまでソースを繰り返し処理
/// 整数のみをサポート
fn lex_number(raw: &[char], location: Location) -> Option<(Token, Location)> {
    let mut value = String::new();
    let mut c = raw[location.index];
    let mut next_loc = location;
    while c.is_digit(10) {
        value.push_str(&c.to_string());
        next_loc = next_loc.increment(false);
        c = raw[next_loc.index]
    }

    if value.is_empty() {
        return None;
    }
    Some((
        Token {
            value,
            kind: TokenKind::Number,
            location,
        },
        next_loc,
    ))
}

fn lex_operator(raw: &[char], location: Location) -> Option<(Token, Location)> {
    for operator in OPERATORS {
        let c = raw[location.index];
        let next_loc = location.increment(false);
        if operator == c.to_string() {
            return Some((
                Token {
                    value: operator.to_string(),
                    kind: TokenKind::Operator,
                    location,
                },
                next_loc,
            ));
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::lex::{
        self, lex_identifier, lex_keyword, lex_number, lex_operator, lex_symbol, Token, TokenKind,
    };

    use super::{consume_whitespace, Location};

    #[test]
    fn test_increment() {
        let location = Location {
            index: 0,
            colmun: 0,
            line: 0,
        };
        assert_eq!(
            Location {
                index: 1,
                colmun: 0,
                line: 1,
            },
            location.increment(true)
        );

        let location = Location {
            index: 0,
            colmun: 0,
            line: 0,
        };
        assert_eq!(
            Location {
                index: 1,
                colmun: 1,
                line: 0,
            },
            location.increment(false)
        );
    }

    #[test]
    fn test_consume_whitespace() {
        let raw = vec![' ', 'a'];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert_eq!(
            Location {
                index: 1,
                colmun: 1,
                line: 0,
            },
            consume_whitespace(&raw, location)
        );

        let raw = vec!['\n', '\n'];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert_eq!(
            Location {
                index: 2,
                colmun: 0,
                line: 2,
            },
            consume_whitespace(&raw, location)
        );
    }

    #[test]
    fn test_lex_number() {
        let raw = vec!['0', '\n'];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert_eq!(
            (
                Token {
                    value: "0".to_string(),
                    kind: TokenKind::Number,
                    location: Location {
                        colmun: 0,
                        line: 0,
                        index: 0
                    }
                },
                Location {
                    index: 1,
                    colmun: 1,
                    line: 0,
                }
            ),
            lex_number(&raw, location).unwrap()
        );

        let raw = vec!['1', '0', '.', '0', '1', '\n'];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert_eq!(
            (
                Token {
                    value: "10".to_string(),
                    kind: TokenKind::Number,
                    location: Location {
                        colmun: 0,
                        line: 0,
                        index: 0
                    }
                },
                Location {
                    index: 2,
                    colmun: 2,
                    line: 0,
                }
            ),
            lex_number(&raw, location).unwrap()
        );

        let raw = vec!['\n'];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert!(lex_number(&raw, location).is_none());
    }

    #[test]
    fn test_lex_identifier() {
        let raw = vec!['_', '\n'];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert_eq!(
            (
                Token {
                    value: "_".to_string(),
                    kind: TokenKind::Identifier,
                    location: Location {
                        colmun: 0,
                        line: 0,
                        index: 0
                    }
                },
                Location {
                    index: 1,
                    colmun: 1,
                    line: 0,
                }
            ),
            lex_identifier(&raw, location).unwrap()
        );

        let raw = vec!['f', 'o', 'o', '\n'];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert_eq!(
            (
                Token {
                    value: "foo".to_string(),
                    kind: TokenKind::Identifier,
                    location: Location {
                        colmun: 0,
                        line: 0,
                        index: 0
                    }
                },
                Location {
                    index: 3,
                    colmun: 3,
                    line: 0,
                }
            ),
            lex_identifier(&raw, location).unwrap()
        );

        let raw = vec!['0', '_', '\n'];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert_eq!(Some(None), Some(lex_identifier(&raw, location)));

        let raw = vec![' ', '\n'];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert!(lex_identifier(&raw, location).is_none());
    }

    #[test]
    fn test_lex_keyword() {
        let raw = vec!['f', 'u', 'n', 'c', 't', 'i', 'o', 'n', ' ', '1', ' '];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert_eq!(
            (
                Token {
                    value: "function".to_string(),
                    kind: TokenKind::Keyword,
                    location: Location {
                        colmun: 0,
                        line: 0,
                        index: 0
                    }
                },
                Location {
                    index: 8,
                    colmun: 8,
                    line: 0,
                }
            ),
            lex_keyword(&raw, location).unwrap()
        );

        let raw = vec!['f', 'u', 'n', 'c', 't', 'i', 'o', 'n', '1', ' '];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert!(lex_keyword(&raw, location).is_none());
    }

    #[test]
    fn test_lex_symbol() {
        let raw = vec!['(', ' '];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert_eq!(
            (
                Token {
                    value: "(".to_string(),
                    kind: TokenKind::Symbol,
                    location: Location {
                        colmun: 0,
                        line: 0,
                        index: 0
                    }
                },
                Location {
                    index: 1,
                    colmun: 1,
                    line: 0,
                }
            ),
            lex_symbol(&raw, location).unwrap()
        );

        let raw = vec![' ', ')', ' '];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert!(lex_symbol(&raw, location).is_none());
    }

    #[test]
    fn test_lex_operator() {
        let raw = vec!['+', ' '];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert_eq!(
            (
                Token {
                    value: "+".to_string(),
                    kind: TokenKind::Operator,
                    location: Location {
                        colmun: 0,
                        line: 0,
                        index: 0
                    }
                },
                Location {
                    index: 1,
                    colmun: 1,
                    line: 0,
                }
            ),
            lex_operator(&raw, location).unwrap()
        );

        let raw = vec!['+', '1'];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert_eq!(
            (
                Token {
                    value: "+".to_string(),
                    kind: TokenKind::Operator,
                    location: Location {
                        colmun: 0,
                        line: 0,
                        index: 0
                    }
                },
                Location {
                    colmun: 1,
                    line: 0,
                    index: 1
                },
            ),
            lex_operator(&raw, location).unwrap()
        );

        let raw = vec!['1', '+', '1'];
        let location = Location {
            colmun: 0,
            index: 0,
            line: 0,
        };
        assert!(lex_operator(&raw, location).is_none());
    }

    #[test]
    fn test_lex() {
        let raw = "function func();\nend;".chars().collect::<Vec<char>>();
        let tokens = lex::lex(&raw).unwrap();
        assert_eq!(tokens.len(), 7);
        assert_eq!(
            tokens[0],
            Token {
                value: "function".to_string(),
                kind: TokenKind::Keyword,
                location: Location {
                    colmun: 0,
                    line: 0,
                    index: 0
                }
            },
        );
        assert_eq!(
            tokens[1],
            Token {
                value: "func".to_string(),
                kind: TokenKind::Identifier,
                location: Location {
                    colmun: 9,
                    line: 0,
                    index: 9
                }
            },
        );
        assert_eq!(
            tokens[2],
            Token {
                value: "(".to_string(),
                kind: TokenKind::Symbol,
                location: Location {
                    colmun: 13,
                    line: 0,
                    index: 13
                }
            }
        );
        assert_eq!(
            tokens[3],
            Token {
                value: ")".to_string(),
                kind: TokenKind::Symbol,
                location: Location {
                    colmun: 14,
                    line: 0,
                    index: 14
                }
            }
        );
        assert_eq!(
            tokens[4],
            Token {
                value: ";".to_string(),
                kind: TokenKind::Symbol,
                location: Location {
                    colmun: 15,
                    line: 0,
                    index: 15
                }
            }
        );
        assert_eq!(
            tokens[5],
            Token {
                value: "end".to_string(),
                kind: TokenKind::Keyword,
                location: Location {
                    colmun: 0,
                    line: 1,
                    index: 17
                }
            }
        );

        let raw = "~".chars().collect::<Vec<char>>();
        assert!(lex::lex(&raw).is_err());
    }
}
