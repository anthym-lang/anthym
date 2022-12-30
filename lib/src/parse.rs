use crate::ast::*;
use anyhow::Result;
use pest::iterators::Pair;
use pest::Parser as _;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct Parser;

fn parse_literal(literal: Pair<Rule>) -> Result<Literal> {
    fn unescape(string: &str) -> Option<String> {
        let mut result = String::new();
        let mut chars = string.chars();

        loop {
            match chars.next() {
                Some('\\') => match chars.next()? {
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    'r' => result.push('\r'),
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    '0' => result.push('\0'),
                    '\'' => result.push('\''),
                    'x' => {
                        let string: String = chars.clone().take(2).collect();

                        if string.len() != 2 {
                            return None;
                        }

                        for _ in 0..string.len() {
                            chars.next()?;
                        }

                        let value = u8::from_str_radix(&string, 16).ok()?;

                        result.push(char::from(value));
                    }
                    'u' => {
                        if chars.next()? != '{' {
                            return None;
                        }

                        let string: String = chars.clone().take_while(|c| *c != '}').collect();

                        if string.len() < 2 || 6 < string.len() {
                            return None;
                        }

                        for _ in 0..string.len() + 1 {
                            chars.next()?;
                        }

                        let value = u32::from_str_radix(&string, 16).ok()?;

                        result.push(char::from_u32(value)?);
                    }
                    _ => return None,
                },
                Some(c) => result.push(c),
                None => return Some(result),
            };
        }
    }

    let literal = literal.into_inner().next().unwrap();
    let string = literal.as_str();
    Ok(match literal.as_rule() {
        Rule::number_literal => Literal::Number(string.parse()?),
        Rule::float_literal => Literal::Float(string.parse()?),
        Rule::boolean_literal => Literal::Bool(string.parse()?),
        Rule::char_literal => {
            Literal::Char(unescape(&string[1..string.len() - 1]).unwrap().parse()?)
        }
        Rule::string_literal => Literal::Str(unescape(&string[1..string.len() - 1]).unwrap()),
        rule => unreachable!("{rule:?}"),
    })
}

fn parse_ident(ident: Pair<Rule>) -> Result<Ident> {
    Ok(Ident(ident.as_str().to_owned().into_boxed_str()))
}

fn parse_type(ty: Pair<Rule>) -> Result<Type> {
    Ok(Type(ty.as_str().to_owned().into_boxed_str()))
}

fn parse_fn(func: Pair<Rule>) -> Result<FnDecl> {
    let mut iter = func.into_inner();
    let name = parse_ident(iter.next().unwrap())?;
    let block = parse_block(iter.next_back().unwrap())?;
    let ret = parse_type(iter.next_back().unwrap())?;
    let args: Result<_> = iter
        .array_chunks()
        .map(|[arg, ty]| Ok((parse_ident(arg)?, parse_type(ty)?)))
        .collect();
    Ok(FnDecl(name, args?, ret, block))
}

fn parse_if(if_stmt: Pair<Rule>) -> Result<IfStmt> {
    let mut iter = if_stmt.into_inner();
    let else_block = parse_block(iter.next_back().unwrap())?;
    let ifs = iter
        .array_chunks()
        .map(|[cond, block]| Ok((parse_expr(cond)?, parse_block(block)?)))
        .collect::<Result<_>>()?;
    Ok(IfStmt(ifs, else_block))
}

fn parse_expr(expr: Pair<Rule>) -> Result<Expr> {
    let inner = expr.into_inner().next().unwrap();
    Ok(match inner.as_rule() {
        Rule::literal => Expr::Literal(parse_literal(inner)?),
        Rule::fn_call => {
            let mut iter = inner.into_inner();
            Expr::FnCall(
                parse_ident(iter.next().unwrap())?,
                iter.map(parse_expr).collect::<Result<_>>()?,
            )
        }
        Rule::ident => Expr::Ident(parse_ident(inner)?),
        rule => unreachable!("{rule:?}"),
    })
}

fn parse_stmt(stmt: Pair<Rule>) -> Result<Stmt> {
    Ok(match stmt.as_rule() {
        Rule::fn_decl => Stmt::Fn(parse_fn(stmt)?),
        Rule::let_var => {
            let mut iter = stmt.into_inner();
            Stmt::Var(VarDecl::Let(
                parse_ident(iter.next().unwrap())?,
                parse_expr(iter.next().unwrap())?,
            ))
        }
        Rule::mut_var => {
            let mut iter = stmt.into_inner();
            Stmt::Var(VarDecl::Mut(
                parse_ident(iter.next().unwrap())?,
                parse_expr(iter.next().unwrap())?,
            ))
        }
        Rule::reassign_var => {
            let mut iter = stmt.into_inner();
            Stmt::Var(VarDecl::ReAssign(
                parse_ident(iter.next().unwrap())?,
                parse_expr(iter.next().unwrap())?,
            ))
        }
        Rule::if_stmt => Stmt::If(parse_if(stmt)?),
        Rule::answer_stmt => Stmt::Answer(parse_expr(stmt.into_inner().next().unwrap())?),
        Rule::expr => Stmt::Expr(parse_expr(stmt)?),
        rule => unreachable!("{rule:?}"),
    })
}

fn parse_block(block: Pair<Rule>) -> Result<Block> {
    block.into_inner().map(parse_stmt).collect()
}

pub fn parse_text(raw: &str) -> Result<Program> {
    let mut stmts = Parser::parse(Rule::program, raw)?
        .next()
        .unwrap()
        .into_inner();
    stmts.next_back();
    stmts.map(parse_stmt).collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parser() {
        let program = r#"
fn a() -> Nothing {
    mut x = 1
    x = 2
    answer x
}

let r = "hello"
let s = "hello\n"
let t = 'a'
let u = '\n'
"#;
        let ast = parse_text(program);
        assert!(ast.is_ok());
        assert_eq!(
            ast.unwrap(),
            Program(vec![
                Stmt::Fn(FnDecl(
                    "a".into(),
                    Vec::new(),
                    "Nothing".into(),
                    Block(vec![
                        Stmt::Var(VarDecl::Mut("x".into(), Expr::Literal(Literal::Number(1)))),
                        Stmt::Var(VarDecl::ReAssign(
                            "x".into(),
                            Expr::Literal(Literal::Number(2))
                        )),
                        Stmt::Answer(Expr::Ident("x".into()))
                    ],)
                )),
                Stmt::Var(VarDecl::Let(
                    "r".into(),
                    Expr::Literal(Literal::Str("hello".into()))
                )),
                Stmt::Var(VarDecl::Let(
                    "s".into(),
                    Expr::Literal(Literal::Str("hello\n".into()))
                )),
                Stmt::Var(VarDecl::Let("t".into(), Expr::Literal(Literal::Char('a')))),
                Stmt::Var(VarDecl::Let("u".into(), Expr::Literal(Literal::Char('\n'))))
            ])
        );
    }
}
