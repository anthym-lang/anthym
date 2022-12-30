use crate::ast::*;
use crate::parse::parse_text;
use anyhow::Result;

const INDENT_LEVEL: &str = "    ";

fn format_literal(literal: Literal) -> String {
    match literal {
        Literal::Str(s) => format!(r#""{}""#, s.escape_default()),
        Literal::Char(ch) => format!("'{}'", ch.escape_default()),
        Literal::Bool(bool) => format!("{bool}"),
        Literal::Number(num) => format!("{num}"),
        Literal::Float(float) => format!("{float}"),
    }
}

fn format_ident(ident: Ident) -> String {
    format!("{ident}", ident = ident.0)
}

fn format_type(ty: Type) -> String {
    format!("{ty}", ty = ty.0)
}

fn format_fn(func: FnDecl) -> String {
    format!(
        r"
fn {name}({args}) -> {ret} {block}
",
        name = format_ident(func.0),
        args = func
            .1
            .into_iter()
            .map(|(arg, ty)| format!("{arg}: {ty}", arg = format_ident(arg), ty = format_type(ty)))
            .collect::<Vec<_>>()
            .join(", "),
        ret = format_type(func.2),
        block = format_block(func.3)
    )
}

fn format_if(if_stmt: IfStmt) -> String {
    if if_stmt.0.len() > 1 {
        let mut iter = if_stmt.0.into_iter();
        let first = iter.next().unwrap();
        let else_ifs = iter
            .map(|(cond, block)| {
                format!(
                    "else if {cond} {block}",
                    cond = format_expr(cond),
                    block = format_block(block)
                )
            })
            .collect::<Vec<_>>()
            .join(" ");
        format!(
            "
if {cond} {block} {else_ifs}
",
            cond = format_expr(first.0),
            block = format_block(first.1),
        )
    } else {
        let if_stmt = if_stmt.0.into_iter().next().unwrap();
        format!(
            "
if {cond} {block}
",
            cond = format_expr(if_stmt.0),
            block = format_block(if_stmt.1)
        )
    }
}

fn format_expr(expr: Expr) -> String {
    match expr {
        Expr::Literal(lit) => format_literal(lit),
        Expr::Ident(id) => format_ident(id),
        Expr::FnCall(name, args) => format!(
            "{name}({args})",
            name = format_ident(name),
            args = args
                .into_iter()
                .map(format_expr)
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

fn format_stmt(stmt: Stmt) -> String {
    match stmt {
        Stmt::Var(var) => match var {
            VarDecl::Let(name, value) => format!(
                "let {name} = {value}",
                name = format_ident(name),
                value = format_expr(value)
            ),
            VarDecl::Mut(name, value) => format!(
                "mut {name} = {value}",
                name = format_ident(name),
                value = format_expr(value)
            ),
            VarDecl::ReAssign(name, value) => format!(
                "{name} = {value}",
                name = format_ident(name),
                value = format_expr(value)
            ),
        },
        Stmt::Fn(func) => format_fn(func),
        Stmt::If(if_stmt) => format_if(if_stmt),
        Stmt::Answer(answer) => format!("answer {answer}", answer = format_expr(answer)),
        Stmt::Expr(expr) => format_expr(expr),
    }
}

fn format_block(block: Block) -> String {
    format!(
        "{{
{content}
}}",
        content = block
            .0
            .into_iter()
            .map(|stmt| format_stmt(stmt)
                .lines()
                .map(|line| format!("{INDENT_LEVEL}{line}"))
                .collect::<Vec<_>>()
                .join("\n"))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

pub fn format_text(raw: &str) -> Result<String> {
    let stmts = parse_text(raw)?.0;
    Ok(stmts
        .into_iter()
        .map(format_stmt)
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_owned())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_formatter_looks() {
        let program_ugly = "fn a(  

)-> Nothing{
    mut x=   1
x   =2



}
    ";
        let program = "
fn a() -> Nothing {
    mut x = 1
    x = 2
}";
        let formatted = format_text(program_ugly);
        assert!(formatted.is_ok());
        assert_eq!(formatted.unwrap(), program.trim().to_owned());
    }

    #[test]
    fn test_formatter_validity() {
        let program_ugly = "fn a(  

)-> Nothing{
    mut x=   1
x   =2



}
    ";
        let formatted = format_text(program_ugly);
        assert!(formatted.is_ok());
        assert_eq!(
            parse_text(program_ugly).unwrap(),
            parse_text(&formatted.unwrap()).unwrap()
        );
    }
}
