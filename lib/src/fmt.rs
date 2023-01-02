use crate::ast::*;
use crate::parse::parse_text;
use anyhow::Result;
use std::fmt::Display;

const INDENT_LEVEL: &str = "    ";

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Literal::Str(s) => format!(r#""{}""#, s.escape_default()),
                Literal::Char(ch) => format!("'{}'", ch.escape_default()),
                Literal::Bool(bool) => format!("{bool}"),
                Literal::Int(num) => format!("{num}"),
                Literal::Float(float) => format!("{float}"),
            }
        )
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for FnDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r"
fn {name}({args}) -> {ret} {block}
",
            name = self.name,
            args = self
                .args
                .iter()
                .map(|(arg, ty)| format!("{arg}: {ty}",))
                .collect::<Vec<_>>()
                .join(", "),
            ret = self.ret,
            block = self.block
        )
    }
}

impl Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            if self.ifs.len() > 1 {
                let mut iter = self.ifs.iter();
                let first = iter.next().unwrap();
                let else_ifs = iter
                    .map(|(cond, block)| format!("else if {cond} {block}",))
                    .collect::<Vec<_>>()
                    .join(" ");
                format!(
                    "
if {cond} {block} {else_ifs}
",
                    cond = first.0,
                    block = first.1,
                )
            } else {
                let if_stmt = self.ifs.first().unwrap();
                format!(
                    "
if {cond} {block}
",
                    cond = if_stmt.0,
                    block = if_stmt.1,
                )
            }
        )
    }
}

impl Display for FnCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{name}({args})",
            name = self.name,
            args = self
                .args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expr::Literal(lit) => lit.to_string(),
                Expr::Ident(id) => id.to_string(),
                Expr::FnCall(call) => call.to_string(),
            }
        )
    }
}

impl Display for Answer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Stmt::Var(var) => match var {
                    Var::Let(name, value) => format!("let {name} = {value}",),
                    Var::Mut(name, value) => format!("mut {name} = {value}",),
                    Var::ReAssign(name, value) => format!("{name} = {value}",),
                },
                Stmt::Fn(decl) => decl.to_string(),
                Stmt::If(if_stmt) => if_stmt.to_string(),
                Stmt::Answer(answer) => answer.to_string(),
                Stmt::Expr(expr) => expr.to_string(),
            }
        )
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{
{content}
}}",
            content = self
                .0
                .iter()
                .map(|stmt| stmt
                    .to_string()
                    .lines()
                    .map(|line| format!("{INDENT_LEVEL}{line}"))
                    .collect::<Vec<_>>()
                    .join("\n"))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", {
            self.0
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n")
                .trim()
                .to_owned()
        })
    }
}

pub fn format_text(raw: &str) -> Result<String> {
    Ok(parse_text(raw)?.to_string())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_formatter_looks() {
        let program_ugly = "fn a(  

)-> nothing{
    mut x=   1
x   =2



}
    ";
        let program = "
fn a() -> nothing {
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

)-> nothing{
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
