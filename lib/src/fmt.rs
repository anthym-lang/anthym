use crate::ast::*;
use crate::parse::parse_text;
use anyhow::Result;
use std::fmt::Display;

const INDENT_LEVEL: &str = "    ";

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let generics = self
            .generics
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        write!(f, "{}", self.base,)?;
        if !generics.is_empty() {
            write!(f, "{}", generics.join(", "))?;
        }

        Ok(())
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Str(s) => write!(f, r#""{}""#, s.escape_default()),
            Literal::Char(ch) => write!(f, "'{}'", ch.escape_default()),
            Literal::Bool(bool) => write!(f, "{bool}"),
            Literal::Int(num) => write!(f, "{num}"),
            Literal::Float(float) => write!(f, "{float}"),
        }
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
            "
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
        let if_stmt = &self.if_stmt;
        let else_ifs = self
            .else_ifs
            .iter()
            .map(|(cond, block)| format!("else if {cond} {block}",))
            .collect::<Vec<_>>()
            .join(" ");
        write!(
            f,
            "
if {if_cond} {if_block} {else_ifs} else {else_block}
",
            if_cond = if_stmt.0,
            if_block = if_stmt.1,
            else_block = self.else_block
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

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Eq => write!(f, "=="),
            Op::Ne => write!(f, "!="),
            Op::Lt => write!(f, "<"),
            Op::Lte => write!(f, "<="),
            Op::Gt => write!(f, ">"),
            Op::Gte => write!(f, ">="),
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Div => write!(f, "/"),
            Op::Mul => write!(f, "*"),
            Op::Not => write!(f, "!"),
        }
    }
}

// FIXME: strange stuff probably happens with formatting expressions in parenthesis
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expr::Literal(lit) => lit.to_string(),
                Expr::Ident(id) => id.to_string(),
                Expr::FnCall(call) => call.to_string(),
                Expr::Binary(left, op, right) => format!("{left} {op} {right}"),
                Expr::Unary(op, expr) => format!("{op}{expr}"),
            }
        )
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {}", self.expr)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Stmt::Var(var) => match var {
                    Var::Let(name, ty, value) => format!("let {name}: {ty} = {value}",),
                    Var::Mut(name, ty, value) => format!("mut {name}: {ty} = {value}",),
                    Var::ReAssign(name, value) => format!("{name} = {value}",),
                },
                Stmt::If(if_stmt) => if_stmt.to_string(),
                Stmt::Return(return_) => return_.to_string(),
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
    mut x:int=   1
x   =2



}
    ";
        let program = "
fn a() -> nothing {
    mut x: int = 1
    x = 2
}";
        let formatted = format_text(program_ugly);
        assert_eq!(
            formatted.map_err(|err| err.to_string()),
            Ok(program.trim().to_owned())
        );
    }

    #[test]
    fn test_formatter_validity() {
        let program_ugly = "fn a(  

)-> nothing{
    mut x:int=   1
x   =2



}
    ";
        let formatted = format_text(program_ugly);
        assert_eq!(
            parse_text(program_ugly).map_err(|err| err.to_string()),
            formatted
                .and_then(parse_text)
                .map_err(|err| err.to_string())
        );
    }
}
