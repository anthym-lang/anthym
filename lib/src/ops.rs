use crate::ast::Op;
use anyhow::{bail, Error};

#[derive(Debug)]
pub(crate) enum BinaryOp {
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    Add,
    Sub,
    Mul,
    Div,
}

impl TryFrom<Op> for BinaryOp {
    type Error = Error;

    fn try_from(value: Op) -> Result<Self, Self::Error> {
        match value {
            Op::Eq => Ok(Self::Eq),
            Op::Ne => Ok(Self::Ne),
            Op::Lt => Ok(Self::Lt),
            Op::Lte => Ok(Self::Lte),
            Op::Gt => Ok(Self::Gt),
            Op::Gte => Ok(Self::Gte),
            Op::Add => Ok(Self::Add),
            Op::Sub => Ok(Self::Sub),
            Op::Mul => Ok(Self::Mul),
            Op::Div => Ok(Self::Div),
            other => bail!("invalid cmp op: {other}"),
        }
    }
}

#[derive(Debug)]
pub(crate) enum CmpOp {
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
}

impl TryFrom<BinaryOp> for CmpOp {
    type Error = Error;

    fn try_from(value: BinaryOp) -> Result<Self, Self::Error> {
        match value {
            BinaryOp::Eq => Ok(Self::Eq),
            BinaryOp::Ne => Ok(Self::Ne),
            BinaryOp::Lt => Ok(Self::Lt),
            BinaryOp::Lte => Ok(Self::Lte),
            BinaryOp::Gt => Ok(Self::Gt),
            BinaryOp::Gte => Ok(Self::Gte),
            other => bail!("invalid cmp op: {other:?}"),
        }
    }
}

#[derive(Debug)]
pub(crate) enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl TryFrom<BinaryOp> for ArithmeticOp {
    type Error = Error;

    fn try_from(value: BinaryOp) -> Result<Self, Self::Error> {
        match value {
            BinaryOp::Add => Ok(Self::Add),
            BinaryOp::Sub => Ok(Self::Sub),
            BinaryOp::Mul => Ok(Self::Mul),
            BinaryOp::Div => Ok(Self::Div),
            other => bail!("invalid arithmetic op: {other:?}"),
        }
    }
}

#[derive(Debug)]
pub(crate) enum UnaryOp {
    Sub,
    Not,
}

impl TryFrom<Op> for UnaryOp {
    type Error = Error;

    fn try_from(value: Op) -> Result<Self, Self::Error> {
        match value {
            Op::Sub => Ok(Self::Sub),
            Op::Not => Ok(Self::Not),
            op => bail!("invalid unary op: {op:?}"),
        }
    }
}
