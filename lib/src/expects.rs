use crate::token::TokenType;

const unsafe fn concat<A: Copy, B: Copy, C: Copy>(a: A, b: B) -> C {
    #[repr(C)]
    struct Both<A, B>(A, B);

    union Transmute<A, B, C> {
        from: std::mem::ManuallyDrop<Both<A, B>>,
        to: std::mem::ManuallyDrop<C>,
    }

    std::mem::ManuallyDrop::into_inner(
        Transmute {
            from: std::mem::ManuallyDrop::new(Both(a, b)),
        }
        .to,
    )
}

macro_rules! concat_array {
    () => {
        []
    };
    ($a:expr) => {
        $a
    };
    ($a:expr, $b:expr) => {{
            let a = $a;
            let b = $b;
            #[allow(clippy::undocumented_unsafe_blocks)]
            let c: [_; $a.len() + $b.len()] = unsafe { concat(a, b) };
            let _: [*const _; 3] = [a.as_ptr(), b.as_ptr(), c.as_ptr()];
            c
    }};
    ($a:expr, $($rest:expr),*) => {
        concat_array!($a, concat_array!($($rest),*))
    };
    ($a:expr, $($rest:expr),*,) => {
        concat_array!($a, $($rest),*)
    };
}

pub(crate) const IDENT: [TokenType; 1] = [TokenType::Ident];
pub(crate) const LITERAL: [TokenType; 5] = [
    TokenType::IntLiteral,
    TokenType::BoolLiteral,
    TokenType::CharLiteral,
    TokenType::FloatLiteral,
    TokenType::StringLiteral,
];
// pub(crate) const FN_CALL: [TokenType; 1] = IDENT;
pub(crate) const EXPR: [TokenType; 6] = concat_array!(IDENT, LITERAL);
pub(crate) const FN_DECL: [TokenType; 1] = [TokenType::KeywordFn];
pub(crate) const VAR: [TokenType; 3] = [
    TokenType::KeywordLet,
    TokenType::KeywordMut,
    TokenType::Ident,
];
pub(crate) const IF_STMT: [TokenType; 1] = [TokenType::KeywordIf];
pub(crate) const ANSWER: [TokenType; 1] = [TokenType::KeywordAnswer];
pub(crate) const STMT: [TokenType; 12] = concat_array!(IF_STMT, FN_DECL, VAR, ANSWER, EXPR);
// pub(crate) const BLOCK: [TokenType; 1] = [TokenType::LBrace];
