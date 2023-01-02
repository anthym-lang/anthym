use crate::ast::Ident;
use crate::env::SharedEnv;
use crate::function::Function;
use crate::value::Value;
use anyhow::{bail, Result};
use std::borrow::Cow;
use std::rc::Rc;

pub fn builtin_to_function(builtin: fn(Vec<Value>, SharedEnv) -> Result<Value>) -> Function {
    Rc::new(Box::new(builtin))
}

macro_rules! builtins {
    [$($lang_name: ident : $value: expr),+] => {
        HashMap::from([$((stringify!($lang_name).into(), (false, Rc::new(RefCell::new($value.into()))))),+])
    };
}

pub(crate) use builtins;

pub fn println(args: Vec<Value>, _env: SharedEnv) -> Result<Value> {
    if args.len() != 1 {
        bail!("wrong arg count")
    }
    let arg = args.get(0).unwrap();

    #[cfg(not(target_arch = "wasm32"))]
    fn log<S: ToString>(message: S) {
        println!("{}", message.to_string());
    }

    #[cfg(target_arch = "wasm32")]
    #[wasm_bindgen]
    extern "C" {
        #[wasm_bindgen(js_namespace = console)]
        fn log(s: &str);
    }

    log(arg.to_string());

    Ok(Value::Nothing)
}

pub fn throw(args: Vec<Value>, _env: SharedEnv) -> Result<Value> {
    if args.len() != 1 {
        bail!("wrong arg count")
    }

    let arg = args.get(0).unwrap();
    bail!("{arg}")
}

pub const U8: Ident = Ident(Cow::Borrowed("u8"));
