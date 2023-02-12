use crate::env::SharedEnv;
use crate::function::Function;
use crate::value::Value;
use anyhow::{bail, Result};
use std::rc::Rc;

pub(crate) fn builtin_to_function(builtin: fn(Vec<Value>, SharedEnv) -> Result<Value>) -> Function {
    Rc::new(Box::new(builtin))
}

macro_rules! builtins {
    [$($lang_name: ident : $value: expr),+] => {
        HashMap::from([$((stringify!($lang_name).into(), (false, Rc::new(RefCell::new($value.into()))))),+])
    };
}

pub(crate) use builtins;

pub(crate) fn println(args: Vec<Value>, _env: SharedEnv) -> Result<Value> {
    if args.len() != 1 {
        bail!("wrong arg count")
    }

    // SAFETY: we just checked that the argument length == 1
    let arg = unsafe { args.get(0).unwrap_unchecked() };

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

    log(arg.to_string().as_str());

    Ok(Value::Nothing)
}

pub(crate) fn throw(args: Vec<Value>, _env: SharedEnv) -> Result<Value> {
    if args.len() != 1 {
        bail!("wrong arg count")
    }

    // SAFETY: we just checked that the argument length == 1
    let arg = unsafe { args.get(0).unwrap_unchecked() };
    bail!("{arg}")
}
