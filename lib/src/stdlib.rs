use crate::env::SharedEnv;
use crate::run::Value;
use anyhow::{bail, Result};

pub(crate) fn println(args: Vec<Value>, _env: SharedEnv) -> Result<Value> {
    if args.len() != 1 {
        bail!("wrong arg count")
    }
    let arg = args.get(0).expect("what");

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
