use std::fmt::Display;

pub(crate) trait IceExt<T> {
    fn unwrap_or_ice(self) -> T;

    fn unwrap_or_ice_msg(self, msg: &str) -> T;
}

impl<T> IceExt<T> for Option<T> {
    fn unwrap_or_ice(self) -> T {
        match self {
            Some(inner) => inner,
            None => panic!(
                "The compiler/interpreter panicked.
Please file an issue to the github repository."
            ),
        }
    }

    fn unwrap_or_ice_msg(self, msg: &str) -> T {
        match self {
            Some(inner) => inner,
            None => panic!(
                "The compiler/interpreter panicked.
Please file an issue to the github repository.
The following information was provided: {msg}"
            ),
        }
    }
}

impl<T, E> IceExt<T> for Result<T, E>
where
    E: Display,
{
    fn unwrap_or_ice(self) -> T {
        match self {
            Ok(inner) => inner,
            Err(err) => panic!(
                "The compiler/interpreter panicked.
Please file an issue to the github repository.
The error that occured was: {err}"
            ),
        }
    }

    fn unwrap_or_ice_msg(self, msg: &str) -> T {
        match self {
            Ok(inner) => inner,
            Err(err) => panic!(
                "The compiler/interpreter panicked.
Please file an issue to the github repository.
The error that occured was: {err}
The following information was provided: {msg}"
            ),
        }
    }
}
