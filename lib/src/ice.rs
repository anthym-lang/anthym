use std::fmt::Display;

#[cold]
pub(crate) fn ice(msg: &str) -> ! {
    panic!(
        "The compiler/interpreter panicked.
Please file an issue to the github repository.
The following information was provided: {msg}"
    )
}

pub(crate) trait IceExt<T> {
    #[cold]
    fn unwrap_or_ice(self) -> T;

    #[cold]
    fn unwrap_or_ice_msg(self, msg: &str) -> T;
}

impl<T> IceExt<T> for Option<T> {
    #[cold]
    fn unwrap_or_ice(self) -> T {
        match self {
            Some(inner) => inner,
            None => panic!(
                "The compiler/interpreter panicked.
Please file an issue to the github repository."
            ),
        }
    }

    #[cold]
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
    #[cold]
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

    #[cold]
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
