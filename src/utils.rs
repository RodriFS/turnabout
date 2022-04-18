/// It's like Any but it's not short-circuiting
pub trait LooseAnyExt: Iterator {
    fn loose_any<F>(self, f: F) -> bool
    where
        F: Fn(Self::Item) -> bool,
        Self: Sized,
    {
        self.fold(false, |mut acc, next| {
            let stop = f(next);
            if stop {
                acc = stop;
            }
            return acc;
        })
    }
}

impl<I: Iterator> LooseAnyExt for I {}
