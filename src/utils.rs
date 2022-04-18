pub struct FoldIfNone<I>{
  iter: I,
  shoud_continue: bool
}

impl<I: Iterator> Iterator for FoldIfNone<I>
    where {
      type Item = I::Item;


    fn next(&mut self) -> Option<Self::Item> {
      let next = self.iter.next();
      if next.is_none() {
        self.shoud_continue = false;
      }
      next
    }
}


pub trait FoldIfNoneExt: Iterator {
  fn fold_if_none(self) -> (bool, FoldIfNone<Self>)
  where
      Self: Sized,
  {
    let iterator = FoldIfNone {
      iter: self,
      shoud_continue: false
    };
    (iterator.shoud_continue, iterator)
  }
}

impl<I: Iterator> FoldIfNoneExt for I {}
