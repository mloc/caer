#[macro_export]
macro_rules! index {
    (@munch_inner $inst:ident $acc:ident) => {};

    (@munch_inner $inst:ident $acc:ident . $($tail:tt)*) => {
        $crate::index!(@munch $inst $acc $($tail)*);
    };

    (@munch $inst:ident $acc:ident $part:ident $($tail:tt)*) => {
        let part_str = $crate::index!(@stringify $part);
        #[allow(unused)]
        let (idx, next) = $inst.resolve_index(part_str).expect(concat!("can't index by ", stringify!($part)));
        $acc.push(idx);
        $crate::index!(@munch_inner next $acc $($tail)*);
    };

    (@stringify $part:ident) => {stringify!($part)};
    (@stringify *) => {"*"};

    ($($part:tt)*) => {
        |base: Box<dyn $crate::traits::PinionIndexableType>| {
            let mut acc = vec![0];
            let inst = base;

            $crate::index!(@munch inst acc $($part)*);

            Some(acc)
        }
    };
}
