#[macro_export]
macro_rules! index {
    (@munch $inst:ident $acc:ident) => {};

    (@munch $inst:ident $acc:ident . $part:tt $($tail:tt)*) => {
        println!("in munch");
        let part_str = $crate::index!(@stringify $part);
        #[allow(unused)]
        let (idx, next) = $inst.resolve_index(part_str).unwrap();
        $acc.push(idx);
        $crate::index!(@munch next $acc $($tail)*);
    };

    (@stringify $part:ident) => {stringify!($part)};
    (@stringify *) => {"*"};

    ($base:ty, $first:tt $($tail:tt)*) => {
        {
            let base_inst = <$base>::create_empty();
            let mut acc = Vec::new();

            println!("first munch");
            let first_str = $crate::index!(@stringify $first);
            #[allow(unused)]
            let (idx, next) = base_inst.resolve_index(first_str).unwrap();
            acc.push(idx);

            $crate::index!(@munch next acc $($tail)*);

            Some(acc)
        }
    };
}
