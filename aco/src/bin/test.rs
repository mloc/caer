fn print(n: i32) {
    println!("{}", n);
}

fn main() {
    let ctx = aco::Context::create();
    let stack = ctx.make_stack();

    let mut coros = Vec::new();

    for i in 0..10 {
        let coro = aco::Coro::new(&ctx, &stack, print, i);
        coros.push(coro);
    }

    for coro in coros.iter().rev() {
        unsafe {
            ctx.resume(&coro);
        }
    }
}
