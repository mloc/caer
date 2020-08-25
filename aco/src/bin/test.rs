use aco;

fn print(n: i32) {
    println!("{}", n);
}

fn main() {
    let ctx = aco::context::Context::new();
    let stack = aco::stack::Stack::new();

    let mut coros = Vec::new();

    for i in 0..10 {
        let coro = aco::coro::Coro::new(&ctx, &stack, print, i);
        coros.push(coro);
    }

    for coro in coros.iter().rev() {
        unsafe {
            ctx.resume(&coro);
        }
    }
}
