use crate::cfg;
use dreammaker::ast;

pub struct ProcBuilder {
    pub proc: cfg::Proc,
}

impl ProcBuilder {
    pub fn new() -> Self {
        Self {
            proc: cfg::Proc::new(),
        }
    }
    pub fn build_block(&mut self, stmts: &[ast::Statement]) -> cfg::Block {
        let mut block = cfg::Block::new();

        for stmt in stmts.iter() {
            println!("{:?}", stmt);
            match stmt {
                ast::Statement::Var(v) => {
                    if let Some(ref expr) = v.value {
                        self.build_assign(&v.name, expr, &mut block);
                    }
                },

                ast::Statement::Expr(expr) => {
                    match expr {
                        ast::Expression::AssignOp { op, lhs, rhs } => {
                            let var = match lhs.as_ref() {
                                ast::Expression::Base { unary, term, follow } => {
                                    match term {
                                        ast::Term::Ident(ref s) => s,
                                        _ => unimplemented!(),
                                    }
                                },

                                _ => unimplemented!(),
                            };

                            self.build_assign(var, &*rhs, &mut block);
                        },

                        ast::Expression::BinaryOp { op, lhs, rhs } => {
                            let local = self.proc.add_local(None);
                            let res = self.build_expr(rhs, &mut block);
                            block.ops.push(cfg::Op::Mov(local, res));
                            match op {
                                ast::BinaryOp::LShift => {
                                    block.ops.push(cfg::Op::Put(local));
                                },
                                _ => unimplemented!(),
                            };
                        },

                        _ => unimplemented!(),
                    }
                },

                _ => unimplemented!(),
            }
        }

        block
    }

    fn build_assign(&mut self, var: &str, expr: &ast::Expression, block: &mut cfg::Block) {
        let var_id = self.proc.lookup_var(var).unwrap();
        let asg_expr = self.build_expr(expr, block);
        block.ops.push(cfg::Op::Mov(var_id, asg_expr));
    }

    fn build_expr(&mut self, expr: &ast::Expression, block: &mut cfg::Block) -> cfg::Expr {
        match expr {
            ast::Expression::Base { unary, term, follow } => {
                let mut expr = self.build_term(term, block);
                assert!(unary.len() == 0);
                assert!(follow.len() == 0);

                expr
            },

            ast::Expression::BinaryOp { op, lhs, rhs } => {
                let lhs_expr = self.build_expr(lhs, block);
                let rhs_expr = self.build_expr(rhs, block);
                let local = self.proc.add_local(None);

                match op {
                    ast::BinaryOp::Add => {
                        block.ops.push(cfg::Op::Add(local, lhs_expr, rhs_expr));
                    },

                    _ => unimplemented!("{:?}", op),
                }

                cfg::Expr::Place(cfg::Place::Local(local))
            },

            _ => unimplemented!("{:?}", expr),
        }
    }

    fn build_term(&mut self, term: &ast::Term, block: &mut cfg::Block) -> cfg::Expr {
        match term {
            ast::Term::Int(x) => cfg::Expr::Literal(cfg::Literal::Num(*x as f32)),
            ast::Term::Float(x) => cfg::Expr::Literal(cfg::Literal::Num(*x)),
            ast::Term::Ident(var_name) => {
                let var_id = self.proc.lookup_var(var_name).unwrap();
                cfg::Expr::Place(cfg::Place::Local(var_id))
            },
            ast::Term::Call(name, args) => {
                let res = self.proc.add_local(None);

                let arg_exprs: Vec<cfg::Expr> = args.iter().map(|expr| self.build_expr(expr, block)).collect();

                block.ops.push(cfg::Op::Call(res, name.clone(), arg_exprs));

                cfg::Expr::Place(cfg::Place::Local(res))
            },
            _ => unimplemented!("{:?}", term),
        }
    }

}
