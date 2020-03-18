use cstub_bindgen_macro::expose_c_stubs;
use std::mem;
use crate::op;

// null must be first
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[repr(C, u32)]
pub enum Val {
    Null,
    Float(f32),
    Int(i32),
}

#[expose_c_stubs(rt_val)]
impl Val {
    fn float(&mut self, val: f32) {
        mem::forget(mem::replace(self, Val::Float(val)));
    }

    fn int(&mut self, val: i32) {
        mem::forget(mem::replace(self, Val::Int(val)));
    }

    fn binary_op(&mut self, op: u32, lhs: &Val, rhs: &Val) {
        let op_enum = unsafe {
            std::mem::transmute::<u32, op::BinaryOp>(op)
        };

        let res = match op_enum {
            op::BinaryOp::Add => Val::add(lhs, rhs),
            op::BinaryOp::Sub => Val::sub(lhs, rhs),
            op::BinaryOp::Mul => Val::mul(lhs, rhs),
            op::BinaryOp::Div => Val::div(lhs, rhs),
            _ => unimplemented!("{:?}", op_enum),
        };

        mem::forget(mem::replace(self, res));
    }

    fn to_switch_disc(&self) -> u32 {
        match self {
            Val::Float(val) => *val as u32,
            Val::Int(val) => *val as u32,
            Val::Null => 0,
        }
    }

    fn print(&self) {
        println!("{:?}", self);
    }

    fn cloned(&mut self) {
        // update refcounts
    }

    fn drop(&mut self) {
        // update refcounts, free memory?
        mem::forget(mem::replace(self, Val::Null));
    }
}

impl Val {
    fn add(lhs: &Val, rhs: &Val) -> Val {
        match &lhs {
            Val::Float(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(lval + rval),
                    Val::Int(rval) => Val::Float(lval + *rval as f32),
                    Val::Null => lhs.clone(),
                }
            },

            Val::Int(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(*lval as f32 + rval),
                    Val::Int(rval) => Val::Int(lval + rval),
                    Val::Null => lhs.clone(),
                }
            },

            Val::Null => Val::Null,
        }
    }

    fn sub(lhs: &Val, rhs: &Val) -> Val {
        match &lhs {
            Val::Float(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(lval - rval),
                    Val::Int(rval) => Val::Float(lval - *rval as f32),
                    Val::Null => lhs.clone(),
                }
            },

            Val::Int(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(*lval as f32 - rval),
                    Val::Int(rval) => Val::Int(lval - rval),
                    Val::Null => lhs.clone(),
                }
            },

            Val::Null => Val::Null,
        }
    }

    fn mul(lhs: &Val, rhs: &Val) -> Val {
        match &lhs {
            Val::Float(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(lval * rval),
                    Val::Int(rval) => Val::Float(lval * *rval as f32),
                    Val::Null => lhs.clone(),
                }
            },

            Val::Int(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(*lval as f32 * rval),
                    Val::Int(rval) => Val::Int(lval * rval),
                    Val::Null => lhs.clone(),
                }
            },

            Val::Null => Val::Null,
        }
    }

    fn div(lhs: &Val, rhs: &Val) -> Val {
        match &lhs {
            Val::Float(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(lval / rval),
                    Val::Int(rval) => Val::Float(*lval as f32 / *rval as f32),
                    Val::Null => lhs.clone(),
                }
            },

            Val::Int(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(*lval as f32 / *rval as f32),
                    Val::Int(rval) => Val::Float(*lval as f32 / *rval as f32),
                    Val::Null => lhs.clone(),
                }
            },

            Val::Null => Val::Null,
        }
    }
}
