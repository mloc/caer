use cstub_bindgen_macro::expose_c_stubs;
use std::mem;
use crate::op;
use crate::string_table::StringId;
use crate::runtime::Runtime;
use crate::datum::Datum;
use std::ops::*;

// null must be first
#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
#[repr(C, u32)]
pub enum Val {
    Null,
    Float(f32),
    String(StringId),
    Ref(*mut Datum),
}

#[expose_c_stubs(rt_val)]
impl Val {
    fn float(&mut self, val: f32) {
        mem::forget(mem::replace(self, Val::Float(val)));
    }

    fn string(&mut self, string: StringId) {
        mem::forget(mem::replace(self, Val::String(string)));
    }

    fn binary_op(&mut self, rt: &mut Runtime, op: u32, lhs: &Val, rhs: &Val) {
        let op_enum = unsafe {
            std::mem::transmute::<u32, op::BinaryOp>(op)
        };

        let mut lhs = *lhs;

        if let Val::Null = lhs {
            lhs = lhs.zero_value();
        }

        let res = match lhs {
            Val::Null => Val::Null,
            Val::Float(lval) => Val::Float(Val::binary_arithm(op_enum, lval, rhs.cast_float())),
            Val::String(lval) => {
                if op_enum != op::BinaryOp::Add {
                    unimplemented!("RTE badop")
                }
                // kinda bad, don't need to alloc a new dmstring if rhs is non-str
                // TODO reconsider this, DM doesn't allow "e" + 2
                let rval = rhs.cast_string(rt);
                Val::String(rt.string_table.concat(lval, rval))
            },
            Val::Ref(_) => unimplemented!("overloads"),
        };

        /*let res = match op_enum {
            op::BinaryOp::Add => Val::add(lhs, rhs),
            op::BinaryOp::Sub => Val::sub(lhs, rhs),
            op::BinaryOp::Mul => Val::mul(lhs, rhs),
            op::BinaryOp::Div => Val::div(lhs, rhs),
            _ => unimplemented!("{:?}", op_enum),
        };*/

        mem::forget(mem::replace(self, res));
    }

    fn to_switch_disc(&self) -> u32 {
        match self {
            Val::Float(val) => (*val != 0.0) as u32,
            Val::Null => 0,
            Val::String(s) => s.is_empty() as u32,
            Val::Ref(id) => 1,
        }
    }

    fn print(&self, rt: &mut Runtime) {
        match self {
            Val::Null => println!("null"),
            Val::Float(n) => println!("{}", n),
            Val::String(s) => println!("{:?}", rt.string_table.get(*s)),
            // disgusting
            _ => {
                let sid = self.cast_string(rt);
                println!("{}", rt.string_table.get(sid));
            },
        }
    }

    fn cloned(&mut self) {
        if let Val::Ref(_) = self {
            //unimplemented!("update refcounts")
        }
    }

    // this is not Drop and shouldn't be treated as such.
    fn drop(&mut self) {
        if let Val::Ref(_) = self {
            //unimplemented!("update refcounts")
        }
        mem::forget(mem::replace(self, Val::Null));
    }

    // bad, needed for now
    // TODO better macro support for primitive returns?
    fn cast_string_val(&mut self, val: &Val, rt: &mut Runtime) {
        self.string(val.cast_string(rt));
    }
}

//this whole block is an awful mess, TODO: fix at some point
impl Val {
    // move somewhere else? not very val specific
    fn binary_arithm(op: op::BinaryOp, l: f32, r: f32) -> f32 {
        let f = match op {
            op::BinaryOp::Add => f32::add,
            op::BinaryOp::Sub => f32::sub,
            op::BinaryOp::Mul => f32::mul,
            op::BinaryOp::Div => f32::div,
            op::BinaryOp::Mod => f32::rem,
            op::BinaryOp::Pow => f32::powf,
            _ => panic!("unimplemented op {:?}", op),
        };
        f(l, r)
    }
    /*fn add(lhs: &Val, rhs: &Val) -> Val {
        match &lhs {
            Val::Float(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(lval + rval),
                    Val::Null => lhs.clone(),
                    _ => panic!(),
                }
            },

            Val::Ref(_) => {
                unimplemented!("overload +")
            }

            Val::Null => rhs.clone(),
        }
    }

    fn sub(lhs: &Val, rhs: &Val) -> Val {
        match &lhs {
            Val::Float(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(lval - rval),
                    Val::Int(rval) => Val::Float(lval - *rval as f32),
                    Val::Null => lhs.clone(),
                    _ => panic!(),
                }
            },

            Val::Int(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(*lval as f32 - rval),
                    Val::Int(rval) => Val::Int(lval - rval),
                    Val::Null => lhs.clone(),
                    _ => panic!(),
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
                    _ => panic!(),
                }
            },

            Val::Int(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(*lval as f32 * rval),
                    Val::Int(rval) => Val::Int(lval * rval),
                    Val::Null => lhs.clone(),
                    _ => panic!(),
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
                    _ => panic!(),
                }
            },

            Val::Int(lval) => {
                match rhs {
                    Val::Float(rval) => Val::Float(*lval as f32 / *rval as f32),
                    Val::Int(rval) => Val::Float(*lval as f32 / *rval as f32),
                    Val::Null => lhs.clone(),
                    _ => panic!(),
                }
            },

            Val::Null => Val::Null,
        }
    }*/

    /*fn cast_int(&self) -> Val {
        let n = match self {
            Val::Null => 0,
            Val::Int(i) => *i,
            Val::Float(f) => *f as i32,
            _ => unimplemented!("RTE badcast"),
        };

        Val::Int(n)
    }*/

    fn cast_float(&self) -> f32 {
        match self {
            Val::Null => 0f32,
            //Val::Int(i) => *i as f32,
            Val::Float(f) => *f,
            _ => unimplemented!("RTE badcast"),
        }
    }

    fn cast_string(&self, rt: &mut Runtime) -> StringId {
        match self {
            Val::Null => rt.string_table.put("null"),
            Val::Float(n) => rt.string_table.put(n.to_string()),
            //Val::Int(n) => n.to_string(),
            Val::String(s) => *s,
            Val::Ref(dp) => {
                if dp.is_null() {
                    StringId::new(0)
                } else {
                    unsafe {
                        // TODO: encapsulate env
                        rt.env.type_tree.types[(**dp).ty].path_str
                    }
                }
            },
        }
    }

    fn zero_value(&self) -> Val {
        match self {
            Val::Null => Val::Null,
            Val::Float(_) => Val::Float(0.0),
            Val::String(_) => Val::String(StringId::new(0)),
            // TODO: rip and tear, ptr in ref is Bad
            Val::Ref(_) => Val::Ref(0 as *mut Datum),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
enum List {
}
