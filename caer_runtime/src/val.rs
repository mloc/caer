use cstub_bindgen_macro::expose_c_stubs;
use std::mem;
use crate::op;
use crate::string_table::{StringId, StringTable};
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

#[no_mangle]
pub extern fn rt_val_float(val: f32) -> Val {
    Val::Float(val)
}

#[no_mangle]
pub extern fn rt_val_string(string: StringId) -> Val {
    Val::String(string)
}

#[no_mangle]
pub extern fn rt_val_binary_op(rt: &mut Runtime, op: op::BinaryOp, lhs: Val, rhs: Val) -> Val {
    let mut lhs = lhs;
    if let Val::Null = lhs {
        lhs = rhs.zero_value();
    }

    match lhs {
        Val::Null => Val::Null,
        Val::Float(lval) => Val::Float(Val::binary_arithm(op, lval, rhs.cast_float())),
        Val::String(lval) => {
            if op!= op::BinaryOp::Add {
                unimplemented!("RTE badop")
            }
            // TODO reconsider this, DM doesn't allow "e" + 2
            let rval = rhs.cast_string(rt);
            Val::String(rt.string_table.concat(lval, rval))
        },
        Val::Ref(_) => unimplemented!("overloads"),
    }
}

// bad, needed for now
#[no_mangle]
pub extern fn rt_val_cast_string_val(val: Val, rt: &mut Runtime) -> StringId {
    val.cast_string(rt)
}

#[no_mangle]
pub extern fn rt_val_to_switch_disc(val: Val) -> u32 {
    match val {
        Val::Float(val) => (val != 0.0) as u32,
        Val::Null => 0,
        Val::String(s) => s.is_empty() as u32,
        Val::Ref(_) => 1,
    }
}

#[no_mangle]
pub extern fn rt_val_print(val: Val, rt: &mut Runtime) {
    match val {
        Val::Null => println!("null"),
        Val::Float(n) => println!("{}", n),
        Val::String(s) => println!("{:?}", rt.string_table.get(s)),
        // disgusting
        _ => {
            let sid = val.cast_string(rt);
            println!("{}", rt.string_table.get(sid));
        },
    }
}

#[no_mangle]
pub extern fn rt_val_cloned(val: Val) {
    if let Val::Ref(_) = val {
        //unimplemented!("update refcounts")
    }
}

// this is not Drop and shouldn't be treated as such.
#[no_mangle]
pub extern fn rt_val_drop(val: Val) {
    if let Val::Ref(_) = val {
        //unimplemented!("update refcounts")
    }
    //mem::forget(mem::replace(self, Val::Null));
}

#[expose_c_stubs(rt_val)]
impl Val {
    /*fn binary_op(&mut self, rt: &mut Runtime, op: u32, lhs: &Val, rhs: &Val) {
      let op_enum = unsafe {
      std::mem::transmute::<u32, op::BinaryOp>(op)
      };

      let mut lhs = *lhs;

      if let Val::Null = lhs {
      lhs = rhs.zero_value();
      }

      let res = match lhs {
      Val::Null => Val::Null,
      Val::Float(lval) => Val::Float(Val::binary_arithm(op_enum, lval, rhs.cast_float())),
      Val::String(lval) => {
      if op_enum != op::BinaryOp::Add {
      unimplemented!("RTE badop")
      }
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
}*/

}

//this whole block is an awful mess, TODO: fix at some point
impl Val {
    pub fn binary_op_const(op: op::BinaryOp, lhs: &Val, rhs: &Val, st: &mut StringTable) -> Val {
        let mut lhs = *lhs;

        if let Val::Null = lhs {
            lhs = rhs.zero_value();
        }

        match lhs {
            Val::Null => Val::Null,
            Val::Float(lval) => Val::Float(Val::binary_arithm(op, lval, rhs.cast_float())),
            Val::String(lval) => {
                if op != op::BinaryOp::Add {
                    unimplemented!("RTE badop")
                }
                // TODO reconsider this, DM doesn't allow "e" + 2. compile error?
                let rval = rhs.cast_string_const(st);
                Val::String(st.concat(lval, rval))
            },
            Val::Ref(_) => unimplemented!(),
        }
    }

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

    fn cast_float(&self) -> f32 {
        match self {
            Val::Null => 0f32,
            //Val::Int(i) => *i as f32,
            Val::Float(f) => *f,
            _ => unimplemented!("RTE badcast: {:?}", self),
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

    fn cast_string_const(&self, st: &mut StringTable) -> StringId {
        match self {
            Val::Null => st.put("null"),
            Val::Float(n) => st.put(n.to_string()),
            Val::String(s) => *s,
            Val::Ref(dp) => unimplemented!(),
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
