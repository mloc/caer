use crate::datum::Datum;
use crate::list::List;
use crate::op;
use crate::runtime::Runtime;
use crate::string_table::{StringId, StringTable};
use crate::arg_pack::ArgPack;
use std::ptr::NonNull;
use std::ops::*;
use ordered_float::OrderedFloat;

// null must be first
// TODO: not copy?
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
#[repr(C, u32)]
pub enum Val {
    Null,
    Float(OrderedFloat<f32>),
    String(StringId),
    Ref(Option<NonNull<Datum>>),
    // TODO: someday, treat as specialized final datum
    ListRef(Option<NonNull<List>>),
}

#[no_mangle]
pub extern "C" fn rt_val_float(val: f32) -> Val {
    Val::Float(val.into())
}

#[no_mangle]
pub extern "C" fn rt_val_string(string: StringId) -> Val {
    Val::String(string)
}

#[no_mangle]
pub extern "C" fn rt_val_binary_op(rt: &mut Runtime, op: op::BinaryOp, lhs: Val, rhs: Val) -> Val {
    match op {
        op::BinaryOp::Eq | op::BinaryOp::Ne | op::BinaryOp::Equiv | op::BinaryOp::NotEquiv => {
            return Val::handle_equality(rt, op, lhs, rhs);
        }
        _ => {}
    }

    let mut lhs = lhs;
    if let Val::Null = lhs {
        lhs = rhs.zero_value();
    }

    match lhs {
        Val::Null => Val::Null,
        Val::Float(lval) => Val::Float(Val::binary_arithm(op, lval.into(), rhs.cast_float().into()).into()),
        Val::String(lval) => {
            if op != op::BinaryOp::Add {
                unimplemented!("RTE badop")
            }
            // TODO reconsider this, DM doesn't allow "e" + 2
            let rval = rhs.cast_string(rt);
            Val::String(rt.string_table.concat(lval, rval))
        }
        Val::Ref(_) => unimplemented!("overloads"),
        Val::ListRef(_) => unimplemented!("list softops"),
    }
}

// bad, needed for now
#[no_mangle]
pub extern "C" fn rt_val_cast_string_val(val: Val, rt: &mut Runtime) -> StringId {
    val.cast_string(rt)
}

#[no_mangle]
pub extern "C" fn rt_val_to_switch_disc(val: Val) -> u32 {
    match val {
        Val::Float(val) => (val.into_inner() != 0.0) as u32,
        Val::Null => 0,
        Val::String(s) => s.is_empty() as u32,
        Val::Ref(ptr) => ptr.is_some() as u32,
        Val::ListRef(ptr) => ptr.is_some() as u32,
    }
}

#[no_mangle]
pub extern "C" fn rt_val_print(val: Val, rt: &mut Runtime) {
    match val {
        Val::Null => println!("null"),
        Val::Float(n) => println!("{}", n),
        Val::String(s) => println!("{:?}", rt.string_table.get(s)),
        // disgusting
        _ => {
            let sid = val.cast_string(rt);
            println!("{}", rt.string_table.get(sid));
        }
    }
}

#[no_mangle]
pub extern "C" fn rt_val_cloned(val: Val) {
    if let Val::Ref(_) = val {
        //unimplemented!("update refcounts")
    }
}

// this is not Drop and shouldn't be treated as such.
#[no_mangle]
pub extern "C" fn rt_val_drop(val: Val) {
    if let Val::Ref(_) = val {
        //unimplemented!("update refcounts")
    }
    //mem::forget(mem::replace(self, Val::Null));
}

#[no_mangle]
pub extern "C" fn rt_val_call_proc(val: Val, proc_name: StringId, args: &ArgPack, rt: &mut Runtime) -> Val {
    val.call_proc(proc_name, args, rt)
}

//this whole block is an awful mess, TODO: fix at some point
impl Val {
    fn handle_equality(_rt: &mut Runtime, op: op::BinaryOp, lhs: Val, rhs: Val) -> Val {
        if let Val::Ref(_) = lhs {
            unimplemented!("overload case for lhs datum");
        }

        let bres = match op {
            op::BinaryOp::Eq => Val::basic_eq(lhs, rhs),
            op::BinaryOp::Ne => !Val::basic_eq(lhs, rhs),
            _ => unimplemented!("{:?}", op),
        };

        Val::Float((bres as usize as f32).into())
    }

    fn basic_eq(lhs: Val, rhs: Val) -> bool {
        #[derive(PartialEq)]
        enum Equatable {
            Null,
            Float(OrderedFloat<f32>),
            String(StringId),
        }

        let to_equatable = |v| match v {
            Val::Null => Some(Equatable::Null),
            Val::Float(n) => Some(Equatable::Float(n)),
            Val::String(id) => Some(Equatable::String(id)),
            _ => None,
        };

        match (to_equatable(lhs), to_equatable(rhs)) {
            (Some(lhs_eqt), Some(rhs_eqt)) => lhs_eqt == rhs_eqt,
            _ => false,
        }
    }

    pub fn call_proc(self, proc_name: StringId, args: &ArgPack, rt: &mut Runtime) -> Val {
        match self {
            Val::Ref(Some(mut ptr)) => {
                let datum_ref = unsafe { ptr.as_mut() };
                let lookup_fn = rt.vtable[datum_ref.ty].proc_lookup;
                let proc_fn = lookup_fn(proc_name);
                proc_fn(args)
            },
            _ => panic!("can't call proc on val {:?}", self),
        }
    }

    pub fn binary_op_const(op: op::BinaryOp, lhs: &Val, rhs: &Val, st: &mut StringTable) -> Val {
        let mut lhs = *lhs;

        if let Val::Null = lhs {
            lhs = rhs.zero_value();
        }

        match lhs {
            Val::Null => Val::Null,
            Val::Float(lval) => Val::Float(Val::binary_arithm(op, lval.into(), rhs.cast_float().into()).into()),
            Val::String(lval) => {
                if op != op::BinaryOp::Add {
                    unimplemented!("RTE badop")
                }
                // TODO reconsider this, DM doesn't allow "e" + 2. compile error?
                let rval = rhs.cast_string_const(st);
                Val::String(st.concat(lval, rval))
            }
            Val::Ref(_) => unimplemented!(),
            Val::ListRef(_) => unimplemented!(),
        }
    }

    fn wrap_bitop(f: impl FnOnce(u32, u32) -> u32) -> impl FnOnce(f32, f32) -> f32 {
        |lhs_f, rhs_f| {
            let lhs_i = lhs_f as i32 as u32 & 0xff_ff_ff;
            let rhs_i = rhs_f as i32 as u32 & 0xff_ff_ff;
            let res_i = f(lhs_i, rhs_i) & 0xff_ff_ff;
            res_i as f32
        }
    }

    // move somewhere else? not very val specific
    fn binary_arithm(op: op::BinaryOp, l: f32, r: f32) -> f32 {
        match op {
            op::BinaryOp::Add => f32::add(l, r),
            op::BinaryOp::Sub => f32::sub(l, r),
            op::BinaryOp::Mul => f32::mul(l, r),
            op::BinaryOp::Div => f32::div(l, r),
            op::BinaryOp::Mod => f32::rem(l, r),
            op::BinaryOp::Pow => f32::powf(l, r),
            op::BinaryOp::Eq => (l == r) as u8 as f32,
            op::BinaryOp::Ne => (l != r) as u8 as f32,
            op::BinaryOp::Gt => (l > r) as u8 as f32,
            op::BinaryOp::Ge => (l >= r) as u8 as f32,
            op::BinaryOp::Lt => (l < r) as u8 as f32,
            op::BinaryOp::Le => (l <= r) as u8 as f32,
            op::BinaryOp::Shl => Self::wrap_bitop(u32::shl)(l, r),
            op::BinaryOp::Shr => Self::wrap_bitop(u32::shr)(l, r),
            op::BinaryOp::BitAnd => Self::wrap_bitop(u32::bitand)(l, r),
            op::BinaryOp::BitOr => Self::wrap_bitop(u32::bitor)(l, r),
            op::BinaryOp::BitXor => Self::wrap_bitop(u32::bitxor)(l, r),
            _ => panic!("unimplemented op {:?}", op),
        }
    }

    fn cast_float(&self) -> OrderedFloat<f32> {
        match self {
            Val::Null => 0f32.into(),
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
            Val::Ref(None) => rt.string_table.put("null"),
            Val::Ref(Some(dp)) => {
                unsafe {
                    // TODO: encapsulate env
                    rt.env.type_tree.types[dp.as_ref().ty].path_str
                }
            }
            Val::ListRef(None) => rt.string_table.put("null"),
            Val::ListRef(Some(_)) => rt.string_table.put("/list"),
        }
    }

    fn cast_string_const(&self, st: &mut StringTable) -> StringId {
        match self {
            Val::Null => st.put("null"),
            Val::Float(n) => st.put(n.to_string()),
            Val::String(s) => *s,
            Val::Ref(_dp) => unimplemented!(),
            Val::ListRef(_lp) => unimplemented!(),
        }
    }

    fn zero_value(&self) -> Val {
        match self {
            Val::Null => Val::Null,
            Val::Float(_) => Val::Float(0f32.into()),
            Val::String(_) => Val::String(StringId::new(0)),
            // TODO: rip and tear, ptr in ref is Bad
            Val::Ref(_) => Val::Ref(None),
            Val::ListRef(_) => Val::ListRef(None),
        }
    }
}
