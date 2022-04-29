use std::ops::*;
use std::ptr::NonNull;

use aed_common::messages;
use caer_types::{layout, op};
use ordered_float::OrderedFloat;
use pinion::{pinion_export, PinionData};

use crate::arg_pack::ProcPack;
use crate::rtti::RttiRef;
use crate::runtime::Runtime;
use crate::string::{resolve_string, RtString};

// null must be first
// TODO: not copy?
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone, PinionData)]
#[repr(C, u8)]
pub enum Val {
    Null = layout::VAL_DISCRIM_NULL,
    Float(OrderedFloat<f32>) = layout::VAL_DISCRIM_FLOAT,
    String(Option<NonNull<RtString>>) = layout::VAL_DISCRIM_STRING,
    Ref(RttiRef) = layout::VAL_DISCRIM_REF,
}

#[pinion_export]
pub fn rt_val_binary_op(rt: &mut Runtime, op: op::BinaryOp, lhs: &Val, rhs: &Val, out: &mut Val) {
    match op {
        op::BinaryOp::Eq | op::BinaryOp::Ne | op::BinaryOp::Equiv | op::BinaryOp::NotEquiv => {
            *out = Val::handle_equality(rt, op, lhs, rhs);
        },
        _ => {},
    }

    let mut lhs = *lhs;
    if let Val::Null = lhs {
        lhs = rhs.zero_value();
    }

    *out = match lhs {
        Val::Null => Val::Null,
        Val::Float(lval) => {
            Val::Float(Val::binary_arithm(op, lval.into(), rhs.cast_float().into()).into())
        },
        Val::String(lval) => {
            if op != op::BinaryOp::Add {
                unimplemented!("RTE badop")
            }
            // TODO reconsider this, DM doesn't allow "e" + 2
            let rval = rhs.cast_string(rt);
            Val::String(Some(
                RtString::from_str(format!("{}{}", resolve_string(lval), rval)).heapify(&rt.alloc),
            ))
        },
        Val::Ref(_) => unimplemented!("overloads"),
    }
}

// bad, needed for now
#[pinion_export]
pub fn rt_val_cast_string_val(val: &Val, rt: &mut Runtime) -> NonNull<RtString> {
    RtString::from_str(val.cast_string(rt)).heapify(&rt.alloc)
}

#[pinion_export]
pub fn rt_val_to_switch_disc(val: &Val) -> u32 {
    match val {
        Val::Float(val) => (val.into_inner() != 0.0) as u32,
        Val::Null => 0,
        Val::String(None) => 0,
        Val::String(Some(s)) => !(unsafe { s.as_ref() }.as_str().is_empty()) as u32,
        Val::Ref(_ptr) => 1,
    }
}

#[pinion_export]
pub fn rt_val_print(val: &Val, rt: &mut Runtime) {
    let s = match val {
        Val::Null => "null".into(),
        Val::Float(n) => format!("{}", n),
        Val::String(None) => "".into(),
        Val::String(Some(s)) => unsafe { s.as_ref() }.as_str().into(),
        _ => val.cast_string(rt),
    };
    println!("{}", s);
    rt.send_message(&messages::Server::Message(s));
}

#[pinion_export]
pub fn rt_val_call_proc(
    val: &Val, proc_name: &RtString, args: &ProcPack, rt: &mut Runtime, out: &mut Val,
) {
    val.call_proc(proc_name, args, rt, out)
}

//this whole block is an awful mess, TODO: fix at some point
impl Val {
    fn handle_equality(_rt: &mut Runtime, op: op::BinaryOp, lhs: &Val, rhs: &Val) -> Val {
        if let Val::Ref(_) = lhs {
            unimplemented!("overload case for lhs datum");
        }

        let bres = match op {
            op::BinaryOp::Eq => Val::basic_eq(*lhs, *rhs),
            op::BinaryOp::Ne => !Val::basic_eq(*lhs, *rhs),
            _ => unimplemented!("{:?}", op),
        };

        Val::Float((bres as usize as f32).into())
    }

    fn basic_eq(lhs: Val, rhs: Val) -> bool {
        #[derive(PartialEq)]
        enum Equatable<'a> {
            Null,
            Float(OrderedFloat<f32>),
            String(&'a str),
        }

        let to_equatable = |v| match v {
            Val::Null => Some(Equatable::Null),
            Val::Float(n) => Some(Equatable::Float(n)),
            Val::String(None) => Some(Equatable::String("")),
            Val::String(Some(s)) => Some(Equatable::String(unsafe { s.as_ref() }.as_str())),
            _ => None,
        };

        match (to_equatable(lhs), to_equatable(rhs)) {
            (Some(lhs_eqt), Some(rhs_eqt)) => lhs_eqt == rhs_eqt,
            _ => false,
        }
    }

    pub fn call_proc(self, proc_name: &RtString, args: &ProcPack, rt: &mut Runtime, out: &mut Val) {
        match self {
            Val::Ref(rtti_ref) => {
                // TODO: include src
                let lookup_fn = rtti_ref.vptr.proc_lookup;
                let proc_fn = lookup_fn(proc_name, rt);
                proc_fn(args, rt, out)
            },
            _ => panic!("RTE can't call proc on val {:?}", self),
        }
    }

    /*pub fn binary_op_const(op: op::BinaryOp, lhs: &Val, rhs: &Val, st: &mut StringTable) -> Val {
        unimplemented!();
        /*let mut lhs = *lhs;

        if let Val::Null = lhs {
            lhs = rhs.zero_value();
        }

        match lhs {
            Val::Null => Val::Null,
            Val::Float(lval) => {
                Val::Float(Val::binary_arithm(op, lval.into(), rhs.cast_float().into()).into())
            },
            Val::String(lval) => {
                if op != op::BinaryOp::Add {
                    unimplemented!("RTE badop")
                }
                // TODO reconsider this, DM doesn't allow "e" + 2. compile error?
                let rval = rhs.cast_string_const(st);
                Val::String(st.concat(lval, rval))
            },
            Val::Ref(_) => unimplemented!(),
        }*/
    }*/

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

            #[allow(clippy::float_cmp)]
            op::BinaryOp::Eq => (l == r) as u8 as f32,
            #[allow(clippy::float_cmp)]
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

    #[inline]
    pub fn try_cast_float(&self) -> Option<OrderedFloat<f32>> {
        match self {
            Val::Null => Some(0f32.into()),
            Val::Float(f) => Some(*f),
            _ => None,
        }
    }

    pub fn cast_float(&self) -> OrderedFloat<f32> {
        self.try_cast_float()
            .unwrap_or_else(|| unimplemented!("RTE badcast: {:?} -> f", self))
    }

    #[inline]
    pub fn try_cast_int(&self) -> Option<i64> {
        match self {
            Val::Null => Some(0),
            Val::Float(f) => Some(f.into_inner() as _),
            _ => None,
        }
    }

    pub fn cast_int(&self) -> i64 {
        self.try_cast_int()
            .unwrap_or_else(|| unimplemented!("RTE badcast: {:?} -> i", self))
    }

    // TODO: use cow
    fn cast_string(&self, _rt: &mut Runtime) -> String {
        match self {
            Val::Null => "null".into(),
            Val::Float(n) => n.to_string(),
            //Val::Int(n) => n.to_string(),
            Val::String(s) => resolve_string(*s).into(),
            Val::Ref(_rr) => {
                todo!("ref stringing");
                /*let datum = unsafe { rr.ptr.as_ref() };
                let dty = datum.ty;
                match rt.env.type_tree.types[dty].specialization {
                    Specialization::List => {
                        let list_ptr = dp.cast();
                        let list: &List = unsafe { list_ptr.as_ref() };
                        format!("{:?}", list)
                    },
                    Specialization::Datum => rt.env.type_tree.types[dty].path_string.clone(),
                }*/
            },
        }
    }

    /*fn cast_string_const(&self, st: &mut StringTable) -> StringId {
        match self {
            Val::Null => st.put("null"),
            Val::Float(n) => st.put(n.to_string()),
            Val::String(s) => st.put(resolve_string(*s)),
            Val::Ref(_dp) => unimplemented!(),
        }
    }*/

    fn zero_value(&self) -> Val {
        match self {
            Val::Null => Val::Null,
            Val::Float(_) => Val::Float(0f32.into()),
            Val::String(_) => Val::String(None),
            Val::Ref(_) => Val::Null,
        }
    }
}
