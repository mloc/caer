use index_vec::define_index_type;
use pinion::PinionData;

macro_rules! define_caer_id {
    ($name:ident: $pty:ident) => {
        define_index_type! {pub struct $name = $pty;}
        impl PinionData for $name {
            type Static = Self;

            fn get_layout(lctx: &mut pinion::layout_ctx::LayoutCtx) -> pinion::layout::Layout {
                $pty::get_layout(lctx)
            }

            unsafe fn validate(ptr: *const u8) {
                $pty::validate(ptr)
            }
        }
    };
}

define_caer_id! {PathTypeId: u32}
define_caer_id! {InstanceTypeId: u32}
define_caer_id! {ProcId: u32}
// format is overriden by runtime stringtable for extra info?
define_caer_id! {StringId: u64}
// closures aren't exactly procs. oh well.
define_caer_id! {FuncId: u64}
define_caer_id! {TypeId: u32}

// Nasty consts
pub const TYPE_ID_ANY: TypeId = TypeId { _raw: 0 };
pub const TYPE_ID_STRING: TypeId = TypeId { _raw: 1 };
// these are very temporary, probably, maybe???
pub const TYPE_ID_LIST: TypeId = TypeId { _raw: 2 };
pub const TYPE_ID_FLOAT: TypeId = TypeId { _raw: 3 };
pub const TYPE_ID_REF_ANY: TypeId = TypeId { _raw: 4 };

pub const INSTANCE_TYPE_ID_STRING: InstanceTypeId = InstanceTypeId { _raw: 0 };
pub const INSTANCE_TYPE_ID_LIST: InstanceTypeId = InstanceTypeId { _raw: 1 };
