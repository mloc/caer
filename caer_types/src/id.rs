use index_vec::define_index_type;

define_index_type! {pub struct PathTypeId = u32;}
define_index_type! {pub struct InstanceTypeId = u32;}
define_index_type! {pub struct ProcId = u32;}
// format is overriden by runtime stringtable for extra info?
define_index_type! {pub struct StringId = u64;}
// closures aren't exactly procs. oh well.
define_index_type! {pub struct FuncId = u64;}
define_index_type! {pub struct TypeId = u32;}

// Nasty consts
pub const TYPE_ID_ANY: TypeId = TypeId { _raw: 0 };
pub const TYPE_ID_STRING: TypeId = TypeId { _raw: 1 };
// these are very temporary, probably, maybe???
pub const TYPE_ID_LIST: TypeId = TypeId { _raw: 2 };
pub const TYPE_ID_FLOAT: TypeId = TypeId { _raw: 3 };
pub const TYPE_ID_REF_ANY: TypeId = TypeId { _raw: 4 };

pub const INSTANCE_TYPE_ID_STRING: InstanceTypeId = InstanceTypeId { _raw: 0 };
pub const INSTANCE_TYPE_ID_LIST: InstanceTypeId = InstanceTypeId { _raw: 1 };
