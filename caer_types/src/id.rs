use index_vec::define_index_type;

define_index_type!{pub struct TypeId = u32;}
define_index_type!{pub struct ProcId = u32;}
// format is overriden by runtime stringtable for extra info?
define_index_type!{pub struct StringId = u64;}
define_index_type!{pub struct FuncId = u64;}
    // closures aren't exactly procs. oh well.
