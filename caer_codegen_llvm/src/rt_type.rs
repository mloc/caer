use pinion::ptr::Ptr;
use pinion::struct_type;
use pinion::types::*;

struct_type!(
    pub Val {
        disc: Int8, // Discrim
        data: ValUnion, // 16 bytes of data
    },
    false,
    "val",
);

struct_type!(
    pub ValUnion {
        // Value for most, vtable ptr for ref
        low: Int64,
        // Ptr for ref, unused for rest
        high: Int64,
    },
    false,
);

// Argpack bits

struct_type!(
    pub ArgPackTuple {
        name_id: Int64,
        val: Val,
    },
    false,
    "arg_pack_tuple",
);

struct_type!(
    pub ArgPack {
        a: Int64,
        b: Ptr<Val, 0>,
        c: Int64,
        d: Ptr<ArgPackTuple, 0>,
        e: Val,
    },
    false,
    "arg_pack",
);
