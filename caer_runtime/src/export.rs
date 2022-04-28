use pinion::pinion_module;

use crate::{runtime, val};

pinion_module! {
    Runtime,
    [
        val::rt_val_binary_op,
        val::rt_val_cast_string_val,
        val::rt_val_to_switch_disc,
        val::rt_val_print,
        val::rt_val_call_proc,
        runtime::rt_runtime_concat_strings,
    ]
}
