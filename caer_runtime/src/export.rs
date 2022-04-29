use pinion::pinion_module;

use crate::{arg_pack, runtime, val};

pinion_module! {
    Runtime,
    [
        val::rt_val_binary_op,
        val::rt_val_cast_string_val,
        val::rt_val_to_switch_disc,
        val::rt_val_print,
        val::rt_val_call_proc,

        runtime::rt_runtime_init,
        runtime::rt_runtime_concat_strings,
        runtime::rt_runtime_string_to_id,
        runtime::rt_runtime_suspend,
        runtime::rt_runtime_alloc_datum,
        runtime::rt_runtime_spawn_closure,
        runtime::rt_runtime_get_time,

        arg_pack::rt_arg_pack_unpack_into,
    ]
}
