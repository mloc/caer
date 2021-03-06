use std::mem::size_of;

use caer_types::layout;
use inkwell::types::BasicType;

/// Only way of getting addressspace(1) in inkwell, for now
pub const GC_ADDRESS_SPACE: inkwell::AddressSpace = inkwell::AddressSpace::Global;

#[derive(Debug)]
pub struct Context<'a, 'ctx> {
    pub llvm_ctx: &'ctx inkwell::context::Context,
    pub builder: &'a inkwell::builder::Builder<'ctx>,
    pub module: &'a inkwell::module::Module<'ctx>,
    pub rt: RtFuncs<'ctx>,
}

impl<'a, 'ctx> Context<'a, 'ctx> {
    pub fn new(
        llctx: &'ctx inkwell::context::Context, llmod: &'a inkwell::module::Module<'ctx>,
        llbuild: &'a inkwell::builder::Builder<'ctx>,
    ) -> Self {
        let rt = RtFuncs::new(llctx, llmod);

        Self {
            builder: llbuild,
            module: llmod,
            llvm_ctx: llctx,
            rt,
        }
    }

    // wrong spot for this
    pub fn make_vtable_lookup(
        &self, vt_global: inkwell::values::GlobalValue<'ctx>,
    ) -> Vec<inkwell::values::FunctionValue<'ctx>> {
        let mut vt_lookup = Vec::new();
        for (i, ty) in self
            .rt
            .ty
            .vt_entry_type
            .get_field_types()
            .iter()
            .enumerate()
        {
            let func_ty = ty.fn_type(&[self.rt.ty.datum_common_type_ptr.into()], false);
            let func =
                self.module
                    .add_function(&format!("vtable_lookup_field_{}", i), func_ty, None);

            let entry_block = self.llvm_ctx.append_basic_block(func, "entry");
            self.builder.position_at_end(entry_block);

            let datum_ptr = func.get_first_param().unwrap().into_pointer_value();
            let ty_id_ptr = unsafe {
                self.builder.build_in_bounds_gep(
                    datum_ptr,
                    &[
                        self.llvm_ctx.i32_type().const_zero(),
                        self.llvm_ctx
                            .i32_type()
                            .const_int(layout::DATUM_TY_FIELD_OFFSET, false),
                    ],
                    "ty_id_ptr",
                )
            };
            let ty_id = self.builder.build_load(ty_id_ptr, "ty_id").into_int_value();

            let field_ptr = unsafe {
                self.builder.build_in_bounds_gep(
                    vt_global.as_pointer_value(),
                    &[
                        self.llvm_ctx.i32_type().const_zero(),
                        ty_id,
                        self.llvm_ctx.i32_type().const_int(i as u64, false),
                    ],
                    &format!("vtable_field_{}_ptr", i),
                )
            };
            let field = self
                .builder
                .build_load(field_ptr, &format!("vtable_field_{}", i));

            self.builder.build_return(Some(&field));

            vt_lookup.push(func);
        }

        vt_lookup
    }
}

// TODO: probably move out of context
// TODO: redo all of this to a friendlier system
#[derive(Debug)]
pub struct RtFuncTyBundle<'ctx> {
    // TODO: undo this, it's a hack to clean up optimized output
    pub val_type: inkwell::types::StructType<'ctx>,
    pub val_type_ptr: inkwell::types::PointerType<'ctx>,
    //val_type: inkwell::types::IntType<'ctx>,
    pub opaque_type: inkwell::types::StructType<'ctx>,
    pub opaque_type_ptr: inkwell::types::PointerType<'ctx>,

    pub arg_pack_type: inkwell::types::StructType<'ctx>,
    pub arg_pack_tuple_type: inkwell::types::StructType<'ctx>,

    pub proc_type: inkwell::types::FunctionType<'ctx>,
    pub closure_type: inkwell::types::FunctionType<'ctx>,

    pub heap_header_type: inkwell::types::StructType<'ctx>,
    pub datum_common_type: inkwell::types::StructType<'ctx>,
    pub datum_common_type_ptr: inkwell::types::PointerType<'ctx>,
    pub string_type: inkwell::types::StructType<'ctx>,
    pub string_type_ptr: inkwell::types::PointerType<'ctx>,

    pub rt_type: inkwell::types::ArrayType<'ctx>,
    pub vt_entry_type: inkwell::types::StructType<'ctx>,

    pub landingpad_type: inkwell::types::StructType<'ctx>,
}

impl<'ctx> RtFuncTyBundle<'ctx> {
    fn new(ctx: &'ctx inkwell::context::Context) -> Self {
        let val_type = ctx.struct_type(&[ctx.i32_type().into(), ctx.i64_type().into()], false);
        let val_type_ptr = val_type.ptr_type(GC_ADDRESS_SPACE);

        let opaque_type = ctx.opaque_struct_type("opaque");
        let opaque_type_ptr = opaque_type.ptr_type(inkwell::AddressSpace::Generic);

        let rt_type = ctx
            .i8_type()
            .array_type(size_of::<caer_runtime::runtime::Runtime>() as u32);

        let arg_pack_tuple_type = ctx.struct_type(&[ctx.i64_type().into(), val_type.into()], false);
        let arg_pack_tuple_type_ptr = arg_pack_tuple_type.ptr_type(inkwell::AddressSpace::Generic);

        let arg_pack_type = ctx.struct_type(
            &[
                ctx.i64_type().into(),
                val_type.ptr_type(inkwell::AddressSpace::Generic).into(),
                ctx.i64_type().into(),
                arg_pack_tuple_type_ptr.into(),
                val_type.into(),
            ],
            false,
        );

        let proc_type = ctx.void_type().fn_type(
            &[
                arg_pack_type
                    .ptr_type(inkwell::AddressSpace::Generic)
                    .into(),
                rt_type.ptr_type(inkwell::AddressSpace::Generic).into(),
                val_type_ptr.into(),
            ],
            false,
        );
        let closure_type = ctx.void_type().fn_type(
            &[
                val_type_ptr.into(),
                rt_type.ptr_type(inkwell::AddressSpace::Generic).into(),
                val_type_ptr.into(),
            ],
            false,
        );

        let heap_header_type = ctx.struct_type(
            &[
                // kind
                ctx.i8_type().into(),
                // gc marker
                ctx.i8_type().into(),
            ],
            false,
        );

        let datum_common_type = ctx.struct_type(
            &[
                heap_header_type.into(),
                // ty id
                ctx.i32_type().into(),
            ],
            false,
        );
        let datum_common_type_ptr = datum_common_type.ptr_type(GC_ADDRESS_SPACE);

        let string_type = ctx.struct_type(
            &[
                heap_header_type.into(),
                // size
                ctx.i64_type().into(),
                // ptr
                ctx.i8_type()
                    .ptr_type(inkwell::AddressSpace::Generic)
                    .into(),
            ],
            false,
        );
        let string_type_ptr = string_type.ptr_type(GC_ADDRESS_SPACE);

        let vt_entry_type = ctx.struct_type(
            &[
                // size
                ctx.i64_type().into(),
                // var_get fn ptr
                ctx.void_type()
                    .fn_type(
                        &[
                            datum_common_type_ptr.into(),
                            ctx.i64_type().into(),
                            val_type_ptr.into(),
                        ],
                        false,
                    )
                    .ptr_type(inkwell::AddressSpace::Generic)
                    .into(),
                // var_set fn ptr
                ctx.void_type()
                    .fn_type(
                        &[
                            datum_common_type_ptr.into(),
                            ctx.i64_type().into(),
                            val_type_ptr.into(),
                        ],
                        false,
                    )
                    .ptr_type(inkwell::AddressSpace::Generic)
                    .into(),
                // proc_lookup fn ptr
                proc_type
                    .ptr_type(inkwell::AddressSpace::Generic)
                    .fn_type(
                        &[
                            ctx.i64_type().into(),
                            rt_type.ptr_type(inkwell::AddressSpace::Generic).into(),
                        ],
                        false,
                    )
                    .ptr_type(inkwell::AddressSpace::Generic)
                    .into(),
            ],
            false,
        );

        let landingpad_type = ctx.struct_type(
            &[
                opaque_type.ptr_type(inkwell::AddressSpace::Generic).into(),
                ctx.i32_type().into(),
            ],
            false,
        );

        RtFuncTyBundle {
            val_type,
            val_type_ptr,
            opaque_type,
            opaque_type_ptr,
            arg_pack_type,
            arg_pack_tuple_type,
            proc_type,
            closure_type,
            heap_header_type,
            datum_common_type,
            datum_common_type_ptr,
            string_type,
            string_type_ptr,
            rt_type,
            vt_entry_type,
            landingpad_type,
        }
    }
}

macro_rules! rt_funcs {
    ( $name:ident, [ $( ( $func:ident, $ret:ident ~ $retspec:ident, [ $( $arg:ident ~ $argspec:ident ),* $(,)* ] ) ),* $(,)* ] ) => {
        #[derive(Debug)]
        pub struct $name <'ctx> {
            pub ty: RtFuncTyBundle<'ctx>,
            $(
                pub $func: inkwell::values::FunctionValue<'ctx>,
            )*
        }

        impl<'ctx> $name <'ctx> {
            fn new(ctx: &'ctx inkwell::context::Context, module: &inkwell::module::Module<'ctx>) -> $name<'ctx> {
                assert_eq!(size_of::<caer_runtime::val::Val>(), 16);

                let tyb = RtFuncTyBundle::new(ctx);

                $name {
                    $(
                        $func: module.add_function(stringify!($func),
                            rt_funcs!(@genty ctx $retspec tyb $ret $ret).fn_type(&[
                                $(
                                    rt_funcs!(@genty ctx $argspec tyb $arg $arg).into(),
                                )*
                            ], false),
                        None),
                    )*
                    ty: tyb,
                }
            }
        }
    };

    ( @genty $ctx:ident $spec:ident $tyb:ident val_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident val_type_ptr $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident opaque_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident opaque_type_ptr $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident rt_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident arg_pack_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident vt_entry_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident datum_common_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident string_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident proc_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident $tym:ident $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $ctx.$ty())
    );

    ( @genty @ptrify val , $e:expr) => (
        $e
    );

    ( @genty @ptrify ptr , $e:expr) => (
        $e.ptr_type(GC_ADDRESS_SPACE)
    );

    ( @genty @ptrify gptr , $e:expr) => (
        $e.ptr_type(inkwell::AddressSpace::Generic)
    );
}

rt_funcs! {
    RtFuncs,
    [
        (rt_val_binary_op, void_type~val, [rt_type~gptr, i32_type~val, val_type~ptr, val_type~ptr, val_type~ptr]),
        (rt_val_to_switch_disc, i32_type~val, [val_type~ptr]),
        (rt_val_print, void_type~val, [val_type~ptr, rt_type~gptr]),
        (rt_val_cloned, void_type~val, [val_type~ptr]),
        (rt_val_drop, void_type~val, [val_type~ptr]),
        (rt_val_cast_string_val, string_type~ptr, [val_type~ptr, rt_type~gptr]),
        (rt_val_call_proc, void_type~val, [val_type~ptr, i64_type~val, arg_pack_type~ptr, rt_type~gptr, val_type~ptr]),

        (rt_runtime_init, void_type~val, [rt_type~gptr, i8_type~gptr, i8_type~gptr, vt_entry_type~gptr, opaque_type_ptr~gptr, i64_type~val]),
        (rt_runtime_alloc_datum, datum_common_type~ptr, [rt_type~gptr, i32_type~val]),
        (rt_runtime_concat_strings, string_type~ptr, [rt_type~gptr, string_type~ptr, string_type~ptr]),
        (rt_runtime_suspend, void_type~val, [rt_type~gptr, val_type~ptr]),
        (rt_runtime_spawn_closure, void_type~val, [
            rt_type~gptr,
            // func id of closure
            i64_type~val,
            // # of vals in env, mostly for sanity
            i64_type~val,
            // ptr to env array
            val_type~ptr,
        ]),

        (rt_arg_pack_unpack_into, void_type~val, [arg_pack_type~gptr, val_type_ptr~gptr, i64_type~val, rt_type~gptr]),

        (rt_list_var_get, void_type~val, [opaque_type~ptr, i64_type~val, val_type~ptr]),
        (rt_list_var_set, void_type~val, [opaque_type~ptr, i64_type~val, val_type~ptr]),
        (rt_list_proc_lookup, proc_type~ptr, [opaque_type~ptr, i64_type~val, rt_type~ptr]),

        (rt_throw, void_type~val, [val_type~ptr]),
        (rt_exception_get_val, void_type~val, [opaque_type~gptr, val_type~ptr]),

        (dm_eh_personality, i32_type~val, [i32_type~val, i32_type~val, i64_type~val, opaque_type~ptr, opaque_type~ptr]),
    ]
}
