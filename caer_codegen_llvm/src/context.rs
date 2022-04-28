use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::size_of;
use std::rc::Rc;

use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::FunctionValue;
use pinion::layout::{Func, Layout};
use pinion::layout_ctx::{LayoutCtx, LayoutId};
use pinion::types::Primitive;
use pinion::{PinionEnum, PinionModule, PinionStruct};

use crate::repr::{EnumRepr, ReprManager, StructRepr};

pub type ExFunc = <caer_runtime::export::Runtime as PinionModule>::Funcs;

/// Not really needed currently while using explicit forms- set to same as normal
pub const GC_ADDRESS_SPACE: inkwell::AddressSpace = inkwell::AddressSpace::Generic;

#[derive(Debug)]
pub struct Context<'a, 'ctx> {
    pub llvm_ctx: &'ctx inkwell::context::Context,
    pub builder: &'a inkwell::builder::Builder<'ctx>,
    pub module: &'a inkwell::module::Module<'ctx>,
    pub rt: RtFuncs<'ctx>,

    repr_manager: RefCell<ReprManager<'ctx>>,
    funcs: HashMap<ExFunc, (Func, FunctionValue<'ctx>)>,
}

impl<'a, 'ctx> Context<'a, 'ctx> {
    pub fn new(
        llctx: &'ctx inkwell::context::Context, llmod: &'a inkwell::module::Module<'ctx>,
        llbuild: &'a inkwell::builder::Builder<'ctx>,
    ) -> Self {
        let rt = RtFuncs::new(llctx, llmod);

        let mut repr_manager = ReprManager::new();
        let funcs_vec = repr_manager.get_all_funcs::<caer_runtime::export::Runtime>(llctx);
        let funcs = funcs_vec
            .into_iter()
            .map(|(id, (layout, ty))| {
                let val = llmod.add_function(layout.name, ty, None);
                (id, (layout, val))
            })
            .collect();

        Self {
            builder: llbuild,
            module: llmod,
            llvm_ctx: llctx,
            rt,
            repr_manager: RefCell::new(repr_manager),
            funcs,
        }
    }

    // TODO: collapse the monomorphing here..? it's nice for the API but bad for code size
    pub fn get_struct<T: PinionStruct>(&self) -> Rc<StructRepr<'ctx>> {
        self.repr_manager
            .borrow_mut()
            .get_struct::<T>(self.llvm_ctx)
    }

    pub fn get_enum<T: PinionEnum>(&self) -> Rc<EnumRepr<'ctx>> {
        self.repr_manager.borrow_mut().get_enum::<T>(self.llvm_ctx)
    }

    pub fn get_func(&self, func: ExFunc) -> FunctionValue<'ctx> {
        self.funcs.get(&func).unwrap().1
    }

    // wrong spot for this
    /*pub fn make_vtable_lookup(
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
    }*/
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

    pub rt_type: inkwell::types::StructType<'ctx>,
    pub vt_entry_type: inkwell::types::StructType<'ctx>,
    pub vt_entry_type_ptr: inkwell::types::PointerType<'ctx>,

    pub ref_type: inkwell::types::StructType<'ctx>,

    pub landingpad_type: inkwell::types::StructType<'ctx>,
}

impl<'ctx> RtFuncTyBundle<'ctx> {
    fn new(ctx: &'ctx inkwell::context::Context) -> Self {
        let val_union_type = ctx.struct_type(
            &[
                // Value for most, vtable ptr for ref
                ctx.i64_type().into(),
                // Ptr for ref, unused for rest
                ctx.i64_type().into(),
            ],
            false,
        );

        let val_type = ctx.named_struct_type(
            &[
                // Disc
                ctx.i8_type().into(),
                val_union_type.into(),
            ],
            false,
            "val",
        );
        let val_type_ptr = val_type.ptr_type(GC_ADDRESS_SPACE);

        let opaque_type = ctx.opaque_struct_type("opaque");
        let opaque_type_ptr = opaque_type.ptr_type(inkwell::AddressSpace::Generic);

        let rt_type = ctx.named_struct_type(
            &[ctx
                .i8_type()
                .array_type(size_of::<caer_runtime::runtime::Runtime>() as u32)
                .into()],
            false,
            "runtime",
        );

        let arg_pack_tuple_type = ctx.named_struct_type(
            &[ctx.i64_type().into(), val_type.into()],
            false,
            "arg_pack_tuple",
        );
        let arg_pack_tuple_type_ptr = arg_pack_tuple_type.ptr_type(inkwell::AddressSpace::Generic);

        let arg_pack_type = ctx.named_struct_type(
            &[
                ctx.i64_type().into(),
                val_type.ptr_type(inkwell::AddressSpace::Generic).into(),
                ctx.i64_type().into(),
                arg_pack_tuple_type_ptr.into(),
                val_type.into(),
            ],
            false,
            "arg_pack",
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

        let heap_header_type = ctx.named_struct_type(
            &[
                // gc marker
                ctx.struct_type(&[ctx.i8_type().into()], false).into(),
            ],
            false,
            "heap_header",
        );

        let datum_common_type = ctx.named_struct_type(
            &[
                heap_header_type.into(),
                // ty id
                //ctx.i32_type().into(),
            ],
            false,
            "datum_header",
        );
        let datum_common_type_ptr = datum_common_type.ptr_type(GC_ADDRESS_SPACE);

        let string_type = ctx.named_struct_type(
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
            "rt_string",
        );
        let string_type_ptr = string_type.ptr_type(GC_ADDRESS_SPACE);

        let vt_entry_type = ctx.named_struct_type(
            &[
                // type id
                ctx.i32_type().into(),
                // size
                ctx.i64_type().into(),
                // var_get fn ptr
                ctx.void_type()
                    .fn_type(
                        &[
                            datum_common_type_ptr.into(),
                            string_type_ptr.into(),
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
                            string_type_ptr.into(),
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
                            string_type_ptr.into(),
                            rt_type.ptr_type(inkwell::AddressSpace::Generic).into(),
                        ],
                        false,
                    )
                    .ptr_type(inkwell::AddressSpace::Generic)
                    .into(),
            ],
            false,
            "vt_entry",
        );
        let vt_entry_type_ptr = vt_entry_type.ptr_type(inkwell::AddressSpace::Generic);

        // Fat pointer, rename?
        let ref_type = ctx.named_struct_type(
            &[
                vt_entry_type_ptr.into(),
                opaque_type.ptr_type(GC_ADDRESS_SPACE).into(),
            ],
            false,
            "ref",
        );

        let landingpad_type = ctx.named_struct_type(
            &[
                opaque_type.ptr_type(inkwell::AddressSpace::Generic).into(),
                ctx.i32_type().into(),
            ],
            false,
            "landingpad",
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
            vt_entry_type_ptr,
            ref_type,
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
                assert_eq!(size_of::<caer_runtime::val::Val>(), 24);

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
        (rt_val_call_proc, void_type~val, [val_type~ptr, string_type~ptr, arg_pack_type~ptr, rt_type~gptr, val_type~ptr]),

        (rt_runtime_init, void_type~val, [rt_type~gptr, i8_type~gptr, i8_type~gptr, vt_entry_type~gptr, opaque_type_ptr~gptr, i64_type~val]),
        (rt_runtime_alloc_datum, datum_common_type~ptr, [rt_type~gptr, i32_type~val]),
        (rt_runtime_string_to_id, i64_type~val, [rt_type~gptr, string_type~ptr]),
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
