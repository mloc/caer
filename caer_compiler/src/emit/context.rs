use inkwell;
use std::mem::size_of;
use inkwell::types::BasicType;
use caer_runtime::datum;

#[derive(Debug)]
pub struct Context<'a, 'ctx> {
    pub llvm_ctx: &'ctx inkwell::context::Context,
    pub builder: &'a inkwell::builder::Builder<'ctx>,
    pub module: &'a inkwell::module::Module<'ctx>,
    pub rt: RtFuncs<'ctx>,
}

impl<'a, 'ctx> Context<'a, 'ctx> {
    pub fn new(llctx: &'ctx inkwell::context::Context, llmod: &'a inkwell::module::Module<'ctx>, llbuild: &'a inkwell::builder::Builder<'ctx>) -> Self {
        let rt = RtFuncs::new(llctx, llmod);

        Self {
            builder: llbuild,
            module: llmod,
            llvm_ctx: llctx,
            rt: rt,
        }
    }

    // wrong spot for this
    pub fn make_vtable_lookup(&self, vt_global: inkwell::values::GlobalValue<'ctx>) -> Vec<inkwell::values::FunctionValue<'ctx>> {
        let mut vt_lookup = Vec::new();
        for (i, ty) in self.rt.ty.vt_entry_type.get_field_types().iter().enumerate() {
            let func_ty = ty.fn_type(&[self.rt.ty.datum_common_type_ptr.into()], false);
            let func = self.module.add_function(&format!("vtable_lookup_field_{}", i), func_ty, None);

            let entry_block = self.llvm_ctx.append_basic_block(func, "entry");
            self.builder.position_at_end(entry_block);

            let datum_ptr = func.get_first_param().unwrap().into_pointer_value();
            let ty_id_ptr = unsafe { self.builder.build_in_bounds_gep(datum_ptr, &[
                self.llvm_ctx.i32_type().const_zero(),
                self.llvm_ctx.i32_type().const_int(datum::DATUM_TY_FIELD_OFFSET, false),
            ], "ty_id_ptr") };
            let ty_id = self.builder.build_load(ty_id_ptr, "ty_id").into_int_value();

            let field_ptr = unsafe { self.builder.build_in_bounds_gep(vt_global.as_pointer_value(), &[
                self.llvm_ctx.i32_type().const_zero(),
                ty_id,
                self.llvm_ctx.i32_type().const_int(i as u64, false),
            ], &format!("vtable_field_{}_ptr", i)) };
            let field = self.builder.build_load(field_ptr, &format!("vtable_field_{}", i));

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

    pub arg_pack_type: inkwell::types::StructType<'ctx>,
    pub arg_pack_tuple_type: inkwell::types::StructType<'ctx>,

    pub proc_type: inkwell::types::FunctionType<'ctx>,

    pub datum_common_type: inkwell::types::StructType<'ctx>,
    pub datum_common_type_ptr: inkwell::types::PointerType<'ctx>,

    pub rt_type: inkwell::types::ArrayType<'ctx>,
    pub vt_entry_type: inkwell::types::StructType<'ctx>,
}

impl<'ctx> RtFuncTyBundle<'ctx> {
    fn new(ctx: &'ctx inkwell::context::Context) -> Self {
        let val_type = ctx.struct_type(&[ctx.i32_type().into(), ctx.i64_type().into()], false);
        let val_type_ptr = val_type.ptr_type(inkwell::AddressSpace::Generic);

        let opaque_type = ctx.opaque_struct_type("opaque");

        let arg_pack_tuple_type = ctx.struct_type(&[ctx.i64_type().into(), val_type_ptr.into()], false);
        let arg_pack_tuple_type_ptr = arg_pack_tuple_type.ptr_type(inkwell::AddressSpace::Generic);

        let arg_pack_type = ctx.struct_type(&[ctx.i64_type().into(), val_type_ptr.into(), ctx.i64_type().into(), arg_pack_tuple_type_ptr.into()], false);

        let proc_type = val_type.fn_type(&[arg_pack_type.ptr_type(inkwell::AddressSpace::Generic).into()], false);

        let datum_common_type = ctx.struct_type(&[
            // ref
            ctx.i32_type().into(),
        ], false);
        let datum_common_type_ptr = datum_common_type.ptr_type(inkwell::AddressSpace::Generic);

        let rt_type = ctx.i8_type().array_type(size_of::<caer_runtime::runtime::Runtime>() as u32);
        let vt_entry_type = ctx.struct_type(&[
            // size
            ctx.i64_type().into(),
            // var_index fn ptr
            ctx.i32_type().fn_type(&[ctx.i64_type().into()], false).ptr_type(inkwell::AddressSpace::Generic).into(),
            // var_get fn ptr
            val_type.fn_type(&[datum_common_type_ptr.into(), ctx.i64_type().into()], false).ptr_type(inkwell::AddressSpace::Generic).into(),
            // var_set fn ptr
            ctx.void_type().fn_type(&[datum_common_type_ptr.into(), ctx.i64_type().into(), val_type.into()], false).ptr_type(inkwell::AddressSpace::Generic).into(),
            // proc_lookup fn ptr
            proc_type.ptr_type(inkwell::AddressSpace::Generic).fn_type(&[ctx.i32_type().into()], false).ptr_type(inkwell::AddressSpace::Generic).into(),
        ], false);

        RtFuncTyBundle {
            val_type: val_type,
            val_type_ptr: val_type_ptr,
            opaque_type: opaque_type,
            arg_pack_type: arg_pack_type,
            arg_pack_tuple_type: arg_pack_tuple_type,
            proc_type: proc_type,
            datum_common_type: datum_common_type,
            datum_common_type_ptr: datum_common_type_ptr,
            rt_type: rt_type,
            vt_entry_type: vt_entry_type,
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

    ( @genty $ctx:ident $spec:ident $tyb:ident rt_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident arg_pack_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident vt_entry_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident $tym:ident $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $ctx.$ty())
    );

    ( @genty @ptrify val , $e:expr) => (
        $e
    );

    ( @genty @ptrify ptr , $e:expr) => (
        $e.ptr_type(inkwell::AddressSpace::Generic)
    );
}

rt_funcs!{
    RtFuncs,
    [
        (rt_val_float, val_type~val, [f32_type~val]),
        (rt_val_string, val_type~val, [i64_type~val]),
        (rt_val_binary_op, val_type~val, [rt_type~ptr, i32_type~val, val_type~val, val_type~val]),
        (rt_val_to_switch_disc, i32_type~val, [val_type~val]),
        (rt_val_print, void_type~val, [val_type~val, rt_type~ptr]),
        (rt_val_cloned, void_type~val, [val_type~val]),
        (rt_val_drop, void_type~val, [val_type~val]),
        (rt_val_cast_string_val, i64_type~val, [val_type~val, rt_type~ptr]),

        (rt_runtime_init, void_type~val, [rt_type~ptr, vt_entry_type~ptr]),
        (rt_runtime_alloc_datum, opaque_type~ptr, [rt_type~ptr, i32_type~val]),
        (rt_runtime_concat_strings, i64_type~val, [rt_type~ptr, i64_type~val, i64_type~val]),

        (rt_arg_pack_unpack_into, void_type~val, [arg_pack_type~ptr, val_type_ptr~ptr, i64_type~val, rt_type~ptr]),
    ]
}
