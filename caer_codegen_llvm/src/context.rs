use std::any::TypeId;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::c_void;
use std::mem::size_of;
use std::rc::Rc;

use caer_runtime::runtime::Runtime;
use caer_runtime::val::Val;
use inkwell::intrinsics::Intrinsic;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum, PointerType};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use pinion::layout::Func;
use pinion::{
    PinionCallBundle, PinionData, PinionEnum, PinionFuncInstance, PinionModule, PinionStruct,
};

use crate::repr::{EnumRepr, ReprManager, StructRepr};
use crate::value::MaybeRet;

pub type ExFunc = <caer_runtime::export::Runtime as PinionModule>::Funcs;
pub type ExMod = <caer_runtime::export::Runtime as PinionModule>::TFuncs;

/// Not really needed currently while using explicit forms- set to same as normal
pub const GC_ADDRESS_SPACE: inkwell::AddressSpace = inkwell::AddressSpace::Generic;

#[derive(Debug)]
pub struct Context<'a, 'ctx> {
    pub llvm_ctx: &'ctx inkwell::context::Context,
    pub builder: &'a inkwell::builder::Builder<'ctx>,
    pub module: &'a inkwell::module::Module<'ctx>,
    pub rt: RtFuncs<'ctx>,

    target_data: TargetData,

    repr_manager: RefCell<ReprManager<'ctx>>,
    funcs: HashMap<ExFunc, (Func<'ctx>, FunctionValue<'ctx>)>,
    newfuncs: HashMap<TypeId, (Func<'ctx>, FunctionValue<'ctx>)>,
}

impl<'a, 'ctx> Context<'a, 'ctx> {
    pub fn new(
        llctx: &'ctx inkwell::context::Context, llmod: &'a inkwell::module::Module<'ctx>,
        llbuild: &'a inkwell::builder::Builder<'ctx>,
    ) -> Self {
        let mut repr_manager = ReprManager::new();
        let funcs_vec = repr_manager.get_all_funcs::<caer_runtime::export::Runtime>(llctx);
        let inter_funcs: Vec<_> = funcs_vec
            .into_iter()
            .map(|(typeid, (layout, ty), id)| {
                let val = llmod.add_function(layout.name, ty, None);
                (typeid, (layout, val), id)
            })
            .collect();

        let funcs = inter_funcs
            .iter()
            .cloned()
            .map(|(_, v, k)| (k, v))
            .collect();
        let newfuncs = inter_funcs
            .iter()
            .cloned()
            .map(|(k, v, _)| (k, v))
            .collect();

        let rt = RtFuncs::new(llctx, llmod, &mut repr_manager);

        // is this.. OK?
        let data_layout = llmod.get_data_layout();
        let target_data = TargetData::create(data_layout.as_str().to_str().unwrap());

        Self {
            builder: llbuild,
            module: llmod,
            llvm_ctx: llctx,
            rt,
            target_data,
            repr_manager: RefCell::new(repr_manager),
            funcs,
            newfuncs,
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

    pub fn get_type<T: PinionData>(&self) -> BasicTypeEnum<'ctx> {
        self.repr_manager
            .borrow_mut()
            .get_type::<T>(self.llvm_ctx)
            .expect("ty must be sized")
    }

    pub fn get_type_ptr<T: PinionData>(&self) -> PointerType<'ctx> {
        self.get_type::<T>()
            .ptr_type(inkwell::AddressSpace::Generic)
    }

    pub fn get_store_size<T: PinionData>(&self) -> u64 {
        let ty = self.get_type::<T>();
        self.target_data.get_store_size(&ty)
    }

    pub unsafe fn const_gep(&self, ptr: PointerValue<'ctx>, indexes: &[u64]) -> PointerValue<'ctx> {
        let gep_indexes: Vec<_> = indexes
            .iter()
            .map(|i| self.llvm_ctx.i32_type().const_int(*i, false))
            .collect();
        self.builder
            .build_in_bounds_gep(ptr, &gep_indexes, "gepiv_ptr")
    }

    // TODO: most callers of this should be using catch machinery
    pub fn build_call<const N: usize, F: PinionFuncInstance, R>(
        &self, cb: PinionCallBundle<N, F, R, BasicValueEnum<'ctx>>,
    ) -> MaybeRet<'ctx, R> {
        let func_typeid = TypeId::of::<F>();
        let func_val = self.newfuncs[&func_typeid].1;
        // TODO: use BMVE in call bundles
        let args_bmve: Vec<_> = cb.args.iter().copied().map(Into::into).collect();
        let ret = self
            .builder
            .build_call(func_val, &args_bmve, "")
            .try_as_basic_value()
            .left();
        MaybeRet::create(ret)
    }

    pub fn get_intrinsic(
        &self, name: &str, param_types: &[BasicTypeEnum],
    ) -> Option<FunctionValue<'ctx>> {
        let intrinsic = Intrinsic::find(name)?;
        intrinsic.get_declaration(self.module, param_types)
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
    pub opaque_type: inkwell::types::StructType<'ctx>,

    pub closure_type: inkwell::types::FunctionType<'ctx>,

    pub landingpad_type: inkwell::types::StructType<'ctx>,
}

impl<'ctx> RtFuncTyBundle<'ctx> {
    fn new(ctx: &'ctx inkwell::context::Context, rm: &mut ReprManager<'ctx>) -> Self {
        let val_type = rm.get_type::<Val>(ctx).unwrap().into_struct_type();
        let val_type_ptr = val_type.ptr_type(GC_ADDRESS_SPACE);

        let opaque_type = rm.get_type::<c_void>(ctx).unwrap().into_struct_type();

        let rt_type = rm.get_type::<Runtime>(ctx).unwrap();
        let closure_type = ctx.void_type().fn_type(
            &[
                val_type_ptr.into(),
                rt_type.ptr_type(inkwell::AddressSpace::Generic).into(),
                val_type_ptr.into(),
            ],
            false,
        );

        let landingpad_type = ctx.opaque_struct_type("landingpad");
        landingpad_type.set_body(
            &[
                opaque_type.ptr_type(inkwell::AddressSpace::Generic).into(),
                ctx.i32_type().into(),
            ],
            false,
        );

        RtFuncTyBundle {
            val_type,
            opaque_type,
            closure_type,
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
            fn new(ctx: &'ctx inkwell::context::Context, module: &inkwell::module::Module<'ctx>, rm: &mut ReprManager<'ctx>) -> $name<'ctx> {
                assert_eq!(size_of::<caer_runtime::val::Val>(), 24);

                let tyb = RtFuncTyBundle::new(ctx, rm);

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

    ( @genty $ctx:ident $spec:ident $tyb:ident opaque_type $ty:ident) => (
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
        (rt_throw, void_type~val, [val_type~ptr]),
        (rt_exception_get_val, void_type~val, [opaque_type~gptr, val_type~ptr]),

        (dm_eh_personality, i32_type~val, [i32_type~val, i32_type~val, i64_type~val, opaque_type~ptr, opaque_type~ptr]),
    ]
}
