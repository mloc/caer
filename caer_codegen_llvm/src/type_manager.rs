use std::cell::RefCell;
use std::ffi::c_void;
use std::mem::size_of;
use std::rc::Rc;

use caer_runtime::runtime::Runtime;
use caer_runtime::val::Val;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, PointerType};
use inkwell::values::FunctionValue;
use pinion::{layout, PinionData, PinionEnum, PinionStruct, PinionTaggedUnion};

use crate::context::{Context, ExFunc, ExRuntime};
use crate::emit_type::EmitType;
use crate::repr::{EnumRepr, ReprManager, StructRepr, TaggedUnionRepr};

#[derive(Debug)]
pub struct TypeManager<'ctx> {
    ctx: &'ctx Context<'ctx>,
    repr_manager: RefCell<ReprManager<'ctx>>,
    pub rt: RtFuncs<'ctx>,
}

impl<'ctx> TypeManager<'ctx> {
    pub fn new(ctx: &'ctx Context<'ctx>) -> Self {
        let mut repr_manager = ReprManager::new(&ctx.llvm_ctx);
        let rt = RtFuncs::new(ctx, &mut repr_manager);
        Self {
            ctx,
            repr_manager: repr_manager.into(),
            rt,
        }
    }

    // TODO: collapse the monomorphing here..? it's nice for the API but bad for code size
    pub fn get_struct<T: PinionStruct>(&self) -> Rc<StructRepr<'ctx>> {
        self.repr_manager.borrow_mut().get_struct::<T>()
    }

    pub fn get_enum<T: PinionEnum>(&self) -> Rc<EnumRepr<'ctx>> {
        self.repr_manager.borrow_mut().get_enum::<T>()
    }

    pub fn get_tagged_union<T: PinionTaggedUnion>(&self) -> Rc<TaggedUnionRepr<'ctx>> {
        self.repr_manager
            .borrow_mut()
            .get_tagged_union::<T>()
    }

    pub fn get_type<T: PinionData>(&self) -> EmitType<'ctx> {
        self.repr_manager.borrow_mut().get_type::<T>()
    }

    pub fn get_llvm_type<T: PinionData>(&self) -> BasicTypeEnum<'ctx> {
        self.get_type::<T>().get_ty().unwrap()
    }

    pub fn get_llvm_type_ptr<T: PinionData>(&self) -> PointerType<'ctx> {
        self.get_llvm_type::<T>()
            .ptr_type(inkwell::AddressSpace::default())
    }

    pub fn get_store_size<T: PinionData>(&self) -> u64 {
        let ty = self.get_llvm_type::<T>();
        self.ctx.target_data.get_store_size(&ty)
    }
}

// TODO: probably move out of here
// TODO: redo all of this to pinion stuff
#[derive(Debug)]
pub struct RtFuncTyBundle<'ctx> {
    // TODO: undo this, it's a hack to clean up optimized output
    pub val_type: inkwell::types::StructType<'ctx>,
    pub opaque_type: inkwell::types::StructType<'ctx>,

    pub closure_type: inkwell::types::FunctionType<'ctx>,

    pub landingpad_type: inkwell::types::StructType<'ctx>,
}

impl<'ctx> RtFuncTyBundle<'ctx> {
    fn new(ctx: &'ctx Context<'ctx>, rm: &mut ReprManager<'ctx>) -> Self {
        let val_type = rm.get_type::<Val>().get_ty().unwrap().into_struct_type();
        let val_type_ptr = val_type.ptr_type(inkwell::AddressSpace::default());

        let opaque_type = rm
            .get_type::<c_void>()
            .get_ty()
            .unwrap()
            .into_struct_type();

        let rt_type = rm.get_type::<Runtime>().get_ty().unwrap();
        let closure_type = ctx.llvm_ctx.void_type().fn_type(
            &[
                val_type_ptr.into(),
                rt_type.ptr_type(inkwell::AddressSpace::default()).into(),
                val_type_ptr.into(),
            ],
            false,
        );

        let landingpad_type = ctx.llvm_ctx.opaque_struct_type("landingpad");
        landingpad_type.set_body(
            &[
                opaque_type
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(),
                ctx.llvm_ctx.i32_type().into(),
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
            fn new(ctx: &'ctx Context<'ctx>, rm: &mut ReprManager<'ctx>) -> $name<'ctx> {
                assert_eq!(size_of::<caer_runtime::val::Val>(), 24);

                let tyb = RtFuncTyBundle::new(ctx, rm);
                let llvm_ctx = &ctx.llvm_ctx;

                $name {
                    $(
                        $func: ctx.module.add_function(stringify!($func),
                            rt_funcs!(@genty llvm_ctx $retspec tyb $ret $ret).fn_type(&[
                                $(
                                    rt_funcs!(@genty llvm_ctx $argspec tyb $arg $arg).into(),
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
        $e.ptr_type(inkwell::AddressSpace::default())
    );

    ( @genty @ptrify gptr , $e:expr) => (
        $e.ptr_type(inkwell::AddressSpace::default())
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
