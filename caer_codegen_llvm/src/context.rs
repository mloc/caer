use std::any::TypeId;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::c_void;
use std::mem::size_of;
use std::rc::Rc;

use caer_runtime::arg_pack::ProcPack;
use caer_runtime::runtime::Runtime;
use caer_runtime::val::Val;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, PointerType};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use pinion::layout::{self, Func};
use pinion::{
    ConstItem, PinionCallBundle, PinionConstWrap, PinionData, PinionEnum, PinionFuncInstance,
    PinionModule, PinionStruct, PinionTaggedUnion,
};

use crate::emit_type::EmitType;
use crate::prog;
use crate::repr::{EnumRepr, ReprManager, StructRepr, TaggedUnionRepr};
use crate::value::{BrandedValue, MaybeRet};

pub type ExRuntime = caer_runtime::export::Runtime;
pub type ExFunc = <ExRuntime as PinionModule>::Funcs;
pub type ExMod = <ExRuntime as PinionModule>::TFuncs;

/// Not really needed currently while using explicit forms- set to same as normal
//pub const GC_ADDRESS_SPACE: inkwell::AddressSpace = inkwell::AddressSpace::default();

type TestType<'a> = Box<inkwell::module::Module<'a>>;

// Verify that `TestType` is covariant.
fn test_covariance<'short, 'long: 'short>() {
    let long: TestType<'long> = make();
    let short: TestType<'short> = long;
}
fn make<T>() -> T {
    unimplemented!()
}

#[derive(Debug)]
pub struct Context<'ctx> {
    pub llvm_ctx: &'ctx inkwell::context::Context,
    // Module contains a RefCell, which makes it invariant over 'ctx. Using an Rc gets around
    // needing another lifetime. (somehow??? builder also needs one)
    pub builder: inkwell::builder::Builder<'ctx>,
    pub module: inkwell::module::Module<'ctx>,

    pub target_data: TargetData,

    pub r: ReprManager<'ctx>,
}

fn get_target_data(module: &inkwell::module::Module<'_>) -> TargetData {
    let data_layout = module.get_data_layout();
    TargetData::create(data_layout.as_str().to_str().unwrap())
}

impl<'ctx> Context<'ctx> {
    pub fn new(
        llctx: &'ctx inkwell::context::Context, llmod: inkwell::module::Module<'ctx>,
        llbuild: inkwell::builder::Builder<'ctx>,
    ) -> Self {
        let mut repr_manager = ReprManager::new();

        // is this.. OK?
        let target_data = get_target_data(&llmod);

        Self {
            builder: llbuild,
            module: llmod,
            llvm_ctx: llctx,
            target_data,
            r: repr_manager,
        }
    }

    pub unsafe fn const_gep(&self, ptr: PointerValue<'ctx>, indexes: &[u64]) -> PointerValue<'ctx> {
        let gep_indexes: Vec<_> = indexes
            .iter()
            .map(|i| self.llvm_ctx.i32_type().const_int(*i, false))
            .collect();
        self.builder
            .build_in_bounds_gep(ptr, &gep_indexes, "gepiv_ptr")
            .unwrap()
    }

    // TODO: most callers of this should be using catch machinery
    pub fn build_call<const N: usize, F: PinionFuncInstance<ExRuntime>, R>(
        &self, cb: PinionCallBundle<N, ExRuntime, F, R, BasicValueEnum<'ctx>>,
    ) -> MaybeRet<'ctx, R> {
        /*let func_typeid = TypeId::of::<F>();
        let func_val = self.newfuncs[&func_typeid].1;
        // TODO: use BMVE in call bundles
        let args_bmve: Vec<_> = cb.args.iter().copied().map(Into::into).collect();
        let ret = self
            .builder
            .build_call(func_val, &args_bmve, "")
            .try_as_basic_value()
            .left();
        MaybeRet::create(ret)*/
        todo!()
    }

    pub fn get_intrinsic(&self, intrinsic: prog::Intrinsic) -> FunctionValue<'ctx> {
        // TODO: cache?
        let ty_f32 = self.llvm_ctx.f32_type().into();

        let (name, tys): (&str, Vec<BasicTypeEnum>) = match intrinsic {
            prog::Intrinsic::FPow => ("llvm.pow", vec![ty_f32]),
            prog::Intrinsic::Trap => ("llvm.trap", vec![]),
            //Intrinsic::LifetimeStart => ("llvm.lifetime.start", vec![]),
            //Intrinsic::LifetimeEnd => ("llvm.lifetime.end", vec![]),
            //Intrinsic::InvariantStart => ("llvm.invariant.start", vec![]),
        };

        self.get_intrinsic_raw(name, &tys).unwrap()
    }

    pub fn get_intrinsic_raw(
        &self, name: &str, param_types: &[BasicTypeEnum],
    ) -> Option<FunctionValue<'ctx>> {
        let intrinsic = inkwell::intrinsics::Intrinsic::find(name)?;
        intrinsic.get_declaration(&self.module, param_types)
    }

    pub(crate) fn copy_val(&self, src: PointerValue<'ctx>, dest: PointerValue<'ctx>) {
        assert_eq!(src.get_type(), dest.get_type());

        let memcpy_intrinsic = self
            .get_intrinsic_raw(
                "llvm.memcpy",
                &[
                    src.get_type().into(),
                    dest.get_type().into(),
                    self.llvm_ctx.i64_type().into(),
                ],
            )
            .unwrap();

        self.builder.build_call(
            memcpy_intrinsic,
            &[
                dest.into(),
                src.into(),
                self.llvm_ctx.i64_type().const_int(24, false).into(),
                self.llvm_ctx.bool_type().const_zero().into(),
            ],
            "",
        );
    }

    // TODO: there's a lot of monomorph in this chain :( there's also a lot of borrow_mut thrashing
    // there's def. a better way to arrange this with some other type and a wrapper trait
    /*pub fn const_wrap<T: PinionConstWrap>(&self, val: &T) -> BrandedValue<'ctx, T> {
        let const_wrap = self.repr_manager.borrow_mut().const_wrap(val);
        todo!()
    }*/

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

/*impl<'a, 'ctx> pinion::interface::Context for Context<'a, 'ctx> {
    type BasicType = ();
    type Funcs = &'ctx Self;
    type FunctionType = ();
    type Repr = ();
    type ReprManager;

    fn get_funcs(&self) -> Self::Funcs {
        todo!()
    }

    fn get_repr_manager(&self) -> Self::ReprManager {
        todo!()
    }
}*/
