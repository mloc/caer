use std::any::TypeId;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, IntType, StructType};
use pinion::layout::{Enum, Func, Layout, StructLayout, TaggedUnion, Union};
use pinion::layout_ctx::{LayoutCtx, LayoutId};
use pinion::types::Primitive;
use pinion::{
    ConstItem, PinionConstWrap, PinionData, PinionEnum, PinionModule, PinionStruct,
    PinionTaggedUnion,
};

use crate::emit_type::EmitType;

#[derive(Debug)]
pub struct StructRepr<'ctx> {
    pub layout: StructLayout,
    pub ty: StructType<'ctx>,
}

impl<'ctx> StructRepr<'ctx> {
    pub fn new(layout: StructLayout, ty: StructType<'ctx>) -> Self {
        Self { layout, ty }
    }
}

#[derive(Debug)]
pub struct EnumRepr<'ctx> {
    pub layout: Enum,
    pub ty: IntType<'ctx>,
}

impl<'ctx> EnumRepr<'ctx> {
    pub fn new(layout: Enum, ty: IntType<'ctx>) -> Self {
        Self { layout, ty }
    }
}

#[derive(Debug)]
pub struct TaggedUnionRepr<'ctx> {
    pub layout: TaggedUnion,
    pub tag_repr: Rc<EnumRepr<'ctx>>,
    pub ty: StructType<'ctx>,
}

impl<'ctx> TaggedUnionRepr<'ctx> {
    pub fn new(layout: TaggedUnion, tag_repr: Rc<EnumRepr<'ctx>>, ty: StructType<'ctx>) -> Self {
        Self {
            layout,
            tag_repr,
            ty,
        }
    }
}

// TODO: reconsider name/loc, rethink structure. there's a bunch of icky repetition
#[derive(Debug)]
pub struct ReprManager<'ctx> {
    ll_ctx: &'ctx inkwell::context::Context,
    layout_ctx: RefCell<LayoutCtx>,
    basic_types: RefCell<HashMap<LayoutId, Option<BasicTypeEnum<'ctx>>>>,
    structs: RefCell<HashMap<LayoutId, Rc<StructRepr<'ctx>>>>,
    enums: RefCell<HashMap<LayoutId, Rc<EnumRepr<'ctx>>>>,
    tagged_unions: RefCell<HashMap<LayoutId, Rc<TaggedUnionRepr<'ctx>>>>,
}

/*impl<'ctx> pinion::ReprManager for ReprManager<'ctx> {
    type Funcs = ();
    type Repr = ();

    fn build_repr<T: PinionData>(&mut self, ctx: Self::Funcs) -> Self::Repr {
        todo!()
    }
}*/

impl<'ctx> ReprManager<'ctx> {
    pub fn new(ll_ctx: &'ctx inkwell::context::Context) -> Self {
        Self {
            ll_ctx,
            layout_ctx: Default::default(),
            basic_types: Default::default(),
            structs: Default::default(),
            enums: Default::default(),
            tagged_unions: Default::default(),
        }
    }

    // TODO: might be cleaner to move this out
    pub fn const_wrap<T: PinionConstWrap>(&self, val: &T) -> ConstItem {
        val.const_wrap(&mut self.layout_ctx.borrow_mut())
    }

    pub fn get_type<T: PinionData>(&self) -> EmitType<'ctx> {
        let id = self.layout_ctx.borrow_mut().populate::<T>();
        let (_, ty) = self.build_layout(id);
        EmitType::new(id, ty)
    }

    pub fn get_llvm_type<T: PinionData>(&self) -> BasicTypeEnum<'ctx> {
        self.get_type::<T>().get_ty().unwrap()
    }

    pub fn get_all_funcs<T: PinionModule>(
        &self,
    ) -> Vec<(TypeId, (Func, FunctionType<'ctx>), T::Funcs)> {
        T::get_funcs(&mut self.layout_ctx.borrow_mut())
            .into_iter()
            .map(|(id, typeid, layout)| {
                let func = self.build_func(&layout);
                (typeid, (layout, func), id)
            })
            .collect()
    }

    fn build_func(&self, func: &Func) -> FunctionType<'ctx> {
        println!("building func {:#?}", func);
        let params: Vec<_> = func
            .param_tys
            .iter()
            .map(|id| self.build_layout(*id).1.expect("param must be sized"))
            .map(Into::into)
            .collect();

        match func.return_ty {
            Some(id) => {
                let (_, ret_ty) = self.build_layout(id);
                ret_ty
                    .expect("return type must be sized")
                    .fn_type(&params, false)
            },
            None => self.ll_ctx.void_type().fn_type(&params, false),
        }
    }

    pub fn get_struct<T: PinionStruct>(&self) -> Rc<StructRepr<'ctx>> {
        let id = self.layout_ctx.borrow_mut().populate::<T>();
        self.get_struct_from_id(id)
    }

    fn get_struct_from_id(&self, id: LayoutId) -> Rc<StructRepr<'ctx>> {
        if let Some(repr) = self.structs.borrow().get(&id) {
            return repr.clone();
        }
        let (layout, bty) = self.build_layout(id);

        let struct_layout = match layout.borrow() {
            Layout::Struct(sl) => sl.clone(),
            _ => panic!("id didn't produce a struct layout"),
        };

        let repr = Rc::new(StructRepr::new(
            struct_layout,
            bty.unwrap().into_struct_type(),
        ));
        self.structs.borrow_mut().insert(id, repr.clone());
        repr
    }

    pub fn get_enum<T: PinionEnum>(&self) -> Rc<EnumRepr<'ctx>> {
        let id = self.layout_ctx.borrow_mut().populate::<T>();
        self.get_enum_from_id(id)
    }

    fn get_enum_from_id(&self, id: LayoutId) -> Rc<EnumRepr<'ctx>> {
        if let Some(repr) = self.enums.borrow().get(&id) {
            return repr.clone();
        }
        let (layout, bty) = self.build_layout(id);
        let enum_layout = match layout.borrow() {
            Layout::Enum(el) => el.clone(),
            _ => panic!("id didn't produce an enum layout"),
        };

        let repr = Rc::new(EnumRepr::new(enum_layout, bty.unwrap().into_int_type()));
        self.enums.borrow_mut().insert(id, repr.clone());
        repr
    }

    pub fn get_tagged_union<T: PinionTaggedUnion>(&self) -> Rc<TaggedUnionRepr<'ctx>> {
        let id = self.layout_ctx.borrow_mut().populate::<T>();
        self.get_tagged_union_from_id(id)
    }

    fn get_tagged_union_from_id(&self, id: LayoutId) -> Rc<TaggedUnionRepr<'ctx>> {
        if let Some(repr) = self.tagged_unions.borrow().get(&id) {
            return repr.clone();
        }
        let (layout, bty) = self.build_layout(id);
        let tagged_union_layout = match layout.borrow() {
            Layout::TaggedUnion(tul) => tul.clone(),
            _ => panic!("id didn't produce a tagged union layout"),
        };

        let tag_repr = self.get_enum_from_id(tagged_union_layout.tag_layout);
        let repr = Rc::new(TaggedUnionRepr::new(
            tagged_union_layout,
            tag_repr,
            bty.unwrap().into_struct_type(),
        ));
        self.tagged_unions.borrow_mut().insert(id, repr.clone());
        repr
    }

    fn build_layout(&self, id: LayoutId) -> (Rc<Layout>, Option<BasicTypeEnum<'ctx>>) {
        let layout = self.layout_ctx.borrow().get(id).unwrap();
        if let Some(bty) = self.basic_types.borrow().get(&id) {
            (layout, *bty)
        } else {
            let bty = self.build_bty_stage1(&layout);
            self.basic_types.borrow_mut().insert(id, bty);
            if let Some(bty) = bty {
                self.build_bty_stage2(&layout, bty);
            }
            (layout, bty)
        }
    }

    fn build_bty_stage1(&self, layout: &Layout) -> Option<BasicTypeEnum<'ctx>> {
        match layout {
            Layout::Struct(sl) => Some(
                self.ll_ctx
                    .opaque_struct_type(sl.name.unwrap_or_default())
                    .into(),
            ),
            Layout::Primitive(prim) => Some(match prim {
                Primitive::Bool => self.ll_ctx.bool_type().into(),
                Primitive::Int8 => self.ll_ctx.i8_type().into(),
                Primitive::Int16 => self.ll_ctx.i16_type().into(),
                Primitive::Int32 => self.ll_ctx.i32_type().into(),
                Primitive::Int64 => self.ll_ctx.i64_type().into(),
                Primitive::Float16 => self.ll_ctx.f16_type().into(),
                Primitive::Float32 => self.ll_ctx.f32_type().into(),
                Primitive::Float64 => self.ll_ctx.f64_type().into(),
            }),
            Layout::Pointer(ptr) => {
                let (_, pointee) = self.build_layout(ptr.element);
                Some(
                    pointee
                        .expect("pointee must be sized")
                        .ptr_type(inkwell::AddressSpace::default())
                        .into(),
                )
            },
            Layout::Union(ul) => Some(
                self.ll_ctx
                    .opaque_struct_type(ul.name.unwrap_or_default())
                    .into(),
            ),
            Layout::Enum(el) => Some(self.build_layout(el.disc_layout).1.unwrap()),
            Layout::TaggedUnion(tul) => Some(
                self.ll_ctx
                    .opaque_struct_type(tul.name.unwrap_or_default())
                    .into(),
            ),
            Layout::FuncPtr(func) => {
                let func_ty = self.build_func(func);
                Some(func_ty.ptr_type(inkwell::AddressSpace::default()).into())
            },
            Layout::OpaqueStruct(opaque_layout) => {
                let name = opaque_layout.name.unwrap_or_default();
                let ty = self.ll_ctx.opaque_struct_type(name);
                if let Some(size) = opaque_layout.size {
                    ty.set_body(&[self.ll_ctx.i8_type().array_type(size).into()], false);
                }
                Some(ty.into())
            },
            Layout::Unit => None,
            Layout::Unsized => None,
        }
    }

    fn build_bty_stage2(&self, layout: &Layout, to_fill: BasicTypeEnum<'ctx>) {
        match layout {
            Layout::Struct(struct_layout) => {
                self.build_struct(struct_layout, to_fill.into_struct_type());
            },
            Layout::Union(union_layout) => {
                self.build_union(union_layout, to_fill.into_struct_type());
            },
            Layout::TaggedUnion(tagged_union_layout) => {
                self.build_tagged_union(tagged_union_layout, to_fill.into_struct_type());
            },
            Layout::Primitive(_)
            | Layout::Pointer(_)
            | Layout::FuncPtr(_)
            | Layout::OpaqueStruct(_)
            | Layout::Enum(_)
            | Layout::Unit
            | Layout::Unsized => {},
        }
    }

    fn build_struct(&self, layout: &StructLayout, to_fill: StructType<'ctx>) {
        let fields: Vec<_> = layout
            .fields
            .iter()
            .filter_map(|id| self.build_layout(*id).1)
            .collect();
        to_fill.set_body(&fields, false);
    }

    fn build_union(&self, layout: &Union, to_fill: StructType<'ctx>) {
        assert_eq!(layout.size % layout.alignment, 0);
        let align_unit_ty = self
            .ll_ctx
            .custom_width_int_type((layout.alignment * 8) as _);
        let body = align_unit_ty.array_type((layout.size / layout.alignment) as _);
        to_fill.set_body(&[body.into()], false);
    }

    fn build_tagged_union(&self, layout: &TaggedUnion, to_fill: StructType<'ctx>) {
        let tag_part = self.build_layout(layout.tag_layout).1.unwrap();
        let union_part = self.build_layout(layout.union_layout).1.unwrap();
        to_fill.set_body(&[tag_part.into(), union_part.into()], false);
    }
}
