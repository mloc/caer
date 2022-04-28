use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use pinion::layout::{Enum, Layout, StructLayout};
use pinion::layout_ctx::{LayoutCtx, LayoutId};
use pinion::types::Primitive;
use pinion::{PinionEnum, PinionStruct};

use crate::context::Context;

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
    pub ty: StructType<'ctx>,
}

impl<'ctx> EnumRepr<'ctx> {
    pub fn new(layout: Enum, ty: StructType<'ctx>) -> Self {
        Self { layout, ty }
    }
}

// TODO: reconsider name/loc, rethink structure
#[derive(Debug, Default)]
pub struct ReprManager<'ctx> {
    layout_ctx: LayoutCtx,
    basic_types: HashMap<LayoutId, BasicTypeEnum<'ctx>>,

    structs: HashMap<LayoutId, Rc<StructRepr<'ctx>>>,
}

impl<'ctx> ReprManager<'ctx> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_struct<T: PinionStruct>(
        &mut self, ctx: &'ctx inkwell::context::Context,
    ) -> Rc<StructRepr<'ctx>> {
        let id = self.layout_ctx.populate::<T>();
        self.get_struct_from_id(id, ctx)
    }

    fn get_struct_from_id(
        &mut self, id: LayoutId, ctx: &'ctx inkwell::context::Context,
    ) -> Rc<StructRepr<'ctx>> {
        let (layout, bty) = self.build_layout(id, ctx);

        let struct_layout = match layout.borrow() {
            Layout::Struct(sl) => sl.clone(),
            _ => panic!("id didn't produce a struct layout"),
        };

        Rc::new(StructRepr::new(struct_layout, bty.into_struct_type()))
    }

    pub fn get_enum<T: PinionEnum>(
        &mut self, ctx: &'ctx inkwell::context::Context,
    ) -> Rc<EnumRepr<'ctx>> {
        let id = self.layout_ctx.populate::<T>();
        self.get_enum_from_id(id, ctx)
    }

    fn get_enum_from_id(
        &mut self, id: LayoutId, ctx: &'ctx inkwell::context::Context,
    ) -> Rc<EnumRepr<'ctx>> {
        let (layout, bty) = self.build_layout(id, ctx);

        let enum_layout = match layout.borrow() {
            Layout::Enum(el) => el.clone(),
            _ => panic!("id didn't produce an enum layout"),
        };

        Rc::new(EnumRepr::new(enum_layout, bty.into_struct_type()))
    }

    fn build_layout(
        &mut self, id: LayoutId, ctx: &'ctx inkwell::context::Context,
    ) -> (Rc<Layout>, BasicTypeEnum<'ctx>) {
        let layout = self.layout_ctx.get(id).unwrap();
        if let Some(bty) = self.basic_types.get(&id) {
            (layout, *bty)
        } else {
            let bty = self.build_bty(layout.clone(), ctx);
            self.basic_types.insert(id, bty);
            (layout, bty)
        }
    }

    fn build_bty(
        &mut self, layout: Rc<Layout>, ctx: &'ctx inkwell::context::Context,
    ) -> BasicTypeEnum<'ctx> {
        match layout.borrow() {
            Layout::Struct(sl) => {
                let fields: Vec<_> = sl
                    .fields
                    .iter()
                    .map(|id| self.build_layout(*id, ctx).1)
                    .collect();
                ctx.named_struct_type(&fields, false, sl.name.unwrap_or_default())
                    .into()
            },
            Layout::Primitive(prim) => match prim {
                Primitive::Bool => ctx.bool_type().into(),
                Primitive::Int8 => ctx.i8_type().into(),
                Primitive::Int16 => ctx.i16_type().into(),
                Primitive::Int32 => ctx.i32_type().into(),
                Primitive::Int64 => ctx.i64_type().into(),
                Primitive::Float16 => ctx.f16_type().into(),
                Primitive::Float32 => ctx.f32_type().into(),
                Primitive::Float64 => ctx.f64_type().into(),
            },
            Layout::Pointer(ptr) => {
                let (_, pointee) = self.build_layout(ptr.element, ctx);
                pointee.ptr_type(inkwell::AddressSpace::Generic).into()
            },
            Layout::Enum(enum_layout) => {
                assert!(enum_layout.alignment >= enum_layout.disc_width);
                assert!(enum_layout.alignment <= 8);
                assert_eq!(enum_layout.size % enum_layout.alignment, 0);

                // TODO: ick, there's a lot of roundtrips between reprs here
                let disc_part = match enum_layout.disc_width {
                    1 => ctx.i8_type().into(),
                    2 => ctx.i16_type().into(),
                    4 => ctx.i32_type().into(),
                    8 => ctx.i64_type().into(),
                    w => panic!("impossible disc_width: {}", w),
                };

                if enum_layout.field_layouts.is_empty() {
                    // Special case: fieldless enums consist of just the disc
                    assert_eq!(enum_layout.alignment, enum_layout.disc_width);
                    ctx.named_struct_type(&[disc_part], false, enum_layout.name.unwrap_or_default())
                        .into()
                } else {
                    let disc_padding_length = enum_layout.alignment - enum_layout.disc_width;
                    let val_length = enum_layout.size - enum_layout.alignment;

                    let disc_padding_part =
                        ctx.i8_type().array_type(disc_padding_length as _).into();

                    let align_part = match enum_layout.alignment {
                        1 => ctx.i8_type(),
                        2 => ctx.i16_type(),
                        4 => ctx.i32_type(),
                        8 => ctx.i64_type(),
                        w => panic!("unsupported alignment: {}", w),
                    };
                    let val_part = align_part
                        .array_type((val_length / enum_layout.alignment) as _)
                        .into();

                    ctx.named_struct_type(
                        &[disc_part, disc_padding_part, val_part],
                        false,
                        enum_layout.name.unwrap_or_default(),
                    )
                    .into()
                }
            },
            Layout::FuncPtr => {
                let opaque_base = ctx.opaque_struct_type("");
                opaque_base.ptr_type(inkwell::AddressSpace::Generic).into()
            },
            Layout::OpaqueStruct(opaque_layout) => {
                let name = opaque_layout.name.unwrap_or_default();
                if let Some(size) = opaque_layout.size {
                    ctx.named_struct_type(&[ctx.i8_type().array_type(size).into()], false, name)
                        .into()
                } else {
                    ctx.opaque_struct_type(name).into()
                }
            },
            Layout::Unsized => panic!("can't represent unsized type"),
        }
    }
}
