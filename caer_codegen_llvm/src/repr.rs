use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::FunctionValue;
use pinion::layout::{Enum, Func, Layout, StructLayout};
use pinion::layout_ctx::{LayoutCtx, LayoutId};
use pinion::types::Primitive;
use pinion::{PinionData, PinionEnum, PinionModule, PinionStruct};

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
    basic_types: HashMap<LayoutId, Option<BasicTypeEnum<'ctx>>>,

    structs: HashMap<LayoutId, Rc<StructRepr<'ctx>>>,
}

impl<'ctx> ReprManager<'ctx> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_type<T: PinionData>(
        &mut self, ctx: &'ctx inkwell::context::Context,
    ) -> Option<BasicTypeEnum<'ctx>> {
        let id = self.layout_ctx.populate::<T>();
        let (_, ty) = self.build_layout(id, ctx);
        ty
    }

    pub fn get_all_funcs<T: PinionModule>(
        &mut self, ctx: &'ctx inkwell::context::Context,
    ) -> Vec<(T::Funcs, (Func, FunctionType<'ctx>))> {
        T::get_funcs(&mut self.layout_ctx)
            .into_iter()
            .map(|(id, layout)| {
                let func = self.build_func(&layout, ctx);
                (id, (layout, func))
            })
            .collect()
    }

    fn build_func(
        &mut self, func: &Func, ctx: &'ctx inkwell::context::Context,
    ) -> FunctionType<'ctx> {
        println!("building func {:#?}", func);
        let params: Vec<_> = func
            .param_tys
            .iter()
            .map(|id| self.build_layout(*id, ctx).1.expect("param must be sized"))
            .collect();

        match func.return_ty {
            Some(id) => {
                let (_, ret_ty) = self.build_layout(id, ctx);
                ret_ty
                    .expect("return type must be sized")
                    .fn_type(&params, false)
            },
            None => ctx.void_type().fn_type(&params, false),
        }
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

        Rc::new(StructRepr::new(
            struct_layout,
            bty.unwrap().into_struct_type(),
        ))
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

        Rc::new(EnumRepr::new(enum_layout, bty.unwrap().into_struct_type()))
    }

    fn build_layout(
        &mut self, id: LayoutId, ctx: &'ctx inkwell::context::Context,
    ) -> (Rc<Layout>, Option<BasicTypeEnum<'ctx>>) {
        let layout = self.layout_ctx.get(id).unwrap();
        if let Some(bty) = self.basic_types.get(&id) {
            (layout, *bty)
        } else {
            let bty = self.build_bty_stage1(&layout, ctx);
            self.basic_types.insert(id, bty);
            if let Some(bty) = bty {
                self.build_bty_stage2(&layout, bty, ctx);
            }
            (layout, bty)
        }
    }

    fn build_bty_stage1(
        &mut self, layout: &Layout, ctx: &'ctx inkwell::context::Context,
    ) -> Option<BasicTypeEnum<'ctx>> {
        match layout {
            Layout::Struct(sl) => Some(ctx.opaque_struct_type(sl.name.unwrap_or_default()).into()),
            Layout::Primitive(prim) => Some(match prim {
                Primitive::Bool => ctx.bool_type().into(),
                Primitive::Int8 => ctx.i8_type().into(),
                Primitive::Int16 => ctx.i16_type().into(),
                Primitive::Int32 => ctx.i32_type().into(),
                Primitive::Int64 => ctx.i64_type().into(),
                Primitive::Float16 => ctx.f16_type().into(),
                Primitive::Float32 => ctx.f32_type().into(),
                Primitive::Float64 => ctx.f64_type().into(),
            }),
            Layout::Pointer(ptr) => {
                let (_, pointee) = self.build_layout(ptr.element, ctx);
                Some(
                    pointee
                        .expect("pointee must be sized")
                        .ptr_type(inkwell::AddressSpace::Generic)
                        .into(),
                )
            },
            Layout::Enum(enum_layout) => Some(
                ctx.opaque_struct_type(enum_layout.name.unwrap_or_default())
                    .into(),
            ),
            Layout::FuncPtr(func) => {
                let func_ty = self.build_func(func, ctx);
                Some(func_ty.ptr_type(inkwell::AddressSpace::Generic).into())
            },
            Layout::OpaqueStruct(opaque_layout) => {
                let name = opaque_layout.name.unwrap_or_default();
                if let Some(size) = opaque_layout.size {
                    Some(
                        ctx.named_struct_type(
                            &[ctx.i8_type().array_type(size).into()],
                            false,
                            name,
                        )
                        .into(),
                    )
                } else {
                    Some(ctx.opaque_struct_type(name).into())
                }
            },
            Layout::Unsized => None,
        }
    }

    fn build_bty_stage2(
        &mut self, layout: &Layout, to_fill: BasicTypeEnum<'ctx>,
        ctx: &'ctx inkwell::context::Context,
    ) {
        match layout {
            Layout::Struct(struct_layout) => {
                self.build_struct(struct_layout, to_fill.into_struct_type(), ctx);
            },
            Layout::Enum(enum_layout) => {
                self.build_enum(enum_layout, to_fill.into_struct_type(), ctx);
            },
            Layout::Primitive(_)
            | Layout::Pointer(_)
            | Layout::FuncPtr(_)
            | Layout::OpaqueStruct(_)
            | Layout::Unsized => {},
        }
    }

    fn build_struct(
        &mut self, layout: &StructLayout, to_fill: StructType<'ctx>,
        ctx: &'ctx inkwell::context::Context,
    ) {
        let fields: Vec<_> = layout
            .fields
            .iter()
            .filter_map(|id| self.build_layout(*id, ctx).1)
            .collect();
        to_fill.set_body(&fields, false);
    }

    fn build_enum(
        &mut self, layout: &Enum, to_fill: StructType<'ctx>, ctx: &'ctx inkwell::context::Context,
    ) {
        assert!(layout.alignment >= layout.disc_width);
        assert!(layout.alignment <= 8);
        assert_eq!(layout.size % layout.alignment, 0);

        // TODO: ick, there's a lot of roundtrips between reprs here
        let disc_part = match layout.disc_width {
            1 => ctx.i8_type().into(),
            2 => ctx.i16_type().into(),
            4 => ctx.i32_type().into(),
            8 => ctx.i64_type().into(),
            w => panic!("impossible disc_width: {}", w),
        };

        if layout.field_layouts.is_empty() {
            // Special case: fieldless enums consist of just the disc
            assert_eq!(layout.alignment, layout.disc_width);
            to_fill.set_body(&[disc_part], false);
        } else {
            let disc_padding_length = layout.alignment - layout.disc_width;
            let val_length = layout.size - layout.alignment;

            let disc_padding_part = ctx.i8_type().array_type(disc_padding_length as _).into();

            let align_part = match layout.alignment {
                1 => ctx.i8_type(),
                2 => ctx.i16_type(),
                4 => ctx.i32_type(),
                8 => ctx.i64_type(),
                w => panic!("unsupported alignment: {}", w),
            };
            let val_part = align_part
                .array_type((val_length / layout.alignment) as _)
                .into();

            to_fill.set_body(&[disc_part, disc_padding_part, val_part], false);
        }
    }
}
