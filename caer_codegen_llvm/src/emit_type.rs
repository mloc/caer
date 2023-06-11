use inkwell::types::BasicTypeEnum;
use pinion::layout_ctx::LayoutId;

#[derive(Debug, Clone)]
pub struct EmitType<'ctx> {
    layout_id: LayoutId,
    // None for unsized types
    llvm_type: Option<BasicTypeEnum<'ctx>>,
}

impl<'ctx> EmitType<'ctx> {
    pub fn new(layout_id: LayoutId, llvm_type: Option<BasicTypeEnum<'ctx>>) -> Self {
        Self {
            layout_id,
            llvm_type,
        }
    }

    pub fn get_id(&self) -> LayoutId {
        self.layout_id
    }

    pub fn get_ty(&self) -> Option<BasicTypeEnum<'ctx>> {
        self.llvm_type
    }
}
