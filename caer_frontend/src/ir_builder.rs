use caer_ir::cfg;
use caer_ir::module::Module;
use caer_types::id::{FuncId, StringId};
use index_vec::IndexVec;

use super::proc_builder::ProcBuilder;
use crate::objtree_wrapper::ObjtreeWrapper;
use crate::proc_builder::ProcBody;

pub struct IrBuilder<'a> {
    pub ir: &'a mut Module,
    funcs: IndexVec<FuncId, (ProcBody, Option<FuncId>)>,
    pub objtree: &'a ObjtreeWrapper<'a>,
}

impl<'a> IrBuilder<'a> {
    pub fn new(
        ir: &'a mut Module, funcs: IndexVec<FuncId, (ProcBody, Option<FuncId>)>,
        objtree: &'a ObjtreeWrapper<'a>,
    ) -> Self {
        Self { ir, funcs, objtree }
    }

    // TODO: ERRH(fe)
    pub fn build(self) {
        for (id, (body, parent)) in self.funcs.iter_enumerated() {
            let mut func = self.ir.new_func();
            assert_eq!(func.id, id);
            func.parent = *parent;

            ProcBuilder::build(func, self.ir, self.objtree, body);
        }

        // wow ewww
        for proc_id_n in 0..self.ir.funcs.len() {
            let proc_id = FuncId::new(proc_id_n);
            self.ir.funcs[&proc_id].dot(&format!("proc_{}", proc_id.index()));
            caer_ir::analysis::FuncAnalysis::analyse_proc(self.ir, proc_id);
            self.ir.funcs[&proc_id].dot(&format!("opt_proc_{}", proc_id.index()));
        }
    }

    pub fn add_string(&mut self, s: impl Into<String> + AsRef<str>) -> StringId {
        self.ir.string_table.put(s)
    }
}
