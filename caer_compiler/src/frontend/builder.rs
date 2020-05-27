use crate::ir::env::Environment;
use dreammaker::objtree;
use caer_runtime::string_table::StringId;
use std::borrow::Cow;
use super::proc_builder::ProcBuilder;

pub struct Builder<'a> {
    pub tree: &'a objtree::ObjectTree,
    pub env: Environment,
}
impl<'a> Builder<'a> {
    pub fn build(tree: &'a objtree::ObjectTree) -> Environment {
        let mut builder = Self {
            tree: tree,
            env: Environment::new(tree),
        };

        builder.build_procs();

        builder.env
    }

    fn build_procs(&mut self) {
        self.env.procs = self.tree.root().get().procs.iter().filter(|(name, procs)| {
            match &procs.main_value().code {
                objtree::Code::Present(_) => true,
                objtree::Code::Invalid(err) => panic!("oh no dm error {:?}", err),
                objtree::Code::Builtin => false,
                objtree::Code::Disabled => panic!("woop woop procs disabled"),
            }
            //procs.value[0].body.len() != 0 // TODO REMOVE THIS
        }).map(|(name, procs)| {
            (self.add_string(name), ProcBuilder::build(self, &name, &procs.value[0]))
        }).collect();
    }

    pub fn add_string<'s>(&mut self, s: impl Into<Cow<'s, str>>) -> StringId {
        self.env.string_table.put(s)
    }
}
