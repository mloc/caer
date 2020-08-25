use caer_types::layout::GC_STACKMAP_ID;
use llvm_stackmaps::{LittleEndian, Location, LocationPointer, Parser};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct GcStackmap {
    pub addr_to_record: HashMap<u64, GcRecord>,
    pub func_has_maps: HashSet<u64>,
}

#[derive(Debug, Clone)]
pub struct GcRecord {
    pub func_base: u64,
    pub locations: Vec<Location>,
}

impl GcStackmap {
    // TODO: ERRH
    pub fn parse(sect: &[u8]) -> Self {
        if sect.is_empty() {
            return Self {
                addr_to_record: Default::default(),
                func_has_maps: Default::default(),
            };
        }

        let stackmap = Parser::<LittleEndian>::parse(sect).unwrap();
        let mut addr_to_record = HashMap::new();
        let mut func_has_maps = HashSet::new();

        for function in stackmap.functions {
            let func_base = function.addr;
            func_has_maps.insert(func_base);
            for record in function.records {
                if record.patch_point_id != GC_STACKMAP_ID {
                    continue;
                }

                // can't get gc-live working properly, so GC pointers are passed in deopt
                // TODO: sort this out

                assert!(record.locations.len() >= 3);
                assert_eq!(record.locations[0].pointer, LocationPointer::Constant(0));
                assert_eq!(record.locations[1].pointer, LocationPointer::Constant(0));
                let deopts;
                if let LocationPointer::Constant(deopt_n) = record.locations[2].pointer {
                    deopts = deopt_n;
                } else {
                    panic!();
                }

                let mut locations = record.locations;
                locations.drain(0..3);

                assert_eq!(locations.len(), deopts as usize);

                let addr = func_base + (record.instruction_offset as u64);
                addr_to_record.insert(
                    addr,
                    GcRecord {
                        func_base,
                        locations,
                    },
                );
            }
        }

        Self {
            addr_to_record,
            func_has_maps,
        }
    }
}
