use std::collections::{HashMap, HashSet};

use caer_types::id::{InstanceTypeId, TypeId};
use caer_types::layout::GC_STACKMAP_ID;
pub use llvm_stackmaps::LocationPointer;
use llvm_stackmaps::{Function, LittleEndian, Location, Parser, Record};

#[derive(Debug, Clone)]
pub struct GcStackmap {
    pub addr_to_record: HashMap<u64, GcRecord>,
    pub func_has_maps: HashSet<u64>,
}

#[derive(Debug, Clone)]
pub struct GcRecord {
    pub func_base: u64,
    pub root_locations: Vec<TaggedLocation>,
    // Stack Locations are always val-type, no need for tag
    pub stack_locations: Vec<Location>,
}

#[derive(Debug, Clone, Copy)]
pub struct TaggedLocation {
    pub tag: InstanceTypeId,
    pub location: Location,
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
            func_has_maps.insert(function.addr);
            for record in function.records {
                if record.patch_point_id != GC_STACKMAP_ID {
                    continue;
                }
            }
        }

        Self {
            addr_to_record,
            func_has_maps,
        }
    }

    /// Record format:
    /// - calling convention (unused, assumed 0)
    /// - flags (unused, assumed 0)
    /// - num of following deopt locations, D (= len - 3)
    /// - num of root entries, R (2*R <= D)
    /// - num of stack entries, S (S <= D)
    /// + {0..R} # R*2 entries
    ///     # (id, ptr) pair representing a typed gc root, living on the heap (or static)
    ///   - id constant: InstanceTypeId, root alloc type
    ///   - Location pointing to root
    /// + {0..S}
    ///     # (ptr) pointing to a Val on the stack
    ///   - Location pointing to stack val
    fn parse_record(function: &Function, record: &Record) -> (u64, GcRecord) {
        // can't get gc-live working properly, so GC pointers are passed in deopt
        // TODO: sort this out

        assert!(record.locations.len() >= 5);
        let (header, rest) = record.locations.split_at(5);

        assert_eq!(header[0].pointer, LocationPointer::Constant(0));
        assert_eq!(header[1].pointer, LocationPointer::Constant(0));
        let deopts = match header[2].pointer {
            LocationPointer::Constant(deopt_n) => deopt_n,
            _ => panic!(),
        };
        let roots_n = match header[3].pointer {
            LocationPointer::Constant(roots) => roots,
            _ => panic!(),
        };
        let stack_ptrs_n = match header[4].pointer {
            LocationPointer::Constant(stack_ptrs) => stack_ptrs,
            _ => panic!(),
        };

        assert_eq!(record.locations.len(), deopts as usize + 3);
        assert!(deopts >= roots_n * 2);
        assert!(deopts >= stack_ptrs_n);

        let (root_data, stack_data) = rest.split_at(2 * roots_n as usize);
        assert_eq!(stack_data.len(), stack_ptrs_n as usize);

        let root_locations = root_data
            .chunks_exact(2)
            .map(|chunk| {
                let id_loc = chunk[0];
                let ptr_loc = chunk[1];

                let id: InstanceTypeId;
                if let LocationPointer::Constant(cid) = id_loc.pointer {
                    id = InstanceTypeId::new(cid as _);
                } else {
                    panic!();
                }
                TaggedLocation {
                    tag: id,
                    location: ptr_loc,
                }
            })
            .collect();

        let stack_locations = stack_data.iter().copied().collect();

        let addr = function.addr + (record.instruction_offset as u64);
        return (
            addr,
            GcRecord {
                func_base: function.addr,
                root_locations,
                stack_locations,
            },
        );
    }
}
