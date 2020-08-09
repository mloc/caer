// Personality function + LSDA handling mostly cribbed from rust compiler source code,  commit ef92009c1dbe2750f1d24a6619b827721fb49749
// Copyright the rust project's contributors: https://github.com/rust-lang/rust/blob/master/COPYRIGHT

use rustc_unwind as unwind;
use libc::{c_int, uintptr_t};
use rustc_dwarf as dwarf;
use crate::val::Val;

const DM_EXCEPTION_CLASS: unwind::_Unwind_Exception_Class = 0x43414552_444d_0000;

struct Exception {
    #[allow(dead_code)]
    header: unwind::_Unwind_Exception,
    exception_val: Val,
}
#[no_mangle]
unsafe extern "C" fn rt_exception_get_val(exception: &Exception) -> Val {
    exception.exception_val
}

#[no_mangle]
unsafe extern "C" fn rt_throw(exception_val: Val) -> ! {
    let exception = Box::new(Exception {
        header: unwind::_Unwind_Exception {
            exception_class: DM_EXCEPTION_CLASS,
            exception_cleanup,
            private: [0; unwind::unwinder_private_data_size],
        },
        exception_val,
    });
    let exception_param = Box::into_raw(exception) as *mut unwind::_Unwind_Exception;
    let ret = unwind::_Unwind_RaiseException(exception_param);

    println!("_Unwind_RaiseException errored with {:?}, aborting.", ret);
    std::process::abort();

    extern "C" fn exception_cleanup(
        _unwind_code: unwind::_Unwind_Reason_Code,
        exception: *mut unwind::_Unwind_Exception,
    ) {
        unsafe {
            let _: Box<Exception> = Box::from_raw(exception as *mut Exception);
        }
    }
}

#[no_mangle]
unsafe extern "C" fn dm_eh_personality(
    version: c_int,
    actions: unwind::_Unwind_Action,
    _exception_class: unwind::_Unwind_Exception_Class,
    exception_object: *mut unwind::_Unwind_Exception,
    context: *mut unwind::_Unwind_Context,
) -> unwind::_Unwind_Reason_Code {
    if version != 1 {
        return unwind::_URC_FATAL_PHASE1_ERROR;
    }
    let lsda = unwind::_Unwind_GetLanguageSpecificData(context) as *const u8;
    let eh_action = match handle_lsda(lsda, context) {
        Ok(action) => action,
        Err(_) => return unwind::_URC_FATAL_PHASE1_ERROR,
    };
    if actions as i32 & unwind::_UA_SEARCH_PHASE as i32 != 0 {
        match eh_action {
            EHAction::None |
                EHAction::Cleanup(_) => unwind::_URC_CONTINUE_UNWIND,
            EHAction::Catch(_) => unwind::_URC_HANDLER_FOUND,
            EHAction::Terminate => unwind::_URC_FATAL_PHASE1_ERROR,
        }
    } else {
        match eh_action {
            EHAction::None => unwind::_URC_CONTINUE_UNWIND,
            EHAction::Cleanup(lpad) |
                EHAction::Catch(lpad) => {
                    unwind::_Unwind_SetGR(context, unwind::UNWIND_DATA_REG.0,
                        exception_object as uintptr_t);
                    unwind::_Unwind_SetGR(context, unwind::UNWIND_DATA_REG.1, 0);
                    unwind::_Unwind_SetIP(context, lpad);
                    unwind::_URC_INSTALL_CONTEXT
                }
            EHAction::Terminate => unwind::_URC_FATAL_PHASE2_ERROR,
        }
    }
}

#[derive(Debug)]
enum EHAction {
    None,
    Cleanup(usize),
    Catch(usize),
    Terminate,
}

unsafe fn handle_lsda(lsda: *const u8, context: *mut unwind::_Unwind_Context) -> Result<EHAction, ()> {
    if lsda.is_null() {
        return Ok(EHAction::None);
    }

    let func_start = unwind::_Unwind_GetRegionStart(context);
    let mut reader = dwarf::DwarfReader::new(lsda);

    let start_encoding = reader.read::<u8>();
    // base address for landing pad offsets
    let lpad_base = if start_encoding != dwarf::DW_EH_PE_omit {
        reader.read_encoded(start_encoding)?
    } else {
        func_start
    };

    let ttype_encoding = reader.read::<u8>();
    if ttype_encoding != dwarf::DW_EH_PE_omit {
        // Rust doesn't analyze exception types, so we don't care about the type table
        reader.read_uleb128();
    }

    let call_site_encoding = reader.read::<u8>();
    let call_site_table_length = reader.read_uleb128();
    let action_table = reader.ptr.offset(call_site_table_length as isize);
    let ip = {
        let mut ip_before_instr: c_int = 0;
        let ip = unwind::_Unwind_GetIPInfo(context, &mut ip_before_instr);
        if ip_before_instr == 0 {
            ip - 1
        } else {
            ip
        }
    };

    while reader.ptr < action_table {
        let cs_start = reader.read_encoded(call_site_encoding)?;
        let cs_len = reader.read_encoded(call_site_encoding)?;
        let cs_lpad = reader.read_encoded(call_site_encoding)?;
        let cs_action = reader.read_uleb128();
        // Callsite table is sorted by cs_start, so if we've passed the ip, we
        // may stop searching.
        if ip < func_start + cs_start {
            break;
        }
        if ip < func_start + cs_start + cs_len {
            if cs_lpad == 0 {
                return Ok(EHAction::None);
            } else {
                let lpad = lpad_base + cs_lpad;
                //return Ok(interpret_cs_action(cs_action, lpad, foreign_exception));
                // currently, all DM cleanup pads are actually catch pads
                return Ok(EHAction::Catch(lpad));
            }
        }
    }
    Ok(EHAction::Terminate)
}
