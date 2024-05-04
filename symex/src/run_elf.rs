//! Simple runner that starts symbolic execution on LLVM bitcode.
//!
use std::time::Instant;

use regex::Regex;
use tracing::{debug, info, trace};

use crate::{
    elf_util::{ErrorReason, PathStatus, VisualPathResult},
    general_assembly::{
        self,
        executor::PathResult,
        project::PCHook,
        state::GAState,
        GAError,
        RunConfig,
    },
    smt::DContext,
};

fn add_architecture_independent_hooks(cfg: &mut RunConfig) {
    // intrinsic functions
    let start_cyclecount = |state: &mut GAState| {
        state.cycle_count = 0;
        trace!("Reset the cycle count (cycle count: {})", state.cycle_count);

        // jump back to where the function was called from
        let lr = state.get_register("LR".to_owned()).unwrap();
        state.set_register("PC".to_owned(), lr)?;
        Ok(())
    };
    let end_cyclecount = |state: &mut GAState| {
        // stop counting
        state.count_cycles = false;
        trace!(
            "Stopped counting cycles (cycle count: {})",
            state.cycle_count
        );

        // jump back to where the function was called from
        let lr = state.get_register("LR".to_owned()).unwrap();
        state.set_register("PC".to_owned(), lr)?;
        Ok(())
    };

    // add all pc hooks
    cfg.pc_hooks.push((
        Regex::new(r"^panic_cold_explicit$").unwrap(),
        PCHook::EndFaliure("explicit panic"),
    ));
    cfg.pc_hooks.push((
        Regex::new(r"^panic_bounds_check$").unwrap(),
        PCHook::EndFaliure("bounds check panic"),
    ));
    cfg.pc_hooks
        .push((Regex::new(r"^suppress_path$").unwrap(), PCHook::Suppress));
    cfg.pc_hooks.push((
        Regex::new(r"^unreachable_unchecked$").unwrap(),
        PCHook::EndFaliure("reach a unreachable unchecked call undefined behavior"),
    ));
    cfg.pc_hooks.push((
        Regex::new(r"^start_cyclecount$").unwrap(),
        PCHook::Intrinsic(start_cyclecount),
    ));
    cfg.pc_hooks.push((
        Regex::new(r"^end_cyclecount$").unwrap(),
        PCHook::Intrinsic(end_cyclecount),
    ));
    cfg.pc_hooks
        .push((Regex::new(r"^panic$").unwrap(), PCHook::EndFaliure("panic")));
}

/// Run symbolic execution on a elf file where `path` is the path to the ELF
/// file and `function` is the function the execution starts at.
/// `cfg` can be used to configure how the execution is carried out.
pub fn run_elf(
    path: &str,
    function: &str,
    mut cfg: RunConfig,
) -> Result<Vec<VisualPathResult>, GAError> {
    let context = Box::new(DContext::new());
    let context = Box::leak(context);

    add_architecture_independent_hooks(&mut cfg);

    let project = Box::new(general_assembly::project::Project::from_path(
        path, &mut cfg,
    )?);
    let project = Box::leak(project);
    let end_pc = 0xFFFFFFFE;
    project.add_pc_hook(end_pc, PCHook::EndSuccess);
    debug!("Created project: {:?}", project);

    info!("create VM");
    let mut vm = general_assembly::vm::VM::new(project, context, function, end_pc)?;

    run_elf_paths(&mut vm, &cfg)
}

/// Runs all paths in the vm
fn run_elf_paths(
    vm: &mut general_assembly::vm::VM,
    cfg: &RunConfig,
) -> Result<Vec<VisualPathResult>, GAError> {
    let mut path_num = 0;
    let start = Instant::now();
    let mut path_results = vec![];
    while let Some((path_result, state)) = vm.run()? {
        if matches!(path_result, PathResult::Suppress) {
            debug!("Suppressing path");
            continue;
        }
        if matches!(path_result, PathResult::AssumptionUnsat) {
            println!("Encountered an unsatisfiable assumption, ignoring this path");
            continue;
        }

        path_num += 1;

        let v_path_result = match path_result {
            general_assembly::executor::PathResult::Success(_v) => PathStatus::Ok(None),
            general_assembly::executor::PathResult::Faliure(reason) => {
                PathStatus::Failed(ErrorReason {
                    error_message: reason.to_owned(),
                })
            }
            general_assembly::executor::PathResult::AssumptionUnsat => todo!(),
            general_assembly::executor::PathResult::Suppress => todo!(),
        };

        let result = VisualPathResult::from_state(state, path_num, v_path_result)?;

        if cfg.show_path_results {
            println!("{}", result);
        }
        path_results.push(result);
    }
    if cfg.show_path_results {
        println!("time: {:?}", start.elapsed());
    }
    Ok(path_results)
}
