//! Descrebes the VM for general assembly

use super::{
    executor::{GAExecutor, PathResult},
    path_selection::DFSPathSelection,
    project::Project,
    Result,
};
use crate::{
    general_assembly::{path_selection::Path, state::GAState},
    smt::{DContext, DSolver},
};

#[derive(Debug)]
pub struct VM {
    pub project: &'static Project,
    pub paths: DFSPathSelection,
}

impl VM {
    pub fn new(
        project: &'static Project,
        ctx: &'static DContext,
        fn_name: &str,
        end_pc: u64,
    ) -> Result<Self> {
        let mut vm = Self {
            project,
            paths: DFSPathSelection::new(),
        };

        let solver = DSolver::new(ctx);
        let state = GAState::new(ctx, project, solver, fn_name, end_pc)?;

        vm.paths.save_path(Path::new(state, None));

        Ok(vm)
    }

    pub fn new_with_state(project: &'static Project, state: GAState) -> Self {
        let mut vm = Self {
            project,
            paths: DFSPathSelection::new(),
        };

        vm.paths.save_path(Path::new(state, None));

        vm
    }

    pub fn run(&mut self) -> Result<Option<(PathResult, GAState)>> {
        if let Some(path) = self.paths.get_path() {
            // try stuff
            let mut executor = GAExecutor::from_state(path.state, self, self.project);

            for constraint in path.constraints {
                executor.state.constraints.assert(&constraint);
            }

            let result = executor.resume_execution()?;
            return Ok(Some((result, executor.state)));
        }
        Ok(None)
    }
}
