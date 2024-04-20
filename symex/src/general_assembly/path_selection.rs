use std::fmt::Display;

use crate::smt::DExpr;

use super::state::GAState;

#[derive(Debug, Clone)]
pub struct Path {
    /// The state to use when resuming execution.
    ///
    /// The location in the state should be where to resume execution at.
    pub state: GAState,

    /// Constraints to add before starting execution on this path.
    pub constraints: Vec<DExpr>,
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let state = format!("{:}", self.state);
        let constraints = self
            .constraints
            .iter()
            .enumerate()
            .map(|(idx, constraint)| format!("| {idx} -> {constraint:?}\n"))
            .collect::<String>();

        write!(
            f,
            "
- Path
-------
- Conditions:
{constraints}
-------
{state}"
        )
    }
}

impl Path {
    /// Creates a new path starting at a certain state, optionally asserting a condition on the
    /// created path.
    pub fn new(state: GAState, constraint: Option<DExpr>) -> Self {
        let constraints = match constraint {
            Some(c) => vec![c],
            None => vec![],
        };

        Self { state, constraints }
    }
}

/// Depth-first search path exploration.
///
/// Each path is explored for as long as possible, when a path finishes the most recently added
/// path is the next to be run.
#[derive(Debug, Clone)]
pub struct DFSPathSelection {
    paths: Vec<Path>,
}

impl DFSPathSelection {
    /// Creates new without any stored paths.
    pub fn new() -> Self {
        Self { paths: Vec::new() }
    }

    /// Add a new path to be explored.
    pub fn save_path(&mut self, path: Path) {
        path.state.constraints.push();
        self.paths.push(path);
    }

    /// Retrieve the next path to explore.
    pub fn get_path(&mut self) -> Option<Path> {
        match self.paths.pop() {
            Some(path) => {
                path.state.constraints.pop();
                println!(
                    "Re-starting path \nPC:{:#10x}\ncondition:{:?}",
                    path.state.last_pc, path.constraints
                );
                Some(path)
            }
            None => None,
        }
    }

    pub fn waiting_paths(&self) -> usize {
        self.paths.len()
    }
}
