use std::collections::HashMap;

use general_assembly::{
    operand::{DataWord, Operand},
    operation::Operation,
};

use crate::{
    general_assembly::{
        executor::GAExecutor, project::Project, state::GAState, vm::VM, Endianness, WordSize,
    },
    smt::{DContext, DSolver},
};

use super::ArmV7EM;

fn setup_test_vm() -> VM {
    /*
        program_memory: Vec<u8>,
        start_addr: u64,
        end_addr: u64,
        word_size: WordSize,
        endianness: Endianness,
        architecture: A,
        symtab: HashMap<String, u64>,
        pc_hooks: PCHooks,
        reg_read_hooks: RegisterReadHooks,
        reg_write_hooks: RegisterWriteHooks,
        single_memory_read_hooks: SingleMemoryReadHooks,
        range_memory_read_hooks: RangeMemoryReadHooks,
        single_memory_write_hooks: SingleMemoryWriteHooks,
        range_memory_write_hooks: RangeMemoryWriteHooks,
    */
    // create an empty project
    let project = Box::new(Project::manual_project(
        vec![],
        0,
        0,
        WordSize::Bit32,
        Endianness::Little,
        ArmV7EM {},
        HashMap::new(),
        HashMap::new(),
        HashMap::new(),
        HashMap::new(),
        HashMap::new(),
        vec![],
        HashMap::new(),
        vec![],
    ));
    let project = Box::leak(project);
    let context = Box::new(DContext::new());
    let context = Box::leak(context);
    let solver = DSolver::new(context);
    let state = GAState::create_test_state(project, context, solver, 0, u32::MAX as u64);
    let vm = VM::new_with_state(project, state);
    vm
}

#[test]
fn test_mul() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);
    let mut local = HashMap::new();

    let r0 = Operand::Register("R0".to_owned());
    let imm_42 = Operand::Immidiate(DataWord::Word32(42));
    let imm_minus_42 = Operand::Immidiate(DataWord::Word32(-42i32 as u32));
    let imm_16 = Operand::Immidiate(DataWord::Word32(16));
    let imm_minus_16 = Operand::Immidiate(DataWord::Word32(-16i32 as u32));

    // simple multiplication
    let operation = Operation::Mul {
        destination: r0.clone(),
        operand1: imm_42.clone(),
        operand2: imm_16.clone(),
    };
    executor.execute_operation(&operation, &mut local).ok();

    let r0_value = executor
        .get_operand_value(&r0, &local)
        .unwrap()
        .get_constant()
        .unwrap();
    assert_eq!(r0_value, 672);

    // multiplication right minus
    let operation = Operation::Mul {
        destination: r0.clone(),
        operand1: imm_42.clone(),
        operand2: imm_minus_16.clone(),
    };
    executor.execute_operation(&operation, &mut local).ok();

    let r0_value = executor
        .get_operand_value(&r0, &local)
        .unwrap()
        .get_constant()
        .unwrap();
    assert_eq!(r0_value as u32, -672i32 as u32);

    // multiplication left minus
    let operation = Operation::Mul {
        destination: r0.clone(),
        operand1: imm_minus_42.clone(),
        operand2: imm_16.clone(),
    };
    executor.execute_operation(&operation, &mut local).ok();

    let r0_value = executor
        .get_operand_value(&r0, &local)
        .unwrap()
        .get_constant()
        .unwrap();
    assert_eq!(r0_value as u32, -672i32 as u32);

    // multiplication both minus
    let operation = Operation::Mul {
        destination: r0.clone(),
        operand1: imm_minus_42.clone(),
        operand2: imm_minus_16.clone(),
    };
    executor.execute_operation(&operation, &mut local).ok();

    let r0_value = executor
        .get_operand_value(&r0, &local)
        .unwrap()
        .get_constant()
        .unwrap();
    assert_eq!(r0_value, 672);
}
