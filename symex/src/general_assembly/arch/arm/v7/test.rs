use std::collections::HashMap;

use crate::{
    general_assembly::{
        arch::arm::v7::decoder::Convert,
        executor::GAExecutor,
        instruction::{CycleCount, Instruction},
        project::Project,
        state::GAState,
        vm::VM,
        Endianness, WordSize,
    },
    smt::{DContext, DSolver},
};

use super::ArmV7EM;
use general_assembly::operand::Operand;

use disarmv7::prelude::{operation::*, *};

macro_rules! get_operand {
    ($exec:ident register $id:ident) => {{
        let operand = Operand::Register(stringify!($id).to_owned());
        let local = HashMap::new();
        $exec
            .get_operand_value(&operand, &local)
            .expect("Could not find a test specified register")
            .get_constant()
            .expect("Could not get test specified register as constant")
    }};
    ($exec:ident flag $id:ident) => {{
        let operand = Operand::Flag(stringify!($id).to_owned());
        let local = HashMap::new();
        $exec
            .get_operand_value(&operand, &local)
            .expect("Could not find a test specified flag")
            .get_constant()
            .expect("Could not get test specified flag as constant")
    }};
}

/// This can be mis used but will fail at compile time if not correctly structured.
macro_rules! test {
    ($exec:ident {
        $(
            $(
                register $reg:ident
            )?
            $(
                flag $flag:ident
            )?
            $(
                address $address:literal
            )?

            $(== $eq_rhs:literal)?
            $(!= $neq_rhs:literal)?
        ),*
    }) => {
        $(
            let result = get_operand!($exec $(register $reg)? $(address $address)? $(flag $flag)?);
            assert!(result $(== $eq_rhs)? $(!= $neq_rhs)?,stringify!( $($reg)? $($address)? $($flag)? $(!= $eq_rhs)? $(== $neq_rhs)?));
        )*

    };
}

/// This can be mis used but will fail at compile time if not correctly structured.
macro_rules! initiate {
    ($exec:ident {
        $(
            $(
                register $reg:ident
            )?
            $(
                flag $flag:ident
            )?
            $(
                address $address:literal
            )?

            = $eq_value:expr
        );*
    }) => {
        $(
            let operand = initiate!($exec $(register $reg)? $(address $address)? $(flag $flag)?);
            let intermediate = Operand::Immidiate(general_assembly::operand::DataWord::Word32($eq_value as u32));
            let operation = general_assembly::operation::Operation::Move { destination: operand, source: intermediate};
            $exec.execute_operation(&operation,&mut HashMap::new()).expect("Malformed test");
        )*

    };

    ($exec:ident register $id:ident) => {
        Operand::Register(stringify!($id).to_owned())
    };

    ($exec:ident flag $id:ident) => {
        Operand::Flag(stringify!($id).to_owned())
    };
}

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
    let mut project = Box::new(Project::manual_project(
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
    project.add_hooks();

    let project = Box::leak(project);
    let context = Box::new(DContext::new());
    let context = Box::leak(context);
    let solver = DSolver::new(context);
    let state = GAState::create_test_state(project, context, solver, 0, u32::MAX as u64);
    let vm = VM::new_with_state(project, state);
    vm
}

#[test]
fn test_adc_no_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R0 = 1;
        register R1 = 2;
        register R2 = 3;
        flag C = true
    });

    let instruction: Operation = AdcRegister::builder()
        .set_s(Some(SetFlags::Literal(false)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 6,
        flag C == 1
    });
}

#[test]
fn test_adc_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R0 = 1;
        register R1 = 2;
        register R2 = 3;
        flag C = true
    });

    let instruction: Operation = AdcRegister::builder()
        .set_s(Some(SetFlags::Literal(true)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 6,
        flag C == 0
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x80000000;
        register R2 = 0x80000000;
        flag C = false
    });

    let instruction: Operation = AdcRegister::builder()
        .set_s(Some(SetFlags::Literal(true)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0,
        flag C == 1,
        flag Z == 1,
        flag V == 1
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x80000000;
        register R2 = 0x80000000;
        flag C = false;
        flag V = false;
        flag Z = false
    });

    let instruction: Operation = AdcRegister::builder()
        .set_s(Some(SetFlags::InITBlock(true)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(true),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0,
        flag C == 1,
        flag Z == 1,
        flag V == 1
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x80000000;
        register R2 = 0x80000000;
        flag C = false;
        flag V = false;
        flag Z = false
    });

    let instruction: Operation = AdcRegister::builder()
        .set_s(Some(SetFlags::InITBlock(false)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(true),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0,
        flag C == 0,
        flag Z == 0,
        flag V == 0
    });
}

#[test]
fn test_adc_imm_no_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R1 = 2;
        flag C = true
    });

    let instruction: Operation = AdcImmediate::builder()
        .set_s(Some(false))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_imm(3)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 6,
        flag C == 1
    });
}
#[test]
fn test_adc_immediate_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R0 = 1;
        register R1 = 2;
        register R2 = 3;
        flag C = true
    });

    let instruction: Operation = AdcImmediate::builder()
        .set_s(Some(true))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_imm(3)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 6,
        flag C == 0
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x80000000;
        flag C = false
    });

    let instruction: Operation = AdcImmediate::builder()
        .set_s(Some(true))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_imm(0x80000000)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0,
        flag C == 1,
        flag Z == 1,
        flag V == 1
    });
}

#[test]
fn test_add_no_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R0 = 1;
        register R1 = 2;
        register R2 = 3;
        flag C = true
    });

    let instruction: Operation = AddRegister::builder()
        .set_s(Some(SetFlags::Literal(false)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 5,
        flag C == 1
    });
}

#[test]
fn test_add_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R0 = 1;
        register R1 = 2;
        register R2 = 3;
        flag C = true
    });

    let instruction: Operation = AddRegister::builder()
        .set_s(Some(SetFlags::Literal(true)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 5,
        flag C == 0
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x80000000;
        register R2 = 0x80000000;
        flag C = false
    });

    let instruction: Operation = AddRegister::builder()
        .set_s(Some(SetFlags::Literal(true)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0,
        flag C == 1,
        flag Z == 1,
        flag V == 1
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x80000000;
        register R2 = 0x80000000;
        flag C = true;
        flag V = false;
        flag Z = false
    });

    let instruction: Operation = AddRegister::builder()
        .set_s(Some(SetFlags::InITBlock(true)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(true),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0,
        flag C == 1,
        flag Z == 1,
        flag V == 1
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x80000000;
        register R2 = 0x80000000;
        flag C = false;
        flag V = false;
        flag Z = false
    });

    let instruction: Operation = AddRegister::builder()
        .set_s(Some(SetFlags::InITBlock(false)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(true),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0,
        flag C == 0,
        flag Z == 0,
        flag V == 0
    });
}

#[test]
fn test_add_imm_no_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R1 = 2;
        flag C = true
    });

    let instruction: Operation = AdcImmediate::builder()
        .set_s(Some(false))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_imm(3)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 6,
        flag C == 1
    });
}

#[test]
fn test_add_immediate_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R0 = 1;
        register R1 = 2;
        register R2 = 3;
        flag C = true
    });

    let instruction: Operation = AdcImmediate::builder()
        .set_s(Some(true))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_imm(3)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 6,
        flag C == 0
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x80000000;
        flag C = false
    });

    let instruction: Operation = AdcImmediate::builder()
        .set_s(Some(true))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_imm(0x80000000)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0,
        flag C == 1,
        flag Z == 1,
        flag V == 1
    });
}

#[test]
fn test_add_sp_immediate() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R1 = 8;
        register SP = 8;
        flag C = true
    });

    let instruction: Operation = AddSPImmediate::builder()
        .set_s(Some(true))
        .set_rd(Some(Register::SP))
        .set_imm(16)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register SP == 24,
        flag C == 0
    });

    initiate!(executor {
        register R1 = 8;
        register SP = 9;
        flag C = true
    });

    let instruction: Operation = AddSPImmediate::builder()
        .set_s(Some(false))
        .set_rd(Some(Register::SP))
        .set_imm(16)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register SP == 24,
        flag C == 1
    });
}

#[test]
fn test_add_sp_reg() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R1 = 16;
        register SP = 8;
        flag C = true
    });

    let instruction: Operation = AddSPRegister::builder()
        .set_s(Some(true))
        .set_rd(Some(Register::SP))
        .set_rm(Register::R1)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register SP == 24,
        flag C == 0
    });

    initiate!(executor {
        register R1 = 16;
        register SP = 9;
        flag C = true
    });

    let instruction: Operation = AddSPRegister::builder()
        .set_s(Some(false))
        .set_rd(Some(Register::SP))
        .set_rm(Register::R1)
        .set_shift(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register SP == 24,
        flag C == 1
    });

    initiate!(executor {
        register R1 = 8;
        register SP = 9;
        flag C = true
    });

    let instruction: Operation = AddSPRegister::builder()
        .set_s(Some(false))
        .set_rd(Some(Register::SP))
        .set_rm(Register::R1)
        .set_shift(Some(ImmShift {
            shift_n: 1,
            shift_t: Shift::Lsl,
        }))
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register SP == 24,
        flag C == 1
    });
}

#[test]
fn test_adr() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register PC = 16;
        flag C = true
    });

    let instruction: Operation = Adr::builder()
        .set_rd(Register::PC)
        .set_imm(4)
        .set_add(true)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register PC == 24
    });

    initiate!(executor {
        register PC = 16;
        flag C = true
    });

    let instruction: Operation = Adr::builder()
        .set_rd(Register::PC)
        .set_imm(4)
        .set_add(false)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register PC == 16
    });
}

#[test]
fn test_and_no_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x00000002;
        register R2 = 0x80000001;
        flag C = true
    });

    let instruction: Operation = AndRegister::builder()
        .set_s(Some(SetFlags::Literal(false)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(Some(ImmShift {
            shift_n: 1,
            shift_t: Shift::Lsl,
        }))
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0b10,
        flag C == 1
    });
}

#[test]
fn test_and_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x00000002;
        register R2 = 0x80000001;
        flag C = false
    });

    let instruction: Operation = AndRegister::builder()
        .set_s(Some(SetFlags::Literal(true)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(Some(ImmShift {
            shift_n: 1,
            shift_t: Shift::Lsl,
        }))
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0b10,
        flag C == 1
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x00000002;
        register R2 = 0x80000002;
        flag C = false
    });

    let instruction: Operation = AndRegister::builder()
        .set_s(Some(SetFlags::Literal(true)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(Some(ImmShift {
            shift_n: 1,
            shift_t: Shift::Lsl,
        }))
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0b00,
        flag C == 1,
        flag Z == 1,
        flag N == 0
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x00000002;
        register R2 = 0x80000002;
        flag C = 0;
        flag Z = 0;
        flag N = 0
    });

    let instruction: Operation = AndRegister::builder()
        .set_s(Some(SetFlags::InITBlock(true)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(Some(ImmShift {
            shift_n: 1,
            shift_t: Shift::Lsl,
        }))
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(true),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0b00,
        flag C == 1,
        flag Z == 1,
        flag N == 0
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x00000002;
        register R2 = 0x80000002;
        flag C = 0;
        flag Z = 0;
        flag N = 0
    });

    let instruction: Operation = AndRegister::builder()
        .set_s(Some(SetFlags::InITBlock(false)))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_rm(Register::R2)
        .set_shift(Some(ImmShift {
            shift_n: 1,
            shift_t: Shift::Lsl,
        }))
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0b00,
        flag C == 1,
        flag Z == 1,
        flag N == 0
    });
}

#[test]
fn test_and_imm_no_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R1 = 2;
        flag C = true
    });

    let instruction: Operation = AdcImmediate::builder()
        .set_s(Some(false))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_imm(3)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };
    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 6,
        flag C == 1
    });
}

#[test]
fn test_and_immediate_set_flag() {
    let mut vm = setup_test_vm();
    let project = vm.project;

    let mut executor = GAExecutor::from_state(vm.paths.get_path().unwrap().state, &mut vm, project);

    initiate!(executor {
        register R1 = 0x00000002;
        flag C = false
    });

    let instruction: Operation = AndImmediate::builder()
        .set_s(Some(true))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_imm(0x00000002)
        .set_carry(Some(true))
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };

    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0b10,
        flag C == 1
    });

    initiate!(executor {
        register R1 = 0x00000002;
        flag C = false
    });

    let instruction: Operation = AndImmediate::builder()
        .set_s(Some(true))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_imm(0x00000002)
        .set_carry(Some(false))
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };

    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0b10,
        flag C == 0
    });

    initiate!(executor {
        register R0 = 1;
        register R1 = 0x00000002;
        register R2 = 0x80000002;
        flag C = 0;
        flag Z = 0;
        flag N = 0
    });

    initiate!(executor {
        register R1 = 0x00000002;
        flag C = false
    });

    let instruction: Operation = AndImmediate::builder()
        .set_s(Some(true))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_imm(0x00000000)
        .set_carry(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };

    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0b00,
        flag C == 0
    });

    initiate!(executor {
        register R1 = 0x80000002;
        flag C = false
    });

    let instruction: Operation = AndImmediate::builder()
        .set_s(Some(true))
        .set_rd(Some(Register::R1))
        .set_rn(Register::R1)
        .set_imm(0x80000000)
        .set_carry(None)
        .complete()
        .into();

    let instruction = Instruction {
        operations: (16, instruction).convert(false),
        memory_access: false,
        instruction_size: 16,
        max_cycle: CycleCount::Value(0),
    };

    println!("Running instruction {:?}", instruction);
    executor
        .execute_instruction(&instruction)
        .expect("Malformed instruction");

    test!(executor {
        register R1 == 0x80000000,
        flag C == 0,
        flag N == 1
    });
}
