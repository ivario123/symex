# SYMEX

Symbolic execution engine that can operate on either LLVM IR or ARMv7-M machine code. Main use is to analyze Rust programs but programs written in other languages can potentially be analyzed.
Because the library used to read LLVM bytecode is large and cumbersome is the LLVM IR part of the tool hidden behind the feature flag `llvm`.

Since Symex was originally written with only LLVM IR execution in mind are the integration of machine code execution not always done coherently.
Work is ongoing in how the two part should coexist.
Because of this is the following documentation split up in two different parts one for LLVM IR and one for ARMv6-M machine code.

## ARMv6-M

### Getting started

The easiest way to use Symex is by the cargo-symex tool.
I can be installed by running:
```
cargo install --path cargo-symex
```
Then the examples can be executed by first navigating to the `armv6-m-examples` directory and executing:
```
cargo symex --elf --example [example name] --function [function name] (--release)
```

### Additional notes
- Ta analyse a function it must have a entry in the `.symtab` section of the elf file. All symbols in a elf file can be shown using the `readelf -s [path to elf file]` command. To tell rustc to not mangle the function name the attribute `#[no_mangle]` can be used.
- When using symex-lib functions or to be able to detect panic the debug-data must be included in teh elf file.
- A elf file can directly be analyzed with cargo-symex by the `cargo symex --elf --path [path to elf file] --function [function name]`
- Symex can be directly used as a library see `wcet-analasis-example` directory for examples on how to do that.

### Notes on the max cycle count on armv6-m
The max cycle count for each path is calculated by counting the number of cycles for each instruction according to [this document](https://developer.arm.com/documentation/ddi0432/c/programmers-model/instruction-set-summary). It assumes a core without wait-states.

## LLVM IR

### Cargo subcommand

A cargo subcommand is available to easily compile Rust programs into bitcode files and run them
in the symbolic execution engine.

It can be installed with

```shell
> cargo install --path cargo-symex --features llvm
```

For usage instructions see `cargo symex --help`.


### Getting started

Check out the examples contained in `examles/examples`. These can be run with the cargo subcommand

```shell
> cd examples
> cargo-symex --example <example>
```

To compile and run the example `examples/rust_simple` using the cargo subcommand

```shell
> cd examples
> cargo symex --example simple --function rust_simple_test
```

This will display the results of the analysis of the example, showing all the paths it took and
concrete values for all inputs and output.



### Building

#### Dependencies

- [LLVM](https://llvm.org/), used as a library for decoding the LLVM-IR (internal representation)
  of the program under analysis.
- [boolector](https://github.com/Boolector/boolector), Boolector is a Satisfiability Modulo Theories
  (SMT) solver for the theories of fixed-size bit-vectors, arrays and uninterpreted functions.

The project currently uses LLVM 17 which require a relatively recent version of Rust.

#### Devcontainer

As an alternative, a [Dev Container](https://code.visualstudio.com/docs/devcontainers/containers) is provided that automatically installs Rust `1.72` and LLVM 17.

To generate tests use `./symex/compile_tests_dc.sh` instead.

### Making tests work

The tests make use of compiled LLVM IR files which are not tracked by git. To make the tests work
run

```shell
> ./scripts/compile_tests.sh
```

### Known issues

Sometimes when running examples the runner may give

```shell
error: could not copy "<project_dir>/target/debug/examples/<some_file>.bc" to "<project_dir>/target/debug/examples/<some_file>.bc": No such file or directory (os error 2)

error: could not copy "<project_dir>/target/debug/examples/<some_file>.ll" to "<project_dir>/target/debug/examples/<some_file>.ll": No such file or directory (os error 2)

error: could not compile `examples` due to 2 previous errors
```

Until the issue is fixed it can remedied by running `cargo clean` and trying again.

## SYMEX LLVM 14

The older version `symex-llvm-14-legacy` is still avaiable (for now). But it will be phased out later in favor of the more updated version that supports LLVM 17.

Due to LLVM dependencies the current implementation is limited to Rust < 1.64. `cd` to the folder where you want to run `cargo symex` from and override the Rust version to be used as below:

```shell
cd <folder>
rustup override set 1.64
```

Required dependencies for `symex-llvm-14-legacy`

- [LLVM](https://llvm.org/), used as a library for decoding the LLVM-IR (internal representation)
  of the program under analysis.

SMT solver defaults to `boolector`. It is possible to use Z3 instead of Boolector by using the feature flag `z3`.

- [boolector](https://github.com/Boolector/boolector), Boolector is a Satisfiability Modulo Theories
  (SMT) solver for the theories of fixed-size bit-vectors, arrays and uninterpreted functions.
- [Z3](https://github.com/Z3Prover/z3), Z3 is a theorem prover from Microsoft Research.

To use the devcontainer with this setup, see the notes in `.devcontainer/devcontainer.json`.


## Debug output from SYMEX

The implementation uses the Rust log framework. You can set the logging level to the environment variable `RUST_LOG`. See below example (assumes the cargo-sub command `symex`).

```shell
> RUST_LOG=DEBUG cargo symex ...
```

If you want to narrow down the scope of logging you can give a list of modules to log.

```shell
> RUST_LOG="symex=debug" cargo symex ...
```

Symex uses different logging levels:

- info, high level logging of steps taken.
- debug, general logging of important actions.
- trace, further information on internal operations and data structures.

You can also narrow down the scope to specific modules, e.g. the executor.

```shell
> RUST_LOG="symex::executor=trace" cargo symex ...
```



## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
