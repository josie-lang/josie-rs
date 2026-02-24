use crate::program::{
    CompiledProgram, ExecutionOutput, Program, RuntimeError, ValidationError, compile_program,
    execute_compiled_program, execute_program, parse_program,
};
use crate::{Operator, Operators, State};
use serde_json::Value;

/// Engine-scoped execution surface for JOSIE programs.
///
/// This keeps operator registration instance-local (no global singleton),
/// so different runtimes/projects can have different `x.*` extensions safely.
#[derive(Default)]
pub struct Engine {
    operators: Operators,
}

impl Engine {
    /// Create a new engine with core + stdlib operators preloaded.
    pub fn new() -> Self {
        Self {
            operators: Operators::new(),
        }
    }

    /// Register or replace an operator, typically in `x.*`.
    pub fn register(&mut self, name: impl Into<String>, operator: Operator) -> Option<Operator> {
        self.operators.register(name, operator)
    }

    /// Validate and parse program document.
    pub fn check(&self, input: &Value) -> Result<Program, ValidationError> {
        parse_program(input)
    }

    /// Compile once.
    pub fn compile(&self, program: &Program) -> Result<CompiledProgram, ValidationError> {
        compile_program(program)
    }

    /// Run one-shot.
    pub fn run(&self, program: &Program) -> Result<ExecutionOutput, RuntimeError> {
        execute_program(program, &self.operators)
    }

    /// Run compiled program with caller-managed state.
    pub fn run_compiled(
        &self,
        compiled: &CompiledProgram,
        state: &mut State,
    ) -> Result<Value, RuntimeError> {
        execute_compiled_program(compiled, state, &self.operators)
    }

    /// Access the operator registry (read-only).
    pub fn operators(&self) -> &Operators {
        &self.operators
    }
}

