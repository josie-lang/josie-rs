/**
 * JOSIE Program Contract v0.1.0
 * Canonical pipeline-first shape.
 */

export const PROGRAM_KIND = "program" as const;
export const PROGRAM_VERSION = "0.1.0" as const;

export type JsonPrimitive = string | number | boolean | null;
export type JsonValue = JsonPrimitive | JsonObject | JsonValue[];
export interface JsonObject {
  [key: string]: JsonValue;
}

export type ProgramState = JsonObject;

export type StepOp =
  | "set"
  | "get"
  | "call"
  | "map"
  | "filter"
  | "for_each"
  | "reduce"
  | "if"
  | "match"
  | "do"
  | "return";

export type StepType =
  | "action"
  | "decision"
  | "transform"
  | "tool"
  | "checkpoint";

export type StepOnError = "retry" | "fallback" | "halt" | "compensate";

/**
 * Canonical step shape.
 *
 * Notes:
 * - For `op: "call"`, use `from` as callable target (example: "x.pg.query").
 * - `do` and `when` are expression trees.
 */
export interface ProgramStep {
  id?: string;
  type?: StepType;
  op: StepOp;
  from?: string;
  into?: string;
  args?: JsonValue[];
  do?: JsonValue;
  when?: JsonValue;
  input?: JsonObject;
  output?: JsonObject;
  on_error?: StepOnError;
  timeout_ms?: number;
  max_retries?: number;
  idempotency_key?: string;
}

export interface Program {
  kind: typeof PROGRAM_KIND;
  version: typeof PROGRAM_VERSION;
  state: ProgramState;
  steps: ProgramStep[];
}

export interface ValidationIssue {
  code: string;
  message: string;
  stepIndex?: number;
}

export interface CheckResult {
  ok: boolean;
  issues: ValidationIssue[];
}

export interface RunResult {
  value: JsonValue;
  state: ProgramState;
}

export interface CompiledProgram {
  kind: typeof PROGRAM_KIND;
  version: typeof PROGRAM_VERSION;
  fingerprint: string;
}

export type ExtensionOperator = (
  args: JsonValue[],
  state: ProgramState
) => JsonValue | Promise<JsonValue>;

/**
 * Engine-scoped API (instance-first, no global registry).
 */
export interface Engine {
  register(name: `x.${string}`, op: ExtensionOperator): Engine;
  check(program: Program): CheckResult;
  run(program: Program, state?: ProgramState): RunResult | Promise<RunResult>;
  compile(program: Program): CompiledProgram;
  run_compiled(
    compiled: CompiledProgram,
    state?: ProgramState
  ): RunResult | Promise<RunResult>;
}

/**
 * Canonical constructor contract.
 * Implementation lives in runtime package, this is contract-only.
 */
export interface EngineFactory {
  new(): Engine;
}
