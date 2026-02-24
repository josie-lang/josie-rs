/**
 * Runtime attribute/keyword contract based on Josie Web V1 behavior.
 */

export type RuntimeBundle = "josie-runtime.js" | "josie-runtime-dev.js";

export type RuntimeAttribute =
  | "j-text"
  | "j-model"
  | "j-attr:*"
  | "@event"
  | "data-jid"
  | "data-josie-map"
  | "j-component"
  | "j-md"
  | "j-md-var"
  | "script[type=application/josie+json]";

export type RuntimeKeyword =
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
  | "return"
  | "var"
  | "w.event.value"
  | "w.event.key"
  | "w.raf"
  | "w.after"
  | "util.*"
  | "data.*"
  | "x.*";

export interface RuntimeRule {
  attribute: RuntimeAttribute;
  valueType: "path" | "josie-expression" | "json-object" | "string";
  notes?: string;
}

export const RUNTIME_RULES: RuntimeRule[] = [
  { attribute: "j-text", valueType: "path", notes: "Shorthand text binding." },
  {
    attribute: "j-model",
    valueType: "path",
    notes: "Two-way model shorthand; emits set(path, w.event.value).",
  },
  {
    attribute: "j-attr:*",
    valueType: "path",
    notes: "Dynamic attribute shorthand (example: j-attr:class).",
  },
  {
    attribute: "@event",
    valueType: "string",
    notes: "Event action name (resolved to step entrypoint). Runtime uses delegated listeners per event type.",
  },
  {
    attribute: "data-jid",
    valueType: "string",
    notes: "Compiler-stamped stable DOM identity used by fine-grained runtime updates.",
  },
  {
    attribute: "data-josie-map",
    valueType: "json-object",
    notes: "Map/list render config ({source, key?}) with keyed reconciliation.",
  },
  {
    attribute: "j-component",
    valueType: "string",
    notes: "Reusable component include via slug.",
  },
  {
    attribute: "j-md",
    valueType: "string",
    notes: "Inline markdown source to render during compile.",
  },
  {
    attribute: "j-md-var",
    valueType: "path",
    notes: "Render-time markdown binding from backend vars.",
  },
  {
    attribute: "script[type=application/josie+json]",
    valueType: "json-object",
    notes: "Canonical Program payload (kind/version/state/actions/resources/steps).",
  },
];
