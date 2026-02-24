/**
 * Josie Web Contract v0.1.0
 * Foundation: compile first, render fast.
 */

export const JOSIE_WEB_KIND = "josie-web" as const;
export const JOSIE_WEB_VERSION = "0.1.0" as const;

export type JsonPrimitive = string | number | boolean | null;
export type JsonValue = JsonPrimitive | JsonValue[] | { [key: string]: JsonValue };

export type ProcessorName = "tailwind" | "markdown" | "security" | "minify" | string;
export type RuntimeMode = "compiled" | "interpreted";
export type CachePolicy = "ram" | "disk" | "auto";

export type AllowListRule =
  | {
      type: "feature";
      value: "all" | "camera" | "gps" | "indexeddb" | "localStorage" | string;
    }
  | { type: "js"; value: string }
  | { type: "css"; value: string }
  | { type: "url"; value: string }; // "*" allows all URLs

export interface CacheRef {
  namespace: string;
  key: string;
  version?: string;
}

export interface CompileInput {
  template: string;
  components?: Record<string, string>; // reusable .josieml fragments
  processors?: ProcessorName[]; // optional pipeline (tailwind/markdown/etc)
  allowList?: AllowListRule[];
  loadScripts?: string[]; // strict script allow-list for <script src="...">; optional pin: "url|sha384-..."
  runtimeMode?: RuntimeMode; // default: compiled
  cache?: CacheRef;
  cachePolicy?: CachePolicy;
}

export interface ProgramStep {
  id: string;
  type?: string;
  op?: string;
  from?: string;
  into?: string;
  args?: JsonValue[];
  do?: JsonValue;
  when?: JsonValue;
}

export interface ProgramMemo {
  id: string;
  deps: string[];
  runStep: string;
  into: string;
}

export interface ProgramEffect {
  id: string;
  deps: string[];
  runStep: string;
  immediate?: boolean;
  once?: boolean;
}

export interface ProgramContract {
  kind: "program";
  version: string;
  state?: Record<string, JsonValue>;
  actions?: Record<string, JsonValue>;
  resources?: string[];
  refs?: Record<string, JsonValue>;
  memos?: ProgramMemo[];
  effects?: ProgramEffect[];
  allowList?: AllowListRule[];
  loadScripts?: string[];
  steps: ProgramStep[];
}

export interface Diagnostic {
  level: "info" | "warn" | "error";
  code: string;
  message: string;
  file?: string;
  line?: number;
  column?: number;
}

export interface CompiledPage {
  kind: typeof JOSIE_WEB_KIND;
  version: typeof JOSIE_WEB_VERSION;
  fingerprint: string;
  cache?: CacheRef;
  diagnostics?: Diagnostic[];
}

export interface RenderInput {
  compiled: CompiledPage;
  vars: Record<string, JsonValue>; // backend variable mapping
}

export interface RenderOutput {
  html: string;
  diagnostics?: Diagnostic[];
}

export interface StaticSiteBuildInput {
  sourceDir: string;
  outputDir: string;
  pagesDir?: string; // default: "pages"
  componentsDir?: string; // default: "components"
  publicDir?: string; // default: "public"
  processors?: ProcessorName[];
  allowList?: AllowListRule[];
  loadScripts?: string[];
  runtimeMode?: RuntimeMode;
  cachePolicy?: CachePolicy;
  devHmr?: DevHmrConfig;
}

export interface StaticSiteBuildOutput {
  pagesBuilt: number;
  assetsCopied: number;
  generatedRoutes: string[];
}

export interface DevHmrConfig {
  enabled?: boolean; // default: true
  websocketUrl?: string; // default: ws://127.0.0.1:35729/josie-hmr
  versionPath?: string; // default: /__josie_hmr_version.txt
  pollIntervalMs?: number; // default: 1000
}

export interface StaticSiteWatchInput {
  build: StaticSiteBuildInput;
  watchPaths?: string[]; // relative to sourceDir
  pollIntervalMs?: number; // default: 500
}

export interface JosieWeb {
  compile(input: CompileInput): CompiledPage;
  render(input: RenderInput): RenderOutput;
  exportCompiled(compiled: CompiledPage, outputDir: string): void;
  buildStaticSite(input: StaticSiteBuildInput): StaticSiteBuildOutput;
  watchStaticSite(input: StaticSiteWatchInput): void;

  cacheGet(ref: CacheRef): CompiledPage | null;
  cachePut(ref: CacheRef, compiled: CompiledPage): void;
  cacheInvalidate(ref: { namespace: string; key?: string }): number;
}
