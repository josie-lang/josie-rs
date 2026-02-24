# Tailwind Parity Plan (josie-web)

Last reviewed: 2026-02-20  
Target reference: Tailwind CSS docs v4.2

## Official References

- https://tailwindcss.com/docs/utility-first
- https://tailwindcss.com/docs/hover-focus-and-other-states
- https://tailwindcss.com/docs/breakpoints
- https://tailwindcss.com/docs/dark-mode
- https://tailwindcss.com/docs/adding-custom-styles
- https://tailwindcss.com/docs/functions-and-directives
- https://tailwindcss.com/docs/detecting-classes-in-source-files

## Current josie-web Tailwind Engine (Implemented)

Code:
- `crates/josie-web/src/lib.rs` (`apply_tailwind_processor`)
- `crates/josie-web/src/tw.rs` (token compiler)

Current behavior:
- Compile utility tokens to CSS and inject `<style data-josie-tw>`.
- Supports a substantial utility subset (layout/spacing/colors/sizing/text/border/shadow/etc).
- Supports arbitrary values for known utility families (`[...]`) with safety filtering.
- Supports responsive variants: `sm`, `md`, `lg`, `xl`, `2xl`.
- Supports pseudo variants: `hover`, `focus`, `focus-visible`, `focus-within`, `active`, `disabled`, `last`.
- Supports `peer-disabled`.
- Minifies generated CSS.

## Gap vs Tailwind v4.2

Important: this is a parity gap, not a correctness bug list.

### 1. Variant Coverage Gap (High)

Tailwind v4.2 variant surface is much broader than current `variant_pseudo` + `variant_media_query`.

Missing/partial examples:
- Structural/state: `first`, `only`, `odd`, `even`, `visited`, `checked`, `required`, `invalid`, `enabled`, `empty`, etc.
- Group/peer: `group-*` family, most `peer-*` family (currently only `peer-disabled`).
- Color scheme/media: `dark`, `motion-safe`, `motion-reduce`, `contrast-more`, `contrast-less`, `print`, `forced-colors`, etc.
- Attribute selectors: `aria-*`, `data-*`, `open`, `inert`.
- Direction/environment: `rtl`, `ltr`, `portrait`, `landscape`.
- Range breakpoints variants like `max-*` combinations.
- Arbitrary variants (for example bracket selector variants) are not implemented.

### 2. Utility Coverage Gap (High)

Current utility resolver is explicit and finite. Tailwind's full utility surface is significantly larger.

Large missing/partial areas:
- Full filter/backdrop stacks and composition model.
- Full ring/outline/ring-offset model parity.
- Full gradient/color-mix behavior parity.
- Advanced transform composition parity.
- Full typography/prose ecosystem parity.
- Full tables/aspect-ratio/object/scroll/snap/interactivity coverage.
- Full animation/transition timing/easing presets and arbitrary variants.

### 3. Class Detection Gap (High)

Current token collection scans HTML classes and class-like values from program payload.
Tailwind's source detection model includes explicit source registration and safelisting patterns.

Missing/partial:
- Explicit source registration semantics parity.
- Pattern safelisting/blocklisting semantics.
- Robust detection for dynamic templating patterns.

### 4. Theme/Directive Gap (Medium)

Tailwind v4.2 docs include CSS-level directives/functions (`@theme`, `@utility`, `@variant`, etc.).
Current josie-web engine compiles tokens directly without this CSS directive layer.

This is a deliberate simplification today, but it is a parity gap.

## Breaking-Risk Notes (Current Engine)

- `apply_tailwind_processor` strips tailwind CDN script tags and replaces with local compiled CSS.
- Unsupported tokens currently produce diagnostic warnings and no CSS output for those tokens.
- This can produce visual drift when developers assume full Tailwind parity.

## Priority Backlog (Minimize Breakage First)

### P0 (Safety + Compatibility Guardrails)

1. Add strict compatibility modes:
- `tailwindMode = strict | lenient | fallback`
- `strict`: fail compile when unsupported token exists.
- `lenient`: current behavior + diagnostics.
- `fallback`: keep unsupported tokens and optionally preserve external Tailwind runtime path for development.

2. Add unsupported-token diagnostics detail:
- include exact token list (not just count),
- include first-seen file/fragment if available.

3. Add parity golden tests:
- variant matrix tests,
- arbitrary value tests,
- utility snapshot tests.

### P1 (Highest Impact Runtime Parity)

1. Implement broad variant expansion:
- `dark`, `group-*`, `peer-*`, `aria-*`, `data-*`, `max-*`, `print`, `motion-*`, `rtl/ltr`.

2. Implement utility families with highest break risk:
- ring/outline/ring-offset,
- transform/filter/backdrop composition,
- animation and transition completeness.

3. Improve class detection model:
- explicit source registration contract,
- safelist patterns.

### P2 (Fuller Tailwind Ecosystem)

1. Expand utility catalog toward full docs parity.
2. Add optional Tailwind directive compatibility layer (`@theme`-aligned mapping).
3. Add conformance benchmark suite against reference fixtures.

## Recommendation

If the goal is "as real Tailwind as possible" with minimal breakage, do P0 first, then P1 variants, then P1 utility families.  
This gives immediate developer safety while iteratively closing parity.
