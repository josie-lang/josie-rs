# Blessed Libraries & JS Primitives

This document defines the "LLM-Impossible" strategy for the Josie Web platform. We only adapt external JavaScript libraries that handle deep browser edge cases, GPU state management, or complex mathematical projections that are too difficult for high-level LLMs to maintain reliably.

## The Strategy
- **Core Sterility**: The default Josie Web environment remains sterile and Josie-only.
- **Controlled Escape Hatches**: Complex engines are wrapped into declarative JosieML components or pure functional operators.
- **Version Freezing**: All blessed libraries are version-frozen and vendored to ensure stability and security.

## 1. Heavy Graphics & Geospatial
These power the high-performance visual surface of the Josie platform.

*   **Three.js + pixiv/three-vrm**: 
    *   **Purpose**: 3D animations, interactive complex charts, and VRM support.
    *   **JosieML Component**: `<j-3d>`
    *   **Primary Use**: AI companions (VRM) and interactive 3D visualizations.
*   **Deck.gl + Maplibre**:
    *   **Purpose**: Industrial-grade interactive geospatial visualization and vector tiling.
    *   **JosieML Component**: `<j-map>`
    *   **Primary Use**: Massive spatial datasets and map-based management systems.

## 2. Infinite Canvas & Graph Design
*   **Konva.js**:
    *   **Purpose**: Mimicking Miro-style canvases with high-performance 2D manipulation.
    *   **Reactivity Model**: The underlying data model is a pure JSON graph (`nodes` and `edges`) in Josie state. This ensures the design is 100% LLM-readable, while the Konva wrapper handles efficient rendering and event propagation.
    *   **JosieML Component**: `<j-design>`

## 3. Input & Authoring Engines
Replicating a bug-free cursor, selection, and history system across all browsers is nearly impossible for LLMs.

*   **ProseMirror & CodeMirror 6**:
    *   **Purpose**: Modular engines for rich text and code editing.
    *   **JosieML Component**: `<j-editor type="rich|code">`
    *   **Primary Use**: Documentation, sticky notes, and IDE-like experiences.

## 4. Industrial Utilities
*   **SortableJS**:
    *   **Purpose**: Flicker-free, touch-compatible drag-and-drop for lists and boards.
    *   **JosieML Directive**: `@sort` and `@drag`.
*   **Lucide Icons**:
    *   **Purpose**: A clean, consistent SVG icon set.
    *   **JosieML Component**: `<j-icon>`

## 5. Spatiotemporal Math (`stmath`)
A custom, unified math library providing a bit-for-bit identical API between **Rust (Server)** and **JS (Client)**. This logic is implemented as "Standard Utilities" in `josie-core`.

*   **Composition**: A curated subset of **Turf.js** (spatial), **Day.js** (temporal), and **Math.js** (complex logic).
*   **Namespaces**: 
    *   `u.time.*` (Temporal): `now`, `format`, `add`, `diff`.
    *   `u.space.*` (Spatial): `dist`, `inside`, `bbox`.
    *   `u.math.*` (Math): `clamp`, `lerp`, `map`, `round`.
*   **Constraint**: Every operation must return identical results on both backend and frontend to ensure pipeline determinism.

## 6. Native Animation & Motion
*   **Tween Engine**: Built as a first-class citizen directly into `josie-runtime.js`.
*   **Logic**: High-performance interpolation for Josie state changes, triggered by the `j-animate` attribute. This avoids the overhead of external heavy animation libraries like GSAP for common UI transitions.

---

## Josie Web Capability Summary

| Component / Op | Library Source | Primary Role |
| :--- | :--- | :--- |
| `<j-3d>` | Three.js / VRM | AI Avatars & 3D Visualization |
| `<j-design>` | Konva.js | Miro-style Canvas & Graph Design |
| `<j-map>` | Deck.gl / Maplibre | Heavy Geospatial Mapping |
| `<j-editor>` | ProseMirror / CodeMirror | Professional Text/Code Input |
| `x.st.*` | `stmath` (Custom) | Uniform Spatial/Temporal Logic |
| `j-animate` | Native Runtime | System-wide Motion & Transitions |
| `<j-icon>` | Lucide | UI System Symbols |
