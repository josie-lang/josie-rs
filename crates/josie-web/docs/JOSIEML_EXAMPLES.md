# JOSIEML Examples

Runnable example:
- `crates/josie-web/examples/program.josieml`
- `crates/josie-web/examples/todo_app.rs`

## A. Page With Multiple Actions + Steps

```html
<main>
  <h1 j-text="client.page.title"></h1>
  <button @click="counter.inc">Count: <span j-text="client.count"></span></button>
  <button @click="toast.close">Close Toast</button>

  <script type="application/josie+json">
  {
    "kind": "program",
    "version": "0.1.0",
    "state": {
      "client": {
        "page": { "title": "TigerGun" },
        "count": 0,
        "toast": {
          "class": "visible",
          "hideClass": "hidden"
        }
      }
    },
    "actions": {
      "counter.inc": { "runStep": "counter.inc" },
      "toast.close": { "runStep": "toast.close" }
    },
    "steps": [
      {
        "id": "counter.inc",
        "op": "set",
        "into": "client.count",
        "args": [["+", ["var", "client.count"], 1]]
      },
      {
        "id": "toast.close",
        "op": "set",
        "into": "client.toast.class",
        "args": [["var", "client.toast.hideClass"]]
      }
    ]
  }
  </script>
</main>
```

## B. Equivalent `.josie` Program (no HTML)

```json
{
  "kind": "program",
  "version": "0.1.0",
  "state": {
    "client": { "count": 0 }
  },
  "actions": {
    "counter.inc": { "runStep": "counter.inc" }
  },
  "steps": [
    {
      "id": "counter.inc",
      "op": "set",
      "into": "client.count",
      "args": [["+", ["var", "client.count"], 1]]
    }
  ]
}
```

## C. Runtime Mode Switch

Compiled mode (default):

```bash
cargo run -q -p josie-web --example todo_app
```

Interpreted mode:

```bash
JOSIE_WEB_RUNTIME_MODE=interpreted cargo run -q -p josie-web --example todo_app
```

## D. Markdown Block Example

```html
<section class="prose prose-invert max-w-none">
  <j-md>
# Hello

- fast
- simple

This is **bold** and `inline code`.
  </j-md>
</section>
```

When `markdown` processor is enabled, the `<j-md>` block is compiled to normal HTML.

## E. Markdown From Backend Vars (DB Content)

```html
<article class="prose prose-invert text-lg" j-md-var="server.post.body_md"></article>
```

Render vars example:

```json
{
  "server": {
    "post": {
      "body_md": "# Hello\n\n[Docs](https://docs.example.com)"
    }
  }
}
```

URL allow-list:

```json
{
  "allowList": [
    { "type": "url", "value": "*" }
  ]
}
```

## F. Three.js Script Allow-List

```html
<head>
  <script defer src="https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js"></script>
</head>
```

Compile input:

```json
{
  "processors": ["security", "tailwind"],
  "loadScripts": [
    "https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js"
  ],
  "allowList": [
    { "type": "js", "value": "https://cdn.jsdelivr.net/" }
  ]
}
```

Behavior:
- listed Three.js script is kept
- inline scripts are removed
- unlisted script URLs are removed
