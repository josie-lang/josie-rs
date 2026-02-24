# JosieML Writing Guide (Developer & LLM Friendly)

JosieML is designed for high-performance, reactive web pages. To keep your code stable and easy to maintain, follow these "First Principles."

---

## 1. Separate Logic from Markup
**Rule: The HTML should be a "view" into your State. Keep complex logic in the `<script>` block.**

### ❌ Don't: Write complex JSON trees in attributes
Writing logic inside strings is brittle and hard to debug.
```html
<button j-attr:class='["if", ["==", ["var", "client.count"], 0], "bg-gray", "bg-blue"]'>...</button>
```

### ✅ Do: Use descriptive State paths
Update the state in a `step` and let the HTML just point to it.
```html
<button j-attr:class="client.ui.counterClass">...</button>
```

---

## 2. Component Props Pattern
When creating reusable components (e.g., `Button`, `Input`), use this dual-prop pattern to support both static text and reactive data.

### The "Label vs LabelVar" Pattern
Inside your component:
```html
<!-- Button.josieml -->
<button @click="{{p.action}}">
  <span j-show="util.str_len('{{p.labelVar}}')" j-text="{{p.labelVar}}"></span>
  <span j-show="!util.str_len('{{p.labelVar}}')">{{p.label}}</span>
</button>
```

### Usage
```html
<!-- Static label -->
<j-component name="ui.Button" p-label="Submit" p-action="form.submit" />

<!-- Reactive label from state -->
<j-component name="ui.Button" p-labelVar="client.user.name" p-action="user.profile" />
```

---

## 3. Prevent the "Hydration Flash"
Because Josie initializes in the browser, elements that should be hidden might "flash" visible for a split second.

**Rule: Manually add `style="display: none;"` to any element controlled by `j-show` that starts hidden.**

```html
<!-- This ensures the app is hidden until the Auth check completes -->
<section j-show="client.ui.appVisible" style="display: none;">
  <h1>Welcome to the Manager</h1>
</section>
```

---

## 4. Real Josie JSON Syntax
Josie attributes expect **valid JSON arrays** for logic. Avoid "function-call" syntax like `if(a, b, c)`.

*   **Valid:** `j-attr:class='["if", ["==", "client.theme", "dark"], "bg-black", "bg-white"]'`
*   **Invalid:** `j-attr:class="if(==('client.theme', 'dark'), 'bg-black', 'bg-white')"`

---

## 5. Event Handling
Use the `@event` syntax to trigger **Actions**. Actions should usually point to a **Step ID**.

```json
"actions": {
  "button.click": { "runStep": "counter.increment" }
}
```

```html
<button @click="button.click">Increment</button>
```

---

## 6. CSS and Tailwind
Josie uses a compile-time Tailwind scanner.
1.  **Static Classes:** Use the standard `class="..."` attribute for static styling.
2.  **Dynamic Classes:** Use `j-attr:class="..."` for state-driven styles.
3.  **Naming:** Avoid generating class names dynamically (e.g., `util.concat('bg-', color)`). Tailwind needs the full class name string to exist in your file to include it in the CSS bundle.

---

## 7. Efficient Lists (Maps)
Use `data-josie-map` for loops. Always provide a `key` for better performance.

```html
<ul data-josie-map='{"source": "client.items", "key": "local.item.id"}'>
  <li j-text="local.item.name"></li>
</ul>
```
*   `local.item`: Access the current iteration object.
*   `local.index`: Access the current index.
