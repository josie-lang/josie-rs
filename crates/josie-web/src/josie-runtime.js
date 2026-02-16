/*
 * JOSIE Runtime Notes
 *
 * Goal:
 * - Keep SSR HTML authoritative.
 * - Attach reactivity/events during hydration.
 * - Preserve lexical scope for list items (`map` / `item`) on rerender.
 *
 * Stability rules:
 * - `discover()` hydrates with global scope only at app boot.
 * - `bindMap()` rerenders list rows and hydrates each row with item scope.
 * - `hydrateScopeTree()` avoids cross-hydrating into nested foreign map roots.
 *
 * Practical consequence:
 * - Counter-like bindings update fast.
 * - Map item bindings remain correct after updates (no lost `item` scope).
 *
 * Packaging note:
 * - This file is intentionally standalone and dependency-free so it can be
 *   shipped inline from Rust SSR output.
 */
(function (window) {
  'use strict';

  const JOSIE = {
    state: {},
    theme: {},
    fns: {},
    operators: {},
    subscribers: {},
    hydrated: new WeakSet(),

    init(state, theme) {
      this.state = state || {};
      this.theme = theme || {};
      this.fns = {};
      this.registerOperators();
      this.discover(document.body);
    },

    registerOperators() {
      this.operators = {
        var: (args, scope) => this.opVar(args, scope),
        set: (args, scope) => this.opSet(args, scope),
        '+': (args, scope) => this.opAdd(args, scope),
        '-': (args, scope) => this.opSub(args, scope),
        '*': (args, scope) => this.opMul(args, scope),
        '/': (args, scope) => this.opDiv(args, scope),
        '%': (args, scope) => this.opMod(args, scope),
        '==': (args, scope) => this.opEq(args, scope),
        '!=': (args, scope) => this.opNeq(args, scope),
        '>': (args, scope) => this.opGt(args, scope),
        '<': (args, scope) => this.opLt(args, scope),
        '>=': (args, scope) => this.opGte(args, scope),
        '<=': (args, scope) => this.opLte(args, scope),
        if: (args, scope) => this.opIf(args, scope),
        '&&': (args, scope) => this.opAnd(args, scope),
        '||': (args, scope) => this.opOr(args, scope),
        '!': (args, scope) => this.opNot(args, scope),
        'u.concat': (args, scope) => this.opConcat(args, scope),
        'u.lower': (args, scope) => this.opLower(args, scope),
        'u.upper': (args, scope) => this.opUpper(args, scope),
        'u.contains': (args, scope) => this.opContains(args, scope),
        'u.template': (args, scope) => this.opTemplate(args, scope),
        'u.to_int': (args, scope) => this.opToInt(args, scope),
        'u.to_float': (args, scope) => this.opToFloat(args, scope),
        'u.to_string': (args, scope) => this.opToString(args, scope),
        'u.trim': (args, scope) => this.opTrim(args, scope),
        'u.str_len': (args, scope) => this.opStrLen(args, scope),
        do: (args, scope) => this.opDo(args, scope),
        pipe: (args, scope) => this.opPipe(args, scope),
        def: (args, scope) => this.opDef(args, scope),
        call: (args, scope) => this.opCall(args, scope),
        map: (args, scope) => this.opMap(args, scope),
        filter: (args, scope) => this.opFilter(args, scope),
        match: (args, scope) => this.opMatch(args, scope),
        'w.event.value': (args, scope) => this.opEventValue(args, scope),
        'w.event.key': (args, scope) => this.opEventKey(args, scope),
        'w.event.prevent': (args, scope) => this.opEventPrevent(args, scope),
        log: (args, scope) => this.opLog(args, scope),
        effect: (args, scope) => this.opEffect(args, scope),
        len: (args, scope) => this.opLen(args, scope),
        push: (args, scope) => this.opPush(args, scope),
        get: (args, scope) => this.opGet(args, scope),
      };
    },

    evaluate(node, scope) {
      const evalScope = scope || {};
      if (node === null || node === undefined) return null;
      if (typeof node !== 'object') return node;
      if (!Array.isArray(node)) return node;
      if (node.length === 0) return null;

      const op = node[0];
      if (typeof op === 'string' && this.operators[op]) {
        return this.operators[op](node.slice(1), evalScope);
      }

      const out = [];
      for (const item of node) {
        out.push(this.evaluate(item, evalScope));
      }
      return out;
    },

    truthy(value) {
      if (value === null || value === undefined) return false;
      if (typeof value === 'boolean') return value;
      if (typeof value === 'number') return value !== 0;
      if (typeof value === 'string') return value.length > 0;
      if (Array.isArray(value)) return value.length > 0;
      if (typeof value === 'object') return Object.keys(value).length > 0;
      return Boolean(value);
    },

    resolvePath(path, scope) {
      if (typeof path !== 'string') return null;

      const locals = scope.locals || {};
      let parts;
      let current;

      if (path.startsWith('client.')) {
        parts = path.slice('client.'.length).split('.');
        current = this.state;
      } else if (path.startsWith('local.')) {
        parts = path.slice('local.'.length).split('.');
        current = locals;
      } else {
        parts = path.split('.');
        const root = parts[0];
        if (root === 'server') return null;
        if (Object.prototype.hasOwnProperty.call(locals, root)) {
          current = locals[root];
          parts = parts.slice(1);
        } else {
          current = this.state;
        }
      }

      for (const part of parts) {
        if (current === null || current === undefined) return null;
        current = current[part];
      }
      return current === undefined ? null : current;
    },

    normalizeClientPath(path) {
      if (typeof path !== 'string') return null;
      if (path.startsWith('client.')) return path;
      if (path.startsWith('server.') || path.startsWith('local.') || path.startsWith('w.event.')) {
        return null;
      }
      return 'client.' + path;
    },

    writeClientPath(path, value) {
      const normalized = this.normalizeClientPath(path);
      if (!normalized) return;

      const parts = normalized.slice('client.'.length).split('.');
      let current = this.state;
      for (let i = 0; i < parts.length - 1; i += 1) {
        const part = parts[i];
        if (!current[part] || typeof current[part] !== 'object' || Array.isArray(current[part])) {
          current[part] = {};
        }
        current = current[part];
      }

      current[parts[parts.length - 1]] = value;
      this.notify(normalized);
    },

    opVar(args, scope) {
      if (args.length === 0) return null;
      const path = this.evaluate(args[0], scope);
      return this.resolvePath(path, scope);
    },

    opSet(args, scope) {
      if (args.length < 2) return null;
      const path = this.evaluate(args[0], scope);
      const value = this.evaluate(args[1], scope);
      this.writeClientPath(path, value);
      return value;
    },

    opAdd(args, scope) {
      return args.reduce((sum, arg) => sum + Number(this.evaluate(arg, scope) || 0), 0);
    },

    opSub(args, scope) {
      if (args.length === 0) return 0;
      const first = Number(this.evaluate(args[0], scope) || 0);
      if (args.length === 1) return -first;
      const second = Number(this.evaluate(args[1], scope) || 0);
      return first - second;
    },

    opMul(args, scope) {
      return args.reduce((acc, arg) => acc * Number(this.evaluate(arg, scope) || 0), 1);
    },

    opDiv(args, scope) {
      if (args.length < 2) return 0;
      const a = Number(this.evaluate(args[0], scope) || 0);
      const b = Number(this.evaluate(args[1], scope) || 0);
      return b === 0 ? 0 : a / b;
    },

    opMod(args, scope) {
      if (args.length < 2) return 0;
      const a = Number(this.evaluate(args[0], scope) || 0);
      const b = Number(this.evaluate(args[1], scope) || 0);
      return b === 0 ? 0 : a % b;
    },

    opEq(args, scope) {
      if (args.length < 2) return false;
      return this.evaluate(args[0], scope) === this.evaluate(args[1], scope);
    },

    opNeq(args, scope) {
      if (args.length < 2) return true;
      return this.evaluate(args[0], scope) !== this.evaluate(args[1], scope);
    },

    opGt(args, scope) {
      if (args.length < 2) return false;
      return this.evaluate(args[0], scope) > this.evaluate(args[1], scope);
    },

    opLt(args, scope) {
      if (args.length < 2) return false;
      return this.evaluate(args[0], scope) < this.evaluate(args[1], scope);
    },

    opGte(args, scope) {
      if (args.length < 2) return false;
      return this.evaluate(args[0], scope) >= this.evaluate(args[1], scope);
    },

    opLte(args, scope) {
      if (args.length < 2) return false;
      return this.evaluate(args[0], scope) <= this.evaluate(args[1], scope);
    },

    opIf(args, scope) {
      if (args.length === 0) return null;
      if (this.truthy(this.evaluate(args[0], scope))) {
        return args.length > 1 ? this.evaluate(args[1], scope) : null;
      }
      return args.length > 2 ? this.evaluate(args[2], scope) : null;
    },

    opAnd(args, scope) {
      let last = true;
      for (const arg of args) {
        last = this.evaluate(arg, scope);
        if (!this.truthy(last)) return false;
      }
      return last;
    },

    opOr(args, scope) {
      for (const arg of args) {
        const value = this.evaluate(arg, scope);
        if (this.truthy(value)) return value;
      }
      return false;
    },

    opNot(args, scope) {
      if (args.length === 0) return true;
      return !this.truthy(this.evaluate(args[0], scope));
    },

    opConcat(args, scope) {
      return args.map((arg) => String(this.evaluate(arg, scope) ?? '')).join('');
    },

    opLower(args, scope) {
      if (args.length === 0) return '';
      return String(this.evaluate(args[0], scope) ?? '').toLowerCase();
    },

    opUpper(args, scope) {
      if (args.length === 0) return '';
      return String(this.evaluate(args[0], scope) ?? '').toUpperCase();
    },

    opContains(args, scope) {
      if (args.length < 2) return false;
      const haystack = this.evaluate(args[0], scope);
      const needle = this.evaluate(args[1], scope);
      if (typeof haystack === 'string') return haystack.includes(String(needle));
      if (Array.isArray(haystack)) return haystack.includes(needle);
      return false;
    },

    opTemplate(args, scope) {
      if (args.length === 0) return '';
      let out = String(this.evaluate(args[0], scope) ?? '');
      for (let i = 1; i < args.length; i += 1) {
        if (!out.includes('{}')) break;
        out = out.replace('{}', String(this.evaluate(args[i], scope) ?? ''));
      }
      return out;
    },

    opToInt(args, scope) {
      if (args.length === 0) return 0;
      const value = this.evaluate(args[0], scope);
      const n = Number.parseInt(String(value ?? '0'), 10);
      return Number.isFinite(n) ? n : 0;
    },

    opToFloat(args, scope) {
      if (args.length === 0) return 0;
      const value = this.evaluate(args[0], scope);
      const n = Number.parseFloat(String(value ?? '0'));
      return Number.isFinite(n) ? n : 0;
    },

    opToString(args, scope) {
      if (args.length === 0) return '';
      const value = this.evaluate(args[0], scope);
      return value == null ? '' : String(value);
    },

    opTrim(args, scope) {
      if (args.length === 0) return '';
      return String(this.evaluate(args[0], scope) ?? '').trim();
    },

    opStrLen(args, scope) {
      if (args.length === 0) return 0;
      return String(this.evaluate(args[0], scope) ?? '').length;
    },

    opDo(args, scope) {
      let result = null;
      for (const arg of args) {
        result = this.evaluate(arg, scope);
      }
      return result;
    },

    opPipe(args, scope) {
      const hadPipe = Object.prototype.hasOwnProperty.call(this.state, 'pipe');
      const oldPipe = this.state.pipe;
      let result = null;
      for (const arg of args) {
        this.state.pipe = { prev: result };
        result = this.evaluate(arg, scope);
      }
      if (hadPipe) {
        this.state.pipe = oldPipe;
      } else {
        delete this.state.pipe;
      }
      return result;
    },

    opDef(args, _scope) {
      if (args.length < 3) return null;
      const name = typeof args[0] === 'string' ? args[0] : null;
      if (!name) return null;
      const rawParams = Array.isArray(args[1]) ? args[1] : [];
      const params = rawParams.filter((p) => typeof p === 'string');
      const body = args[2];
      this.fns[name] = { params, body };
      return null;
    },

    opCall(args, scope) {
      if (args.length === 0) return null;
      const nameVal = this.evaluate(args[0], scope);
      if (typeof nameVal !== 'string' || nameVal.length === 0) return null;

      const def = this.fns[nameVal];
      if (def) {
        const callLocals = Object.assign({}, scope.locals || {});
        for (let i = 0; i < def.params.length; i += 1) {
          const expr = args[i + 1];
          callLocals[def.params[i]] = expr === undefined ? null : this.evaluate(expr, scope);
        }
        return this.evaluate(def.body, Object.assign({}, scope, { locals: callLocals }));
      }

      if (nameVal !== 'call' && this.operators[nameVal]) {
        return this.operators[nameVal](args.slice(1), scope);
      }

      return null;
    },

    opMap(args, scope) {
      if (args.length < 2) return [];
      const list = this.evaluate(args[0], scope);
      if (!Array.isArray(list)) return [];
      const template = args[1];
      return list.map((item, index) => {
        const locals = Object.assign({}, scope.locals || {}, { item, index });
        return this.evaluate(template, Object.assign({}, scope, { locals }));
      });
    },

    opFilter(args, scope) {
      if (args.length < 2) return [];
      const list = this.evaluate(args[0], scope);
      if (!Array.isArray(list)) return [];
      const predicate = args[1];
      const out = [];
      for (let index = 0; index < list.length; index += 1) {
        const item = list[index];
        const locals = Object.assign({}, scope.locals || {}, { item, index });
        if (this.truthy(this.evaluate(predicate, Object.assign({}, scope, { locals })))) {
          out.push(item);
        }
      }
      return out;
    },

    opMatch(args, scope) {
      if (args.length < 3) return null;
      const value = this.evaluate(args[0], scope);
      for (let i = 1; i < args.length; i += 2) {
        const pattern = args[i];
        const result = args[i + 1];
        if (result === undefined) break;
        if (pattern === '_' || pattern === '"_"') return this.evaluate(result, scope);
        const patternValue = this.evaluate(pattern, scope);
        if (patternValue === '_' || patternValue === value) {
          return this.evaluate(result, scope);
        }
      }
      return null;
    },

    opEventValue(_args, scope) {
      return scope.event ? scope.event.value : null;
    },

    opEventKey(_args, scope) {
      return scope.event ? scope.event.key : null;
    },

    opEventPrevent(_args, scope) {
      if (scope.event && scope.event.raw && typeof scope.event.raw.preventDefault === 'function') {
        scope.event.raw.preventDefault();
        return true;
      }
      return false;
    },

    opLog(args, scope) {
      const values = args.map((arg) => this.evaluate(arg, scope));
      if (typeof console !== 'undefined' && console.log) {
        console.log('[JOSIE]', ...values);
      }
      return values.length > 0 ? values[0] : null;
    },

    opEffect(args, scope) {
      return this.opDo(args, scope);
    },

    opLen(args, scope) {
      if (args.length === 0) return 0;
      const value = this.evaluate(args[0], scope);
      if (Array.isArray(value) || typeof value === 'string') return value.length;
      if (value && typeof value === 'object') return Object.keys(value).length;
      return 0;
    },

    opPush(args, scope) {
      if (args.length < 2) return [];
      const list = this.evaluate(args[0], scope);
      const item = this.evaluate(args[1], scope);
      if (!Array.isArray(list)) return [item];
      return list.concat([item]);
    },

    opGet(args, scope) {
      if (args.length < 2) return null;
      const collection = this.evaluate(args[0], scope);
      const key = this.evaluate(args[1], scope);
      if (Array.isArray(collection)) {
        const index = Number(key);
        return Number.isInteger(index) ? collection[index] ?? null : null;
      }
      if (collection && typeof collection === 'object') {
        const mapKey = String(key);
        return Object.prototype.hasOwnProperty.call(collection, mapKey)
          ? collection[mapKey]
          : null;
      }
      return null;
    },

    subscribe(path, callback) {
      if (!this.subscribers[path]) this.subscribers[path] = [];
      this.subscribers[path].push(callback);
    },

    notify(path) {
      const exact = this.subscribers[path] || [];
      for (const callback of exact) callback();

      const prefixes = path.split('.');
      for (let i = prefixes.length - 1; i > 0; i -= 1) {
        const parent = prefixes.slice(0, i).join('.');
        const parentSubs = this.subscribers[parent] || [];
        for (const callback of parentSubs) callback();
      }
    },

    extractPaths(node, set) {
      const out = set || new Set();
      if (!Array.isArray(node)) return out;

      if (node[0] === 'var' && typeof node[1] === 'string') {
        const path = node[1];
        if (path === 'item' || path.startsWith('item.')) return out;
        if (path === 'parentItem' || path.startsWith('parentItem.')) return out;
        if (path.startsWith('local.') || path.startsWith('server.') || path.startsWith('w.event.')) {
          return out;
        }
        const normalized = this.normalizeClientPath(path);
        if (normalized) out.add(normalized);
      }

      for (const item of node) {
        if (Array.isArray(item)) this.extractPaths(item, out);
      }

      return out;
    },

    parseJsonAttribute(element, attr) {
      const raw = element.getAttribute(attr);
      if (!raw) return null;
      try {
        return JSON.parse(raw);
      } catch (_e) {
        return null;
      }
    },

    parseBindMap(element) {
      const obj = this.parseJsonAttribute(element, 'data-josie-binds');
      if (obj && typeof obj === 'object' && !Array.isArray(obj)) return obj;

      const legacyProp = element.getAttribute('data-josie-bind');
      const legacyExpr = this.parseJsonAttribute(element, 'data-josie-expr');
      if (legacyProp && legacyExpr !== null) {
        const map = {};
        map[legacyProp] = legacyExpr;
        return map;
      }
      return null;
    },

    parseEventMap(element) {
      const obj = this.parseJsonAttribute(element, 'data-josie-ons');
      if (obj && typeof obj === 'object' && !Array.isArray(obj)) return obj;

      const legacyEvent = element.getAttribute('data-josie-on');
      const legacyHandler = this.parseJsonAttribute(element, 'data-josie-handler');
      if (legacyEvent && legacyHandler !== null) {
        const map = {};
        map[legacyEvent] = legacyHandler;
        return map;
      }
      return null;
    },

    applyProp(element, prop, value) {
      if (prop === 'children' || prop === 'textContent') {
        element.textContent = value == null ? '' : String(value);
        return;
      }
      if (prop === 'value') {
        const next = value == null ? '' : String(value);
        if (element.value !== next) element.value = next;
        return;
      }
      if (prop === 'hidden' || prop === 'disabled') {
        element[prop] = Boolean(value);
        return;
      }
      if (prop === 'className') {
        element.className = value == null ? '' : String(value);
        return;
      }

      if (value === null || value === undefined || value === false) {
        element.removeAttribute(prop);
      } else {
        element.setAttribute(prop, String(value));
      }
    },

    bindProperty(element, prop, expr, scope) {
      const update = () => {
        const value = this.evaluate(expr, scope);
        this.applyProp(element, prop, value);
      };

      const deps = Array.from(this.extractPaths(expr));
      for (const path of deps) this.subscribe(path, update);
      update();
    },

    bindEvent(element, eventName, expr, scope) {
      element.addEventListener(eventName, (event) => {
        const eventScope = Object.assign({}, scope, {
          event: {
            value: event && event.target && 'value' in event.target ? event.target.value : null,
            key: event && event.key ? event.key : null,
            raw: event,
          },
        });
        this.evaluate(expr, eventScope);
      });
    },

    bindMap(element, mapConfig, scope) {
      const render = () => {
        const list = this.evaluate(mapConfig.source, scope);
        const items = Array.isArray(list) ? list : [];

        element.innerHTML = '';

        for (const item of items) {
          const itemScope = Object.assign({}, scope, {
            locals: Object.assign({}, scope.locals || {}, {
              parentItem: scope.locals ? scope.locals.item : null,
              item,
            }),
          });

          if (mapConfig.filter && !this.truthy(this.evaluate(mapConfig.filter, itemScope))) {
            continue;
          }

          const child = this.renderNode(mapConfig.each, itemScope);
          if (child) {
            element.appendChild(child);
            this.hydrateScopeTree(child, itemScope);
          }
        }
      };

      const depSet = new Set();
      if (mapConfig.source) this.extractPaths(mapConfig.source, depSet);
      if (mapConfig.filter) this.extractPaths(mapConfig.filter, depSet);
      if (mapConfig.each) this.extractNodePaths(mapConfig.each, depSet);

      for (const path of depSet) this.subscribe(path, render);
      render();
    },

    extractNodePaths(node, depSet) {
      if (!node || typeof node !== 'object') return depSet;

      if (node.bind && typeof node.bind === 'object') {
        for (const key of Object.keys(node.bind)) {
          this.extractPaths(node.bind[key], depSet);
        }
      }

      if (node.on && typeof node.on === 'object') {
        for (const key of Object.keys(node.on)) {
          this.extractPaths(node.on[key], depSet);
        }
      }

      if (node.type === 'map') {
        if (node.source) this.extractPaths(node.source, depSet);
        if (node.filter) this.extractPaths(node.filter, depSet);
        if (node.each) this.extractNodePaths(node.each, depSet);
      }

      if (Array.isArray(node.children)) {
        for (const child of node.children) this.extractNodePaths(child, depSet);
      }

      return depSet;
    },

    styleValue(key, value) {
      const colors = (this.theme && this.theme.colors) || {};
      const radius = (this.theme && this.theme.radius) || [];
      const fontSize =
        (this.theme && (this.theme.font_size || this.theme.fontSize)) || {};
      const shadows = (this.theme && this.theme.shadows) || {};
      const transitions = (this.theme && this.theme.transitions) || {};

      if (key === 'bg') return colors[value] || value;
      if (key === 'fg') return colors[value] || value;
      if (key === 'fontSize') {
        if (typeof value === 'string' && Object.prototype.hasOwnProperty.call(fontSize, value)) {
          return String(fontSize[value]) + 'px';
        }
      }
      if (key === 'radius' && typeof value === 'number') {
        const r = radius[value];
        if (typeof r === 'number') return String(r) + 'px';
      }
      if (key === 'shadow') return shadows[value] || value;
      if (key === 'transition') return transitions[value] || value;
      if (key === 'border') {
        if (typeof value === 'string' && colors[value]) return '1px solid ' + colors[value];
        return value;
      }
      return value;
    },

    spacePx(value) {
      const space = (this.theme && this.theme.space) || [];
      if (typeof value === 'number') {
        if (space[value] !== undefined) return String(space[value]) + 'px';
        return String(value) + 'px';
      }
      return value == null ? '' : String(value);
    },

    applyNodeStyle(element, node) {
      if (!node || !node.style || typeof node.style !== 'object') return;
      const style = node.style;

      if (style.bg !== undefined) element.style.backgroundColor = this.styleValue('bg', style.bg);
      if (style.fg !== undefined) element.style.color = this.styleValue('fg', style.fg);
      if (style.fontSize !== undefined) element.style.fontSize = this.styleValue('fontSize', style.fontSize);
      if (style.fontWeight !== undefined) element.style.fontWeight = String(style.fontWeight);
      if (style.shadow !== undefined) element.style.boxShadow = this.styleValue('shadow', style.shadow);
      if (style.border !== undefined) element.style.border = this.styleValue('border', style.border);
      if (style.transition !== undefined) element.style.transition = this.styleValue('transition', style.transition);
      if (style.maxWidth !== undefined) {
        element.style.maxWidth = typeof style.maxWidth === 'number' ? style.maxWidth + 'px' : String(style.maxWidth);
      }
      if (style.width !== undefined) {
        element.style.width = typeof style.width === 'number' ? style.width + 'px' : String(style.width);
      }
      if (style.height !== undefined) {
        element.style.height = typeof style.height === 'number' ? style.height + 'px' : String(style.height);
      }
      if (style.radius !== undefined) element.style.borderRadius = this.styleValue('radius', style.radius);
      if (style.opacity !== undefined) element.style.opacity = String(style.opacity);
      if (style.minHeight !== undefined) {
        element.style.minHeight = typeof style.minHeight === 'number' ? style.minHeight + 'px' : String(style.minHeight);
      }
      if (style.textAlign !== undefined) element.style.textAlign = String(style.textAlign);
      if (style.display !== undefined) element.style.display = String(style.display);
      if (style.position !== undefined) element.style.position = String(style.position);
      if (style.z !== undefined) element.style.zIndex = String(style.z);
      if (style.cursor !== undefined) element.style.cursor = String(style.cursor);
      if (style.tracking !== undefined) element.style.letterSpacing = String(style.tracking);
      if (style.leading !== undefined) element.style.lineHeight = String(style.leading);
      if (style.transform !== undefined) element.style.textTransform = String(style.transform);
      if (style.aspect !== undefined) element.style.aspectRatio = String(style.aspect);
      if (style.overflow !== undefined) element.style.overflow = String(style.overflow);
      if (style.flex !== undefined) element.style.flex = String(style.flex);
      if (style.p !== undefined) element.style.padding = this.spacePx(style.p);
      if (style.px !== undefined) {
        const v = this.spacePx(style.px);
        element.style.paddingLeft = v;
        element.style.paddingRight = v;
      }
      if (style.py !== undefined) {
        const v = this.spacePx(style.py);
        element.style.paddingTop = v;
        element.style.paddingBottom = v;
      }
      if (style.pt !== undefined) element.style.paddingTop = this.spacePx(style.pt);
      if (style.pr !== undefined) element.style.paddingRight = this.spacePx(style.pr);
      if (style.pb !== undefined) element.style.paddingBottom = this.spacePx(style.pb);
      if (style.pl !== undefined) element.style.paddingLeft = this.spacePx(style.pl);
      if (style.m !== undefined) element.style.margin = this.spacePx(style.m);
      if (style.mx !== undefined) {
        const v = this.spacePx(style.mx);
        element.style.marginLeft = v;
        element.style.marginRight = v;
      }
      if (style.my !== undefined) {
        const v = this.spacePx(style.my);
        element.style.marginTop = v;
        element.style.marginBottom = v;
      }
      if (style.mt !== undefined) element.style.marginTop = this.spacePx(style.mt);
      if (style.mr !== undefined) element.style.marginRight = this.spacePx(style.mr);
      if (style.mb !== undefined) element.style.marginBottom = this.spacePx(style.mb);
      if (style.ml !== undefined) element.style.marginLeft = this.spacePx(style.ml);
      if (style.gap !== undefined) element.style.gap = this.spacePx(style.gap);
      if (style.hidden === true) element.style.display = 'none';
    },

    renderNode(node, scope) {
      if (!node || typeof node !== 'object') return null;
      if (Array.isArray(node)) {
        const value = this.evaluate(node, scope);
        return document.createTextNode(value == null ? '' : String(value));
      }

      let el;
      switch (node.type) {
        case 'Text':
          el = document.createElement('span');
          break;
        case 'Button':
          el = document.createElement('button');
          break;
        case 'Input':
          el = document.createElement('input');
          break;
        case 'Grid':
        case 'Stack':
        case 'Fragment':
        default:
          el = document.createElement('div');
          break;
      }

      if (node.props && typeof node.props === 'object') {
        if (node.type === 'Stack') {
          el.style.display = 'flex';
          el.style.flexDirection = node.props.direction ? String(node.props.direction) : 'row';
          if (node.props.align) el.style.alignItems = String(node.props.align);
          if (node.props.justify) el.style.justifyContent = String(node.props.justify);
          if (typeof node.props.gap === 'number') {
            const space = this.theme && Array.isArray(this.theme.space) ? this.theme.space : [];
            const px = space[node.props.gap] !== undefined ? space[node.props.gap] : 8;
            el.style.gap = String(px) + 'px';
          }
        }
        if (node.type === 'Grid') {
          el.style.display = 'grid';
          if (node.props.cols) el.style.gridTemplateColumns = 'repeat(' + String(node.props.cols) + ', 1fr)';
          if (typeof node.props.gap === 'number') {
            const space = this.theme && Array.isArray(this.theme.space) ? this.theme.space : [];
            const px = space[node.props.gap] !== undefined ? space[node.props.gap] : 8;
            el.style.gap = String(px) + 'px';
          }
        }
        if (node.type === 'Text' && node.props.children !== undefined) {
          const text = Array.isArray(node.props.children)
            ? this.evaluate(node.props.children, scope)
            : node.props.children;
          el.textContent = text == null ? '' : String(text);
        }
        if (node.type === 'Button' && node.props.children !== undefined) {
          el.textContent = String(node.props.children);
        }
        if (node.type === 'Input') {
          if (node.props.placeholder !== undefined) el.setAttribute('placeholder', String(node.props.placeholder));
          if (node.props.value !== undefined) el.value = String(node.props.value);
        }
        if (node.props.className !== undefined) {
          el.className = String(node.props.className);
        }
      }

      if (node.bind && typeof node.bind === 'object') {
        el.setAttribute('data-josie-binds', JSON.stringify(node.bind));
      }
      if (node.on && typeof node.on === 'object') {
        el.setAttribute('data-josie-ons', JSON.stringify(node.on));
      }

      if (node.type === 'map') {
        const mapConfig = {
          source: node.source,
          filter: node.filter,
          each: node.each,
        };
        el.setAttribute('data-josie-map', JSON.stringify(mapConfig));
      }

      this.applyNodeStyle(el, node);

      if (Array.isArray(node.children)) {
        for (const child of node.children) {
          const childEl = this.renderNode(child, scope);
          if (childEl) el.appendChild(childEl);
        }
      }

      return el;
    },

    hydrateElement(element, scope) {
      if (this.hydrated.has(element)) return;
      this.hydrated.add(element);

      const binds = this.parseBindMap(element);
      if (binds) {
        for (const prop of Object.keys(binds)) {
          this.bindProperty(element, prop, binds[prop], scope);
        }
      }

      const events = this.parseEventMap(element);
      if (events) {
        for (const eventName of Object.keys(events)) {
          this.bindEvent(element, eventName, events[eventName], scope);
        }
      }

      const mapConfig = this.parseJsonAttribute(element, 'data-josie-map');
      if (mapConfig && mapConfig.source && mapConfig.each) {
        this.bindMap(element, mapConfig, scope);
      }
    },

    hydrateScopeTree(root, scope) {
      if (!root) return;

      const selector =
        '[data-josie-bind], [data-josie-binds], [data-josie-on], [data-josie-ons], [data-josie-map]';
      const nodes = [root];
      for (const el of root.querySelectorAll(selector)) nodes.push(el);
      const rootMap = typeof root.closest === 'function' ? root.closest('[data-josie-map]') : null;

      const mapRoots = [];
      for (const el of nodes) {
        const enclosingMap =
          typeof el.closest === 'function' ? el.closest('[data-josie-map]') : null;
        const insideForeignMapRoot =
          el !== root &&
          enclosingMap &&
          enclosingMap !== rootMap &&
          !el.hasAttribute('data-josie-map');
        if (insideForeignMapRoot) {
          continue;
        }

        if (el.hasAttribute('data-josie-map')) {
          mapRoots.push(el);
        } else {
          this.hydrateElement(el, scope);
        }
      }

      for (const mapRoot of mapRoots) {
        this.hydrateElement(mapRoot, scope);
      }
    },

    discover(root) {
      this.hydrateScopeTree(root, {});
    },
  };

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = JOSIE;
  }

  window.JOSIE = JOSIE;

  const boot = () => {
    if (window.__JOSIE__ && window.__JOSIE__.state) {
      JOSIE.init(window.__JOSIE__.state, window.__JOSIE__.theme || {});
    } else {
      JOSIE.init({}, {});
    }
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else {
    boot();
  }
})(window);
