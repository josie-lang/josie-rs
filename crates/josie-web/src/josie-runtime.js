/**
 * josie-runtime.js — Josie Web Production Runtime (compiled mode)
 *
 * WHAT THIS FILE IS
 * -----------------
 * This is the client-side runtime shipped to browsers in compiled mode.
 * It is embedded into the Rust josie-web binary via include_str! and emitted
 * verbatim during export. A real minifier should be applied on export to strip
 * these comments and reduce payload (current minify_js_light only trims lines).
 *
 * WHAT IT IS NOT
 * --------------
 * This is NOT the interpreted/dev runtime. For interpreted mode (dev), the
 * browser loads josie-runtime-dev.js which contains the full evalLite
 * interpreter. This file is for production compiled pages only.
 *
 * ARCHITECTURE
 * ------------
 * Josie's core insight: the Rust compiler knows the full reactive dependency
 * graph at compile time (all j-text/j-model/j-attr/j-show/map bindings are
 * static attributes). So instead of doing a full DOM walk on every state
 * change (like React's VDOM diff), Josie builds a static subscription map
 * at page load and updates only the exact DOM nodes that depend on a changed
 * path. O(1) per update, not O(DOM size).
 *
 * REACTIVE UPDATE FLOW
 * --------------------
 *   User event (click, input, etc.)
 *     → delegateEvent listener fires (one listener per event type, on document.body)
 *     → runAction(actionName, event)
 *     → compiled step functions run (native JS, no interpreter)
 *     → J.set(path, value) → enqueueReactivePath(path)
 *     → queueMicrotask fires flushReactiveLoop()
 *         → memos recomputed if deps changed (up to 64 passes to settle)
 *         → effects triggered if deps changed
 *         → notify(path) fires for all dirty paths
 *             → exact subscribers: _subs.get(path)
 *             → descendant subscribers: _subsDesc.get(ancestor)
 *             → wildcard subscribers: _subsWildcard
 *     → only affected DOM nodes update — zero full-body walk
 *
 * STARTUP SEQUENCE (compiled mode)
 * ----------------------------------
 *   1. Browser receives SSR HTML (fast first paint)
 *   2. josie-runtime.js loads (this file, ~10 KB gzip)
 *   3. program.compiled.js loads:
 *        - window.__JOSIE_INITIAL_STATE__  (client state snapshot)
 *        - window.__JOSIE_SERVER_STATE__   (server state snapshot, read-only)
 *        - window.__JOSIE_SECURITY__       (allowList + loadScripts)
 *        - window.__JOSIE_BINDINGS__       (compile-time reactive binding map)
 *        - window.__JOSIE_COMPILED_STEPS__ (pre-compiled step functions)
 *        - window.__JOSIE_COMPILED_ACTIONS__(pre-compiled action dispatchers)
 *   4. JOSIE.init() runs:
 *        - hydrates state from __JOSIE_INITIAL_STATE__
 *        - hydrates serverState from __JOSIE_SERVER_STATE__
 *        - loads compiled steps/actions
 *        - enforces security walls (feature wall, request wall)
 *        - runs reactive memos/effects (initial pass)
 *        - querySelectorAll('[data-jid]') → builds _byJid element map
 *        - installs one subscriber per binding → fires each once to hydrate DOM
 *        - bindEvents() → calls delegateEvent() for each @event type found
 *        - NO full renderTree(document.body) — subscribers already hydrated DOM
 *   5. Page is fully interactive
 *
 * SUBSCRIPTION INDEXES
 * --------------------
 *   _subs      — Map<path, fn[]>   exact path match
 *   _subsDesc  — Map<path, fn[]>   ancestor → descendants index
 *                                  (setting "client.user" notifies "client.user.name")
 *   _subsWildcard — fn[]           wildcard subscribers (data-josie-map with dynamic source)
 *
 * DATA OPS (data.*)
 * -----------------
 * All data.* operations (select, group_by, aggregate, join, sort_by, etc.) are
 * implemented as native JS methods on JOSIE.data. In compiled mode, the Rust
 * compiler emits J.data.select(...) calls with pre-compiled argument expressions.
 * This enables offline/PWA apps to process IndexedDB/localStorage data at full
 * JS engine speed — no interpreter, no server needed.
 *
 * STATE NAMESPACES
 * ----------------
 *   client.*  — mutable, owned by the browser, changed by actions and j-model
 *   server.*  — read-only snapshot injected by the Rust backend at render time
 *   local.*   — scope-local variables inside list/map item templates
 *   w.event.* — current event (value, key) inside an action triggered by user input
 *
 * SECURITY WALLS (runtime enforcement)
 * -------------------------------------
 *   Request wall  — fetch/XHR/WebSocket patched to block URLs not in allowList
 *   Feature wall  — localStorage/indexedDB/GPS/camera gated by allowList features
 *   URL wall      — j-attr:href/src/etc. validated against allowList on each update
 *   Static URL wall — applied at init to sanitize server-rendered href/src attrs
 */
(function (window) {
  'use strict';

  const JOSIE = {
    program: {},
    state: {},
    serverState: {},
    steps: {},
    actions: {},
    boundEvents: new WeakMap(),
    security: { allowList: [], loadScripts: [], features: {} },
    _requestWallPatched: false,
    reactive: { memos: [], effects: [], effectOnceFired: new Set() },
    _pendingReactivePaths: new Set(),
    _reactiveScheduled: false,
    _flushingReactive: false,
    _subs: new Map(),
    _subsDesc: new Map(),
    _subsWildcard: [],
    _byJid: {},
    _delegatedEvents: new Set(),

    util: {
      concat(args) {
        if (!Array.isArray(args)) return '';
        return args.map((v) => (v == null ? '' : String(v))).join('');
      },
      lower(v) { return String(v == null ? '' : v).toLowerCase(); },
      upper(v) { return String(v == null ? '' : v).toUpperCase(); },
      contains(hay, needle) {
        if (typeof hay === 'string') return hay.includes(String(needle));
        if (Array.isArray(hay)) return hay.includes(needle);
        return false;
      },
      template(args) {
        if (!Array.isArray(args) || args.length === 0) return '';
        let out = String(args[0] == null ? '' : args[0]);
        for (let i = 1; i < args.length; i += 1) {
          if (!out.includes('{}')) break;
          out = out.replace('{}', String(args[i] == null ? '' : args[i]));
        }
        return out;
      },
      toInt(v) {
        const n = Number.parseInt(String(v == null ? '0' : v), 10);
        return Number.isFinite(n) ? n : 0;
      },
      toFloat(v) {
        const n = Number.parseFloat(String(v == null ? '0' : v));
        return Number.isFinite(n) ? n : 0;
      },
      toString(v) { return String(v == null ? '' : v); },
      trim(v) { return String(v == null ? '' : v).trim(); },
      strLen(v) { return String(v == null ? '' : v).length; },
      getPath(base, path) {
        const p = String(path == null ? '' : path);
        if (!p) return base;
        return JOSIE.readBySegments(base, p.split('.'));
      },
      asArray(v) {
        if (v == null) return [];
        return Array.isArray(v) ? v : [v];
      },
      toBool(v) { return JOSIE.truthy(v); },
      replace(s, from, to) {
        const src = String(s == null ? '' : s);
        const f = String(from == null ? '' : from);
        const t = String(to == null ? '' : to);
        if (!f) return src;
        return src.replace(f, t);
      },
      replaceAll(s, from, to) {
        const src = String(s == null ? '' : s);
        const f = String(from == null ? '' : from);
        const t = String(to == null ? '' : to);
        if (!f) return src;
        return src.split(f).join(t);
      },
      split(s, delim) {
        const src = String(s == null ? '' : s);
        const d = String(delim == null ? '' : delim);
        if (!d) return src.split('');
        return src.split(d);
      },
      join(arr, delim) {
        if (!Array.isArray(arr)) return '';
        const d = String(delim == null ? '' : delim);
        return arr.map((v) => (v == null ? '' : String(v))).join(d);
      },
      jsonParse(s) {
        try {
          return JSON.parse(String(s == null ? '' : s));
        } catch (_e) {
          return null;
        }
      },
      jsonStringify(v, pretty) {
        try {
          const space = Number(pretty || 0);
          const clamp = Number.isFinite(space) ? Math.max(0, Math.min(8, Math.trunc(space))) : 0;
          return JSON.stringify(v, null, clamp);
        } catch (_e) {
          return '';
        }
      },
      regexMatch(s, p, opts) {
        try {
          return JOSIE.buildRegex(String(p == null ? '' : p), JOSIE.parseRegexOpts(opts), false)
            .test(String(s == null ? '' : s));
        } catch (_e) { return false; }
      },
      regexFind(s, p, opts) {
        try {
          const m = String(s == null ? '' : s)
            .match(JOSIE.buildRegex(String(p == null ? '' : p), JOSIE.parseRegexOpts(opts), false));
          return m && m[0] !== undefined ? m[0] : null;
        } catch (_e) { return null; }
      },
      regexFindAll(s, p, opts) {
        try {
          const src = String(s == null ? '' : s);
          const re = JOSIE.buildRegex(String(p == null ? '' : p), JOSIE.parseRegexOpts(opts), true);
          const out = [];
          let m;
          while ((m = re.exec(src)) !== null) {
            out.push(m[0]);
            if (m[0] === '') re.lastIndex += 1;
          }
          return out;
        } catch (_e) { return []; }
      },
      regexReplace(s, p, r, opts) {
        try {
          const src = String(s == null ? '' : s);
          const rep = String(r == null ? '' : r);
          const conf = JOSIE.parseRegexOpts(opts);
          const re = JOSIE.buildRegex(String(p == null ? '' : p), conf, !!conf.all);
          return src.replace(re, rep);
        } catch (_e) {
          return String(s == null ? '' : s);
        }
      },
    },

    init(bootstrap) {
      const input = bootstrap || window.__JOSIE_BOOTSTRAP__ || {};
      this.program = Object.assign(this.program || {}, input.program || {});
      const compiledState = window.__JOSIE_INITIAL_STATE__ || null;
      const state = input.state || compiledState || (this.program.state && (this.program.state.client || this.program.state)) || {};
      this.state = this.clone(state);
      const compiledServerState = window.__JOSIE_SERVER_STATE__ || null;
      this.serverState = this.clone(input.serverState || compiledServerState || (this.program.state && this.program.state.server) || {});
      this.actions = this.program.actions && typeof this.program.actions === 'object' ? this.program.actions : {};
      this.indexSteps();
      this.loadCompiled();
      this.initSecurity(input);
      this.initReactiveContracts();
      this.flushReactiveNow();
      this.sanitizeStaticUrls(document.body);
      this.initCompiledBindings(input);
      this.bindEvents(document.body, { locals: {}, event: null });
      // Only do a full DOM walk when there are no compiled bindings.
      // With compiled bindings, initCompiledBindings already fired each subscriber
      // once to hydrate every reactive element — a second full walk is redundant.
      if (this._subs.size === 0) {
        this.renderTree(document.body, { locals: {}, event: null });
      }
      this.flushReactiveNow();
    },

    clone(value) {
      return value == null ? value : JSON.parse(JSON.stringify(value));
    },

    indexSteps() {
      this.steps = {};
      const list = Array.isArray(this.program.steps) ? this.program.steps : [];
      for (const step of list) {
        if (step && typeof step.id === 'string' && step.id.length > 0) {
          this.steps[step.id] = step;
        }
      }
    },

    loadCompiled() {
      const compiledSteps = window.__JOSIE_COMPILED_STEPS__;
      if (compiledSteps && typeof compiledSteps === 'object') {
        for (const key of Object.keys(compiledSteps)) {
          if (typeof compiledSteps[key] === 'function') {
            this.steps[key] = compiledSteps[key];
          }
        }
      }

      const compiledActions = window.__JOSIE_COMPILED_ACTIONS__;
      if (compiledActions && typeof compiledActions === 'object') {
        for (const key of Object.keys(compiledActions)) {
          if (typeof compiledActions[key] === 'function') {
            this.actions[key] = compiledActions[key];
          }
        }
      }
    },

    initSecurity(input) {
      const bootstrap = input && input.security && typeof input.security === 'object' ? input.security : {};
      const globalSecurity = window.__JOSIE_SECURITY__ && typeof window.__JOSIE_SECURITY__ === 'object'
        ? window.__JOSIE_SECURITY__
        : {};
      const programAllow = Array.isArray(this.program.allowList) ? this.program.allowList : [];
      const allowList = Array.isArray(globalSecurity.allowList)
        ? globalSecurity.allowList
        : (Array.isArray(bootstrap.allowList) ? bootstrap.allowList : programAllow);
      const loadScripts = Array.isArray(globalSecurity.loadScripts)
        ? globalSecurity.loadScripts
        : (Array.isArray(bootstrap.loadScripts) ? bootstrap.loadScripts : []);

      this.security = {
        allowList,
        loadScripts,
        features: this.computeFeatures(allowList),
      };
      this.enforceFeatureWall();
      this.enforceRequestWall();
    },

    computeFeatures(allowList) {
      const out = {};
      for (const rule of Array.isArray(allowList) ? allowList : []) {
        if (!rule || typeof rule !== 'object') continue;
        if (String(rule.type || '').toLowerCase() !== 'feature') continue;
        const value = String(rule.value || '').trim().toLowerCase();
        if (!value) continue;
        out[value] = true;
      }
      return out;
    },

    featureAllowed(name) {
      const features = this.security && this.security.features ? this.security.features : {};
      if (features.all) return true;
      return !!features[String(name || '').toLowerCase()];
    },

    allowRuleMatches(candidate, rule) {
      const c = String(candidate == null ? '' : candidate).trim();
      const r = String(rule == null ? '' : rule).trim();
      if (!r) return false;
      if (r === '*') return true;
      if (c === r) return true;
      if (r.endsWith('/') && c.startsWith(r)) return true;
      if (!r.endsWith('/') && c.startsWith(r)) return true;
      return false;
    },

    urlRules() {
      const allowList = this.security && Array.isArray(this.security.allowList) ? this.security.allowList : [];
      return allowList
        .filter((rule) => rule && typeof rule === 'object' && String(rule.type || '').toLowerCase() === 'url')
        .map((rule) => String(rule.value || '').trim())
        .filter((v) => v.length > 0);
    },

    isUrlAllowed(url) {
      const candidate = String(url == null ? '' : url).trim();
      if (!candidate) return true;
      const rules = this.urlRules();
      if (rules.length === 0) return true;
      for (const rule of rules) {
        if (this.allowRuleMatches(candidate, rule)) return true;
      }
      return false;
    },

    isUrlAttr(name) {
      const key = String(name || '').toLowerCase();
      return key === 'href'
        || key === 'src'
        || key === 'action'
        || key === 'formaction'
        || key === 'poster'
        || key === 'data';
    },

    enforceFeatureWall() {
      if (!this.featureAllowed('localstorage')) {
        this.blockStorage('localStorage');
      }
      if (!this.featureAllowed('indexeddb')) {
        try {
          Object.defineProperty(window, 'indexedDB', { configurable: true, get() { return undefined; } });
        } catch (_e) {}
      }
      if (!this.featureAllowed('gps') && window.navigator) {
        const deniedGeo = {
          getCurrentPosition(_ok, err) {
            if (typeof err === 'function') err({ code: 1, message: 'blocked by Josie allowList' });
          },
          watchPosition(_ok, err) {
            if (typeof err === 'function') err({ code: 1, message: 'blocked by Josie allowList' });
            return -1;
          },
          clearWatch() {},
        };
        try {
          Object.defineProperty(window.navigator, 'geolocation', { configurable: true, get() { return deniedGeo; } });
        } catch (_e) {}
      }
      if (!this.featureAllowed('camera') && window.navigator && window.navigator.mediaDevices) {
        const denied = function deniedCamera() {
          return Promise.reject(new Error('blocked by Josie allowList'));
        };
        try {
          window.navigator.mediaDevices.getUserMedia = denied;
        } catch (_e) {}
      }
    },

    enforceRequestWall() {
      if (this._requestWallPatched) return;
      this._requestWallPatched = true;
      if (this.urlRules().length === 0) return;
      const self = this;

      if (typeof window.fetch === 'function') {
        const nativeFetch = window.fetch.bind(window);
        window.fetch = function josieFetch(input, init) {
          const url = typeof input === 'string'
            ? input
            : (input && typeof input.url === 'string' ? input.url : '');
          if (!self.isUrlAllowed(url)) {
            return Promise.reject(new Error('blocked by Josie allowList'));
          }
          return nativeFetch(input, init);
        };
      }

      if (window.XMLHttpRequest && window.XMLHttpRequest.prototype && typeof window.XMLHttpRequest.prototype.open === 'function') {
        const nativeOpen = window.XMLHttpRequest.prototype.open;
        window.XMLHttpRequest.prototype.open = function josieXhrOpen(method, url) {
          if (!self.isUrlAllowed(String(url == null ? '' : url))) {
            throw new Error('blocked by Josie allowList');
          }
          return nativeOpen.apply(this, arguments);
        };
      }

      if (typeof window.WebSocket === 'function') {
        const NativeWebSocket = window.WebSocket;
        window.WebSocket = function JosieWebSocket(url, protocols) {
          const target = String(url == null ? '' : url);
          if (!self.isUrlAllowed(target)) {
            throw new Error('blocked by Josie allowList');
          }
          return protocols === undefined
            ? new NativeWebSocket(url)
            : new NativeWebSocket(url, protocols);
        };
        window.WebSocket.prototype = NativeWebSocket.prototype;
      }
    },

    blockStorage(name) {
      const blocked = {
        getItem() { return null; },
        setItem() {},
        removeItem() {},
        clear() {},
        key() { return null; },
        get length() { return 0; },
      };
      try {
        Object.defineProperty(window, name, { configurable: true, get() { return blocked; } });
      } catch (_e) {}
    },

    sanitizeStaticUrls(root) {
      if (!root || root.nodeType !== 1) return;
      const all = [root].concat(Array.from(root.querySelectorAll('*')));
      for (const el of all) {
        for (const attr of Array.from(el.attributes || [])) {
          const key = String(attr.name || '').toLowerCase();
          if (key.startsWith('on')) {
            el.removeAttribute(attr.name);
            continue;
          }
          if (!this.isUrlAttr(key)) continue;
          const value = String(attr.value || '');
          if (!this.isUrlAllowed(value)) {
            el.removeAttribute(attr.name);
          }
        }
      }
    },

    initReactiveContracts() {
      const memos = [];
      const effects = [];
      const rawMemos = Array.isArray(this.program.memos) ? this.program.memos : [];
      const rawEffects = Array.isArray(this.program.effects) ? this.program.effects : [];

      for (const memo of rawMemos) {
        if (!memo || typeof memo !== 'object') continue;
        if (typeof memo.id !== 'string' || memo.id.trim().length === 0) continue;
        if (typeof memo.runStep !== 'string' || memo.runStep.trim().length === 0) continue;
        if (typeof memo.into !== 'string' || memo.into.trim().length === 0) continue;
        memos.push({
          id: memo.id.trim(),
          deps: Array.isArray(memo.deps) ? memo.deps.filter((v) => typeof v === 'string' && v.trim().length > 0) : [],
          runStep: memo.runStep.trim(),
          into: memo.into.trim(),
        });
      }

      for (const effect of rawEffects) {
        if (!effect || typeof effect !== 'object') continue;
        if (typeof effect.id !== 'string' || effect.id.trim().length === 0) continue;
        if (typeof effect.runStep !== 'string' || effect.runStep.trim().length === 0) continue;
        effects.push({
          id: effect.id.trim(),
          deps: Array.isArray(effect.deps) ? effect.deps.filter((v) => typeof v === 'string' && v.trim().length > 0) : [],
          runStep: effect.runStep.trim(),
          immediate: !!effect.immediate,
          once: !!effect.once,
        });
      }

      this.reactive = {
        memos,
        effects,
        effectOnceFired: new Set(),
      };

      const scope = { locals: {}, event: null };
      for (const memo of memos) {
        const value = this.runStep(memo.runStep, scope);
        this.set(memo.into, value);
      }
      for (const effect of effects) {
        if (!effect.immediate) continue;
        if (effect.once && this.reactive.effectOnceFired.has(effect.id)) continue;
        this.runStep(effect.runStep, scope);
        if (effect.once) this.reactive.effectOnceFired.add(effect.id);
      }
    },

    pathMatchesDependency(changedPath, depPath) {
      const changed = String(changedPath || '').trim();
      const dep = String(depPath || '').trim();
      if (!changed || !dep) return false;
      if (dep === '*') return true;
      if (changed === dep) return true;
      if (changed.startsWith(dep + '.')) return true;
      if (dep.startsWith(changed + '.')) return true;
      return false;
    },

    reactiveEntryMatchesPath(entry, changedPath) {
      const deps = Array.isArray(entry.deps) ? entry.deps : [];
      if (deps.length === 0) return true;
      for (const dep of deps) {
        if (this.pathMatchesDependency(changedPath, dep)) return true;
      }
      return false;
    },

    enqueueReactivePath(path) {
      const normalized = String(path || '').trim();
      if (!normalized) return;
      this._pendingReactivePaths.add(normalized);
      if (!this._reactiveScheduled) {
        this._reactiveScheduled = true;
        queueMicrotask(() => {
          this._reactiveScheduled = false;
          this.flushReactiveLoop();
        });
      }
    },

    flushReactiveNow() {
      this._reactiveScheduled = false;
      this.flushReactiveLoop();
    },

    flushReactiveLoop() {
      if (this._flushingReactive) return;
      this._flushingReactive = true;
      let pass = 0;
      const changed = new Set();
      while (this._pendingReactivePaths.size > 0 && pass < 64) {
        pass += 1;
        const paths = Array.from(this._pendingReactivePaths);
        this._pendingReactivePaths.clear();
        for (const path of paths) {
          changed.add(path);
          this.runReactiveForPath(path);
        }
      }
      if (pass >= 64 && window.console && console.warn) {
        console.warn('[JOSIE] reactive flush stopped after 64 passes');
      }
      for (const path of changed) {
        this.notify(path);
      }
      this._flushingReactive = false;
    },

    runReactiveForPath(path) {
      const scope = { locals: {}, event: null };
      const reactive = this.reactive || { memos: [], effects: [], effectOnceFired: new Set() };
      const memos = Array.isArray(reactive.memos) ? reactive.memos : [];
      const effects = Array.isArray(reactive.effects) ? reactive.effects : [];
      const effectOnceFired = reactive.effectOnceFired instanceof Set ? reactive.effectOnceFired : new Set();

      for (const memo of memos) {
        if (!this.reactiveEntryMatchesPath(memo, path)) continue;
        const value = this.runStep(memo.runStep, scope);
        this.set(memo.into, value);
      }

      for (const effect of effects) {
        if (!this.reactiveEntryMatchesPath(effect, path)) continue;
        if (effect.once && effectOnceFired.has(effect.id)) continue;
        this.runStep(effect.runStep, scope);
        if (effect.once) effectOnceFired.add(effect.id);
      }
    },

    initCompiledBindings(input) {
      this._subs = new Map();
      this._subsDesc = new Map();
      this._subsWildcard = [];
      this._byJid = {};
      for (const el of Array.from(document.querySelectorAll('[data-jid]'))) {
        const jid = el.getAttribute('data-jid');
        if (jid) this._byJid[jid] = el;
      }

      const fromBootstrap = input && Array.isArray(input.bindings) ? input.bindings : [];
      const fromGlobal = Array.isArray(window.__JOSIE_BINDINGS__) ? window.__JOSIE_BINDINGS__ : [];
      const bindings = fromGlobal.length > 0 ? fromGlobal : fromBootstrap;
      for (const binding of bindings) {
        this.installBinding(binding);
      }
    },

    subscribe(path, fn) {
      const key = String(path || '').trim();
      if (!key || typeof fn !== 'function') return;
      if (!this._subs.has(key)) this._subs.set(key, []);
      this._subs.get(key).push(fn);
      if (key === '*') {
        this._subsWildcard.push(fn);
        return;
      }
      for (const prefix of this.pathAncestors(key)) {
        if (!this._subsDesc.has(prefix)) this._subsDesc.set(prefix, []);
        this._subsDesc.get(prefix).push(fn);
      }
    },

    pathAncestors(path) {
      const key = String(path || '').trim();
      if (!key) return [];
      const parts = key.split('.').filter((v) => v.length > 0);
      const out = [];
      for (let i = 1; i <= parts.length; i += 1) {
        out.push(parts.slice(0, i).join('.'));
      }
      return out;
    },

    notify(path) {
      const changed = String(path || '').trim();
      if (!changed) return;
      const run = new Set();
      for (const fn of this._subsWildcard) run.add(fn);

      for (const ancestor of this.pathAncestors(changed)) {
        const fns = this._subs.get(ancestor);
        if (!fns) continue;
        for (const fn of fns) run.add(fn);
      }

      const descendants = this._subsDesc.get(changed);
      if (descendants) {
        for (const fn of descendants) run.add(fn);
      }

      for (const fn of run) {
        try { fn(); } catch (_e) {}
      }
    },

    installBinding(binding) {
      if (!binding || typeof binding !== 'object') return;
      const jid = String(binding.jid || '').trim();
      const path = String(binding.path || '').trim();
      const kind = String(binding.kind || '').trim();
      if (!jid || !path || !kind) return;
      const attr = binding.attr == null ? null : String(binding.attr);

      const updater = () => {
        const el = this._byJid[jid];
        if (!el) return;
        
        let value;
        const trimmedPath = String(path || '').trim();
        if (trimmedPath.startsWith('[') || trimmedPath.startsWith('{')) {
          try {
            const expr = JSON.parse(trimmedPath);
            value = this.evalLite(expr, { locals: {}, event: null });
          } catch (e) {
            console.error('[JOSIE] failed to parse binding expression', trimmedPath, e);
            value = null;
          }
        } else {
          value = this.get(trimmedPath, { locals: {}, event: null });
        }
        
        const out = value == null ? '' : String(value);
        if (kind === 'text') {
          if (el.textContent !== out) el.textContent = out;
          return;
        }
        if (kind === 'model') {
          if (el.value !== undefined && el.value !== out) el.value = out;
          return;
        }
        if (kind === 'attr') {
          const prop = String(attr || '').trim();
          if (!prop || prop.toLowerCase().startsWith('on')) return;
          if (this.isUrlAttr(prop) && !this.isUrlAllowed(out)) {
            el.removeAttribute(prop);
            return;
          }
          if (prop === 'class') el.className = out;
          else {
            el.setAttribute(prop, out);
            if (prop === 'value' && el.value !== undefined) el.value = out;
          }
          return;
        }
        if (kind === 'show') {
          const visible = this.truthy(value);
          if (visible) {
            el.style.removeProperty('display');
          } else {
            el.style.setProperty('display', 'none', 'important');
          }
          return;
        }
        if (kind === 'map') {
          const cfg = el.getAttribute('data-josie-map');
          if (!cfg) return;
          this.renderMapNode(el, cfg, { locals: {}, event: null });
        }
      };

      this.subscribe(path, updater);
      updater();
    },

    delegateEvent(eventName) {
      const evt = String(eventName || '').trim();
      if (!evt) return;
      if (this._delegatedEvents.has(evt)) return;
      this._delegatedEvents.add(evt);

      // Capture-phase delegation makes submit/form events reliable across
      // browser differences and avoids missing non-bubbling edge cases.
      document.addEventListener(evt, (raw) => {
        const target = raw && raw.target ? raw.target : null;
        const actionEl = this.findActionElement(target, evt);
        if (!actionEl) return;
        const action = actionEl.getAttribute(`@${evt}`);
        if (!action) return;
        if (evt === 'submit' && raw && typeof raw.preventDefault === 'function') {
          raw.preventDefault();
        }
        const event = {
          value: actionEl && ('value' in actionEl) ? actionEl.value : null,
          key: raw && ('key' in raw) ? raw.key : null,
          raw,
          scope: { locals: {}, event: null },
        };
        this.runAction(String(action).trim(), event);
      }, true);
    },

    findActionElement(start, eventName) {
      const attr = `@${eventName}`;
      let current = start;
      while (current) {
        if (current.nodeType === 1 && current.hasAttribute && current.hasAttribute(attr)) {
          return current;
        }
        current = current.parentElement || null;
      }
      return null;
    },

    truthy(value) {
      if (value == null) return false;
      if (typeof value === 'boolean') return value;
      if (typeof value === 'number') return value !== 0;
      if (typeof value === 'string') return value.length > 0;
      if (Array.isArray(value)) return value.length > 0;
      if (typeof value === 'object') return Object.keys(value).length > 0;
      return Boolean(value);
    },

    readBySegments(obj, segments) {
      let current = obj;
      for (const seg of segments) {
        if (current == null) return null;
        if (Array.isArray(current)) {
          const idx = Number(seg);
          if (!Number.isInteger(idx)) return null;
          current = current[idx];
          continue;
        }
        if (typeof current === 'object') {
          current = current[seg];
          continue;
        }
        return null;
      }
      return current === undefined ? null : current;
    },

    get(path, scope) {
      if (typeof path !== 'string' || path.length === 0) return null;
      if (path.startsWith('!')) {
        return !this.truthy(this.get(path.slice(1), scope));
      }
      const locals = scope && scope.locals ? scope.locals : {};

      if (path.startsWith('local.')) {
        return this.readBySegments(locals, path.slice('local.'.length).split('.'));
      }
      if (path.startsWith('client.')) {
        return this.readBySegments(this.state, path.slice('client.'.length).split('.'));
      }
      if (path.startsWith('server.')) {
        return this.readBySegments(this.serverState || {}, path.slice('server.'.length).split('.'));
      }
      if (path.startsWith('w.event.')) {
        const event = scope && scope.event ? scope.event : null;
        if (!event) return null;
        if (path === 'w.event.value') return event.value;
        if (path === 'w.event.key') return event.key;
        return null;
      }

      const root = path.split('.')[0];
      if (Object.prototype.hasOwnProperty.call(locals, root)) {
        return this.readBySegments(locals, path.split('.'));
      }
      return this.readBySegments(this.state, path.split('.'));
    },

    set(path, value) {
      if (typeof path !== 'string' || path.length === 0) return value;
      if (path.startsWith('server.') || path.startsWith('local.') || path.startsWith('w.event.')) return value;
      const normalized = path.startsWith('client.') ? path.slice('client.'.length) : path;
      const parts = normalized.split('.').filter((p) => p.length > 0);
      if (parts.length === 0) return value;

      let current = this.state;
      for (let i = 0; i < parts.length - 1; i += 1) {
        const key = parts[i];
        if (!current[key] || typeof current[key] !== 'object' || Array.isArray(current[key])) {
          current[key] = {};
        }
        current = current[key];
      }
      current[parts[parts.length - 1]] = value;
      const changedPath = path.startsWith('client.') ? path : ('client.' + normalized);
      this.enqueueReactivePath(changedPath);
      return value;
    },

    getFrom(collection, key) {
      if (Array.isArray(collection)) {
        const idx = Number(key);
        return Number.isInteger(idx) ? (collection[idx] !== undefined ? collection[idx] : null) : null;
      }
      if (collection && typeof collection === 'object') {
        const mapKey = String(key);
        return Object.prototype.hasOwnProperty.call(collection, mapKey) ? collection[mapKey] : null;
      }
      return null;
    },

    parseRegexOpts(raw) {
      const out = {
        caseInsensitive: false,
        multiLine: false,
        dotAll: false,
        unicode: true,
        all: true,
      };
      if (raw == null) return out;
      if (typeof raw === 'boolean') {
        out.all = raw;
        return out;
      }
      if (typeof raw === 'string') {
        for (const ch of raw) {
          if (ch === 'i' || ch === 'I') out.caseInsensitive = true;
          if (ch === 'm' || ch === 'M') out.multiLine = true;
          if (ch === 's' || ch === 'S') out.dotAll = true;
          if (ch === 'u' || ch === 'U') out.unicode = true;
          if (ch === 'g' || ch === 'G') out.all = true;
        }
        return out;
      }
      if (typeof raw === 'object') {
        if (Object.prototype.hasOwnProperty.call(raw, 'case_insensitive')) out.caseInsensitive = this.truthy(raw.case_insensitive);
        if (Object.prototype.hasOwnProperty.call(raw, 'ignore_case')) out.caseInsensitive = this.truthy(raw.ignore_case);
        if (Object.prototype.hasOwnProperty.call(raw, 'multi_line')) out.multiLine = this.truthy(raw.multi_line);
        if (Object.prototype.hasOwnProperty.call(raw, 'multiline')) out.multiLine = this.truthy(raw.multiline);
        if (Object.prototype.hasOwnProperty.call(raw, 'dot_matches_new_line')) out.dotAll = this.truthy(raw.dot_matches_new_line);
        if (Object.prototype.hasOwnProperty.call(raw, 'dot_all')) out.dotAll = this.truthy(raw.dot_all);
        if (Object.prototype.hasOwnProperty.call(raw, 'unicode')) out.unicode = this.truthy(raw.unicode);
        if (Object.prototype.hasOwnProperty.call(raw, 'all')) out.all = this.truthy(raw.all);
      }
      return out;
    },

    buildRegex(pattern, opts, forceGlobal) {
      let flags = '';
      if (opts.caseInsensitive) flags += 'i';
      if (opts.multiLine) flags += 'm';
      if (opts.dotAll) flags += 's';
      if (opts.unicode) flags += 'u';
      if (forceGlobal || opts.all) flags += 'g';
      return new RegExp(pattern, flags);
    },

    callExternal(name, args, scope) {
      const registry = window.JOSIE_X && typeof window.JOSIE_X === 'object' ? window.JOSIE_X : {};
      const fn = registry[name];
      if (typeof fn !== 'function') return null;
      try {
        return fn(args, { state: this.state, scope, josie: this });
      } catch (err) {
        if (window.console && console.error) {
          console.error('[JOSIE] external call failed', name, err);
        }
        return null;
      }
    },

    apiRequest(rawConfig) {
      const cfg = rawConfig && typeof rawConfig === 'object' ? rawConfig : {};
      const method = String(cfg.method || 'GET').toUpperCase();
      const url = String(cfg.url || '').trim();
      if (!url) {
        return { ok: false, status: 0, message: 'api.request url is required', data: null, text: '' };
      }
      if (!this.isUrlAllowed(url)) {
        return { ok: false, status: 0, message: 'blocked by Josie allowList', data: null, text: '' };
      }

      const headers = cfg.headers && typeof cfg.headers === 'object' ? cfg.headers : {};
      let body = null;
      if (Object.prototype.hasOwnProperty.call(cfg, 'json')) {
        try {
          body = JSON.stringify(cfg.json);
        } catch (_e) {
          return { ok: false, status: 0, message: 'api.request json encode failed', data: null, text: '' };
        }
      } else if (Object.prototype.hasOwnProperty.call(cfg, 'body')) {
        if (typeof cfg.body === 'string') body = cfg.body;
        else {
          try { body = JSON.stringify(cfg.body); } catch (_e) { body = String(cfg.body); }
        }
      }

      try {
        const xhr = new XMLHttpRequest();
        xhr.open(method, url, false);
        xhr.withCredentials = true;
        let hasContentType = false;
        for (const [k, v] of Object.entries(headers)) {
          if (String(k).toLowerCase() === 'content-type') hasContentType = true;
          xhr.setRequestHeader(String(k), String(v == null ? '' : v));
        }
        if (!hasContentType && Object.prototype.hasOwnProperty.call(cfg, 'json')) {
          xhr.setRequestHeader('content-type', 'application/json');
        }
        xhr.send(body);

        const text = xhr.responseText || '';
        const contentType = String(xhr.getResponseHeader('content-type') || '').toLowerCase();
        let data = text;
        if (contentType.includes('application/json')) {
          try { data = text ? JSON.parse(text) : null; } catch (_e) { data = text; }
        }
        const status = Number(xhr.status || 0);
        const basicOk = status >= 200 && status < 300;
        const payloadOk = !(data && typeof data === 'object' && Object.prototype.hasOwnProperty.call(data, 'ok') && data.ok === false);
        const ok = basicOk && payloadOk;
        const message = data && typeof data === 'object' && typeof data.message === 'string'
          ? data.message
          : (ok ? 'ok' : `request failed ${status || 0}`);
        return { ok, status, message, data, text };
      } catch (err) {
        return {
          ok: false,
          status: 0,
          message: String(err && err.message ? err.message : err),
          data: null,
          text: '',
        };
      }
    },

    // ── data.* ────────────────────────────────────────────────────────────────
    // Native JS implementations matching josie-core semantics exactly.
    // Used by compiled mode (J.data.select(...)) and offline/PWA data pipelines.
    data: {
      _get(obj, path) {
        if (obj == null || typeof path !== 'string') return null;
        let cur = obj;
        for (const s of path.split('.')) {
          if (cur == null || typeof cur !== 'object') return null;
          cur = Array.isArray(cur) ? cur[Number(s)] : cur[s];
        }
        return cur === undefined ? null : cur;
      },
      _set(obj, path, value) {
        if (obj == null || typeof path !== 'string') return;
        const segs = path.split('.');
        let cur = obj;
        for (let i = 0; i < segs.length - 1; i++) {
          if (cur[segs[i]] == null || typeof cur[segs[i]] !== 'object') cur[segs[i]] = {};
          cur = cur[segs[i]];
        }
        cur[segs[segs.length - 1]] = value;
      },
      _del(obj, path) {
        if (obj == null || typeof path !== 'string') return;
        const segs = path.split('.');
        let cur = obj;
        for (let i = 0; i < segs.length - 1; i++) {
          if (cur == null || typeof cur !== 'object') return;
          cur = cur[segs[i]];
        }
        if (cur && typeof cur === 'object') delete cur[segs[segs.length - 1]];
      },
      select(rows, fields) {
        if (!Array.isArray(rows) || !Array.isArray(fields)) return [];
        return rows.map((row) => {
          const out = {};
          for (const f of fields) {
            if (typeof f !== 'string') continue;
            const v = this._get(row, f);
            this._set(out, f, v == null ? null : v);
          }
          return out;
        });
      },
      rename(rows, mapping) {
        if (!Array.isArray(rows) || !mapping || typeof mapping !== 'object') return [];
        return rows.map((row) => {
          const next = JSON.parse(JSON.stringify(row));
          for (const from of Object.keys(mapping)) {
            const to = mapping[from];
            if (typeof to !== 'string') continue;
            const v = this._get(next, from);
            if (v !== undefined) { this._del(next, from); this._set(next, to, v); }
          }
          return next;
        });
      },
      cast(rows, mapping) {
        if (!Array.isArray(rows) || !mapping || typeof mapping !== 'object') return [];
        return rows.map((row) => {
          const next = JSON.parse(JSON.stringify(row));
          for (const field of Object.keys(mapping)) {
            const ty = String(mapping[field] || '').toLowerCase();
            const v = this._get(next, field);
            if (v == null) continue;
            let c;
            if (ty === 'string') c = String(v);
            else if (ty === 'number' || ty === 'float') c = Number(v);
            else if (ty === 'int') c = Math.trunc(Number(v));
            else if (ty === 'bool' || ty === 'boolean') c = v !== 0 && v !== '' && v !== 'false' && v !== false && v !== '0';
            else c = v;
            this._set(next, field, c);
          }
          return next;
        });
      },
      chunk(rows, size) {
        if (!Array.isArray(rows)) return [];
        const n = Math.trunc(Number(size));
        if (n <= 0) return [];
        const out = [];
        for (let i = 0; i < rows.length; i += n) out.push(rows.slice(i, i + n));
        return out;
      },
      flat_map(rows, path) {
        if (!Array.isArray(rows) || typeof path !== 'string') return [];
        const out = [];
        for (const row of rows) {
          const v = this._get(row, path);
          if (Array.isArray(v)) {
            for (const item of v) { const next = JSON.parse(JSON.stringify(row)); this._set(next, path, item); out.push(next); }
          } else if (v != null) {
            const next = JSON.parse(JSON.stringify(row)); this._set(next, path, v); out.push(next);
          }
        }
        return out;
      },
      distinct(rows, fields) {
        if (!Array.isArray(rows)) return [];
        const seen = new Set();
        const out = [];
        for (const row of rows) {
          const sig = (Array.isArray(fields) && fields.length > 0)
            ? JSON.stringify(fields.map((f) => this._get(row, f)))
            : JSON.stringify(row);
          if (!seen.has(sig)) { seen.add(sig); out.push(row); }
        }
        return out;
      },
      sort_by(rows, spec) {
        if (!Array.isArray(rows)) return [];
        const specs = Array.isArray(spec)
          ? spec.map((s) => typeof s === 'string' ? { col: s, asc: true } : (s && typeof s === 'object' ? { col: s.col, asc: String(s.dir || '').toLowerCase() !== 'desc' } : null)).filter(Boolean)
          : [];
        return rows.slice().sort((a, b) => {
          for (const { col, asc } of specs) {
            const av = this._get(a, col), bv = this._get(b, col);
            let ord = 0;
            if (typeof av === 'number' && typeof bv === 'number') ord = av - bv;
            else if (av != null && bv != null) ord = String(av) < String(bv) ? -1 : String(av) > String(bv) ? 1 : 0;
            else if (av == null && bv != null) ord = -1;
            else if (av != null && bv == null) ord = 1;
            if (ord !== 0) return asc ? ord : -ord;
          }
          return 0;
        });
      },
      group_by(rows, keys) {
        if (!Array.isArray(rows) || !Array.isArray(keys)) return [];
        const order = [], index = new Map();
        for (const row of rows) {
          const keyObj = {}, keyVals = [];
          for (const k of keys) {
            if (typeof k !== 'string') continue;
            const v = this._get(row, k);
            keyObj[k] = v; keyVals.push(v);
          }
          const sig = JSON.stringify(keyVals);
          if (index.has(sig)) { order[index.get(sig)]._rows.push(row); }
          else { index.set(sig, order.length); order.push(Object.assign({}, keyObj, { _rows: [row] })); }
        }
        return order;
      },
      aggregate(groups, spec) {
        if (!Array.isArray(groups) || !spec || typeof spec !== 'object') return [];
        return groups.map((group) => {
          const rows = Array.isArray(group._rows) ? group._rows : [];
          const next = {};
          for (const k of Object.keys(group)) { if (k !== '_rows') next[k] = group[k]; }
          for (const [outKey, expr] of Object.entries(spec)) {
            if (typeof expr !== 'string') continue;
            const lower = expr.toLowerCase().trim();
            if (lower === 'count(*)' || lower === 'count()') { next[outKey] = rows.length; continue; }
            const m = lower.match(/^(sum|avg|min|max|count)\((.+)\)$/);
            if (!m) continue;
            const [, fn, fp] = m;
            const vals = rows.map((r) => this._get(r, fp)).filter((v) => v != null);
            if (fn === 'count') next[outKey] = vals.length;
            else if (fn === 'sum') next[outKey] = vals.reduce((a, b) => a + Number(b), 0);
            else if (fn === 'avg') next[outKey] = vals.length ? vals.reduce((a, b) => a + Number(b), 0) / vals.length : 0;
            else if (fn === 'min') next[outKey] = vals.reduce((a, b) => Number(b) < Number(a) ? b : a, vals.length ? vals[0] : null);
            else if (fn === 'max') next[outKey] = vals.reduce((a, b) => Number(b) > Number(a) ? b : a, vals.length ? vals[0] : null);
          }
          return next;
        });
      },
      join(left, right, opts) {
        if (!Array.isArray(left) || !Array.isArray(right) || !opts) return [];
        const on = Array.isArray(opts.on) ? opts.on.filter((f) => typeof f === 'string') : [];
        const isLeft = String(opts.type || 'inner').toLowerCase() === 'left';
        const rightIdx = new Map();
        for (const r of right) {
          const sig = JSON.stringify(on.map((f) => this._get(r, f)));
          if (!rightIdx.has(sig)) rightIdx.set(sig, []);
          rightIdx.get(sig).push(r);
        }
        const out = [];
        for (const l of left) {
          const sig = JSON.stringify(on.map((f) => this._get(l, f)));
          const matches = rightIdx.get(sig);
          if (matches) {
            for (const r of matches) {
              const merged = JSON.parse(JSON.stringify(l));
              for (const k of Object.keys(r)) { if (k in merged) merged[`right_${k}`] = r[k]; else merged[k] = r[k]; }
              out.push(merged);
            }
          } else if (isLeft) { out.push(JSON.parse(JSON.stringify(l))); }
        }
        return out;
      },
    },
    // ── end data.* ────────────────────────────────────────────────────────────

    call(name, args, scope) {
      if (typeof name !== 'string' || name.length === 0) return null;
      if (name === 'api.request') return this.apiRequest(args && args.length > 0 ? args[0] : null);
      if (name === 'api.get') return this.apiRequest({
        method: 'GET',
        url: args && args.length > 0 ? args[0] : '',
        headers: args && args.length > 1 ? args[1] : null,
      });
      if (name === 'api.post') return this.apiRequest({
        method: 'POST',
        url: args && args.length > 0 ? args[0] : '',
        json: args && args.length > 1 ? args[1] : null,
        headers: args && args.length > 2 ? args[2] : null,
      });
      if (name.startsWith('x.')) {
        if (name === 'x.js' && args.length > 0) {
          try {
            return (new Function(String(args[0]))).call(window);
          } catch (e) {
            console.error('[JOSIE] x.js execution failed', e);
            return null;
          }
        }
        return this.callExternal(name, args, scope);
      }
      return null;
    },

    evalLite(node, scope) {
      if (node == null) return null;
      if (typeof node !== 'object') return node;
      if (Array.isArray(node)) {
        if (node.length === 0) return [];
        const op = node[0];
        const args = node.slice(1);
        if (op === 'var') {
          const p = this.evalLite(args[0], scope);
          return this.get(p, scope);
        }
        if (op === 'get') {
          return this.getFrom(this.evalLite(args[0], scope), this.evalLite(args[1], scope));
        }
        if (op === 'if') {
          return this.truthy(this.evalLite(args[0], scope))
            ? this.evalLite(args[1], scope)
            : this.evalLite(args[2], scope);
        }
        if (op === 'len') {
          const v = this.evalLite(args[0], scope);
          if (Array.isArray(v) || typeof v === 'string') return v.length;
          if (v && typeof v === 'object') return Object.keys(v).length;
          return 0;
        }
        return node.map((v) => this.evalLite(v, scope));
      }
      const out = {};
      for (const key of Object.keys(node)) {
        out[key] = this.evalLite(node[key], scope);
      }
      return out;
    },

    runStep(stepId, scope) {
      const step = this.steps[stepId];
      if (!step) return null;
      if (typeof step === 'function') {
        return step(scope || { locals: {}, event: null });
      }
      return null;
    },

    conditionPasses(raw, scope, last) {
      if (raw == null) return true;
      if (typeof raw === 'boolean') return raw;
      const condScope = Object.assign({}, scope, {
        locals: Object.assign({}, scope.locals || {}, { last })
      });
      if (typeof raw === 'string') {
        if (raw.startsWith('!')) {
          const val = this.get(raw.slice(1), condScope);
          return !this.truthy(val);
        }
        return this.truthy(this.get(raw, condScope));
      }
      return this.truthy(this.evalLite(raw, condScope));
    },

    runAction(actionName, event) {
      const action = this.actions[actionName];
      if (action === undefined) return null;
      const scope = { locals: {}, event: event || null };

      if (typeof action === 'function') {
        const compiledOut = action(event || null);
        this.flushReactiveNow();
        if (this._subs.size === 0) this.renderTree(document.body, { locals: {}, event: null });
        return compiledOut;
      }

      let out = null;
      if (typeof action === 'string') {
        out = this.runStep(action, scope);
      } else if (action && typeof action === 'object') {
        if (typeof action.runStep === 'string') {
          out = this.runStep(action.runStep, scope);
        } else if (Array.isArray(action.run)) {
          let last = null;
          for (const entry of action.run) {
            if (!entry || typeof entry !== 'object') continue;
            if (!this.conditionPasses(entry.if, scope, last)) continue;
            if (typeof entry.step === 'string') {
              last = this.runStep(entry.step, scope);
            }
          }
          out = last;
        }
      }

      this.flushReactiveNow();
      if (this._subs.size === 0) this.renderTree(document.body, { locals: {}, event: null });
      return out;
    },

    bindEvents(root, scope) {
      if (!root || root.nodeType !== 1) return;
      const all = [root].concat(Array.from(root.querySelectorAll('*')));
      for (const el of all) {
        const bound = this.boundEvents.get(el) || new Set();

        const modelPath = el.getAttribute('j-model');
        if (modelPath && !bound.has('__model_input__')) {
          el.addEventListener('input', (raw) => {
            this.set(modelPath, raw && raw.target ? raw.target.value : null);
            this.flushReactiveNow();
            if (this._subs.size === 0) this.renderTree(document.body, { locals: {}, event: null });
          });
          bound.add('__model_input__');
        }

        for (const attr of Array.from(el.attributes || [])) {
          if (!attr.name.startsWith('@')) continue;
          const evt = attr.name.slice(1).trim();
          const action = String(attr.value || '').trim();
          if (!evt || !action) continue;
          const key = `@${evt}:${action}`;
          if (bound.has(key)) continue;
          this.delegateEvent(evt);
          bound.add(key);
        }

        this.boundEvents.set(el, bound);
      }
    },

    renderTree(root, scope) {
      if (!root || root.nodeType !== 1) return;
      const localScope = scope || { locals: {}, event: null };

      const mapConfigRaw = root.getAttribute('data-josie-map');
      if (mapConfigRaw) {
        this.renderMapNode(root, mapConfigRaw, localScope);
        return;
      }

      const showPath = root.getAttribute('j-show');
      if (showPath) {
        const value = this.get(showPath, localScope);
        if (this.truthy(value)) {
          root.style.removeProperty('display');
        } else {
          root.style.setProperty('display', 'none', 'important');
        }
      }

      const textPath = root.getAttribute('j-text');
      if (textPath) {
        const value = this.get(textPath, localScope);
        root.textContent = value == null ? '' : String(value);
      }

      const modelPath = root.getAttribute('j-model');
      if (modelPath) {
        const value = this.get(modelPath, localScope);
        const next = value == null ? '' : String(value);
        if (root.value !== undefined && root.value !== next) {
          root.value = next;
        }
      }

      for (const attr of Array.from(root.attributes || [])) {
        if (!attr.name.startsWith('j-attr:')) continue;
        const prop = attr.name.slice('j-attr:'.length);
        const value = this.get(attr.value, localScope);
        const out = value == null ? '' : String(value);
        if (prop === 'class') root.className = out;
        else if (String(prop).toLowerCase().startsWith('on')) {
          continue;
        }
        else if (this.isUrlAttr(prop) && !this.isUrlAllowed(out)) {
          root.removeAttribute(prop);
          continue;
        }
        else {
          root.setAttribute(prop, out);
          if (prop === 'value' && root.value !== undefined) {
            root.value = out;
          }
        }
      }

      const children = Array.from(root.children || []);
      for (const child of children) {
        this.renderTree(child, localScope);
      }
    },

    renderMapNode(root, configRaw, scope) {
      let config;
      try {
        config = JSON.parse(configRaw);
      } catch (_e) {
        return;
      }
      if (!config || typeof config !== 'object') return;
      if (!root.__josie_template_html) {
        root.__josie_template_html = root.innerHTML;
      }

      const sourceExpr = config.source || null;
      let list = [];
      if (sourceExpr !== null) {
        if (typeof sourceExpr === 'string') {
          const value = this.get(sourceExpr, scope);
          list = Array.isArray(value) ? value : [];
        } else {
          const value = this.evalLite(sourceExpr, scope);
          list = Array.isArray(value) ? value : [];
        }
      }

      const keyExpr = config.key !== undefined ? config.key : null;
      const existing = new Map();
      for (const child of Array.from(root.children || [])) {
        const key = child.getAttribute && child.getAttribute('data-josie-key');
        if (key != null) existing.set(String(key), child);
      }

      const desired = [];
      for (let i = 0; i < list.length; i += 1) {
        const item = list[i];
        const childScope = {
          locals: Object.assign({}, scope.locals || {}, {
            item,
            index: i,
            parentItem: scope.locals ? scope.locals.item : null,
          }),
          event: null,
        };
        const key = this.resolveMapItemKey(keyExpr, item, i, childScope);

        let node = existing.get(key) || null;
        if (node) {
          existing.delete(key);
          // Delegation is already set up globally — no need to re-scan for @event attrs.
          this.renderTree(node, childScope);
        } else {
          node = this.createMapItemNode(root.__josie_template_html, key, childScope);
          if (!node) continue;
        }
        desired.push(node);
      }

      for (const stale of existing.values()) {
        if (stale && stale.parentNode === root) {
          root.removeChild(stale);
        }
      }

      for (let i = 0; i < desired.length; i += 1) {
        const node = desired[i];
        const at = root.children[i] || null;
        if (at !== node) {
          root.insertBefore(node, at);
        }
      }
    },

    resolveMapItemKey(keyExpr, item, index, childScope) {
      if (keyExpr != null) {
        if (typeof keyExpr === 'string') {
          const value = this.get(keyExpr, childScope);
          if (value != null) return String(value);
        } else {
          const value = this.evalLite(keyExpr, childScope);
          if (value != null) return String(value);
        }
      }
      if (item && typeof item === 'object' && Object.prototype.hasOwnProperty.call(item, 'id')) {
        return String(item.id);
      }
      return String(index);
    },

    createMapItemNode(templateHtml, key, childScope) {
      const tpl = document.createElement('template');
      tpl.innerHTML = templateHtml;
      const node = tpl.content.firstElementChild;
      if (!node) return null;
      node.setAttribute('data-josie-key', String(key));
      this.bindEvents(node, childScope);
      this.renderTree(node, childScope);
      return node;
    },
  };

  window.JOSIE = JOSIE;

  if (typeof document !== 'undefined') {
    if (window.__JOSIE_AUTO_INIT__ === false) {
      return;
    }
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', () => JOSIE.init(window.__JOSIE_BOOTSTRAP__ || {}));
    } else {
      JOSIE.init(window.__JOSIE_BOOTSTRAP__ || {});
    }
  }
})(window);
