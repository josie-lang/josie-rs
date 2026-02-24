(function (window) {
  'use strict';

  const JOSIE = {
    program: {},
    state: {},
    steps: {},
    actions: {},
    fns: {},
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

    init(bootstrap) {
      const input = bootstrap || window.__JOSIE_BOOTSTRAP__ || {};
      this.program = input.program || {};
      const state = input.state || (this.program.state && (this.program.state.client || this.program.state)) || {};
      this.state = this.clone(state);
      this.actions = this.program.actions && typeof this.program.actions === 'object' ? this.program.actions : {};
      this.indexSteps();
      this.loadCompiled();
      this.initSecurity(input);
      this.initReactiveContracts();
      this.flushReactiveNow();
      this.sanitizeStaticUrls(document.body);
      this.initCompiledBindings(input);
      this.bindEvents(document.body, { locals: {}, event: null });
      this.renderTree(document.body, { locals: {}, event: null });
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
        if (trimmedPath.startsWith('[')) {
          value = this.evaluate(this.jsonParse(trimmedPath), { locals: {}, event: null });
        } else {
          value = this.resolvePath(trimmedPath, { locals: {}, event: null });
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

    resolvePath(path, scope) {
      if (typeof path !== 'string' || path.length === 0) return null;
      if (path.startsWith('!')) {
        return !this.truthy(this.resolvePath(path.slice(1), scope));
      }
      const locals = scope && scope.locals ? scope.locals : {};

      if (path.startsWith('local.')) {
        return this.readBySegments(locals, path.slice('local.'.length).split('.'));
      }
      if (path.startsWith('client.')) {
        return this.readBySegments(this.state, path.slice('client.'.length).split('.'));
      }
      if (path.startsWith('server.')) {
        return null;
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

    writeClientPath(path, value) {
      if (typeof path !== 'string' || path.length === 0) return;
      if (path.startsWith('server.') || path.startsWith('local.') || path.startsWith('w.event.')) return;
      const normalized = path.startsWith('client.') ? path.slice('client.'.length) : path;
      const parts = normalized.split('.').filter((p) => p.length > 0);
      if (parts.length === 0) return;

      let current = this.state;
      for (let i = 0; i < parts.length - 1; i += 1) {
        const key = parts[i];
        if (!current[key] || typeof current[key] !== 'object' || Array.isArray(current[key])) {
          current[key] = {};
        }
        current = current[key];
      }
      current[parts[parts.length - 1]] = value;
    },

    set(path, value) {
      this.writeClientPath(path, value);
      if (typeof path === 'string' && path.length > 0) {
        const normalized = path.startsWith('client.') ? path : ('client.' + path);
        this.enqueueReactivePath(normalized);
      }
      return value;
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
      if (!url) return { ok: false, status: 0, message: 'api.request url is required', data: null, text: '' };
      if (!this.isUrlAllowed(url)) return { ok: false, status: 0, message: 'blocked by Josie allowList', data: null, text: '' };

      const headers = cfg.headers && typeof cfg.headers === 'object' ? cfg.headers : {};
      let body = null;
      if (Object.prototype.hasOwnProperty.call(cfg, 'json')) {
        try { body = JSON.stringify(cfg.json); } catch (_) { return { ok: false, status: 0, message: 'api.request json encode failed', data: null, text: '' }; }
      } else if (Object.prototype.hasOwnProperty.call(cfg, 'body')) {
        if (typeof cfg.body === 'string') body = cfg.body;
        else {
          try { body = JSON.stringify(cfg.body); } catch (_) { body = String(cfg.body); }
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
          try { data = text ? JSON.parse(text) : null; } catch (_) { data = text; }
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
        return { ok: false, status: 0, message: String(err && err.message ? err.message : err), data: null, text: '' };
      }
    },

    evaluate(node, scope) {
      const evalScope = scope || { locals: {}, event: null };
      if (node == null) return null;
      if (typeof node !== 'object') return node;
      if (!Array.isArray(node)) {
        const out = {};
        for (const key of Object.keys(node)) {
          out[key] = this.evaluate(node[key], evalScope);
        }
        return out;
      }
      if (node.length === 0) return [];

      const op = node[0];
      const args = node.slice(1);
      if (typeof op !== 'string') {
        return node.map((v) => this.evaluate(v, evalScope));
      }

      switch (op) {
        case 'var': {
          if (args.length === 0) return null;
          const path = this.evaluate(args[0], evalScope);
          return this.resolvePath(path, evalScope);
        }
        case 'set': {
          if (args.length < 2) return null;
          const path = this.evaluate(args[0], evalScope);
          const value = this.evaluate(args[1], evalScope);
          return this.set(path, value);
        }
        case '+': return args.reduce((sum, a) => sum + Number(this.evaluate(a, evalScope) || 0), 0);
        case '-': {
          if (args.length === 0) return 0;
          const first = Number(this.evaluate(args[0], evalScope) || 0);
          if (args.length === 1) return -first;
          return first - Number(this.evaluate(args[1], evalScope) || 0);
        }
        case '*': return args.reduce((p, a) => p * Number(this.evaluate(a, evalScope) || 0), 1);
        case '/': {
          if (args.length < 2) return 0;
          const a = Number(this.evaluate(args[0], evalScope) || 0);
          const b = Number(this.evaluate(args[1], evalScope) || 0);
          return b === 0 ? 0 : a / b;
        }
        case '%': {
          if (args.length < 2) return 0;
          const a = Number(this.evaluate(args[0], evalScope) || 0);
          const b = Number(this.evaluate(args[1], evalScope) || 0);
          return b === 0 ? 0 : a % b;
        }
        case '==': return this.evaluate(args[0], evalScope) === this.evaluate(args[1], evalScope);
        case '!=': return this.evaluate(args[0], evalScope) !== this.evaluate(args[1], evalScope);
        case '>': return this.evaluate(args[0], evalScope) > this.evaluate(args[1], evalScope);
        case '<': return this.evaluate(args[0], evalScope) < this.evaluate(args[1], evalScope);
        case '>=': return this.evaluate(args[0], evalScope) >= this.evaluate(args[1], evalScope);
        case '<=': return this.evaluate(args[0], evalScope) <= this.evaluate(args[1], evalScope);
        case 'if': {
          if (this.truthy(this.evaluate(args[0], evalScope))) return args.length > 1 ? this.evaluate(args[1], evalScope) : null;
          return args.length > 2 ? this.evaluate(args[2], evalScope) : null;
        }
        case '&&': {
          let last = true;
          for (const a of args) {
            last = this.evaluate(a, evalScope);
            if (!this.truthy(last)) return false;
          }
          return last;
        }
        case '||': {
          for (const a of args) {
            const v = this.evaluate(a, evalScope);
            if (this.truthy(v)) return v;
          }
          return false;
        }
        case '!': return !this.truthy(this.evaluate(args[0], evalScope));
        case 'do': {
          let last = null;
          for (const a of args) last = this.evaluate(a, evalScope);
          return last;
        }
        case 'pipe': {
          let last = null;
          for (const a of args) {
            this.state.pipe = { prev: last };
            last = this.evaluate(a, evalScope);
          }
          delete this.state.pipe;
          return last;
        }
        case 'def': {
          if (args.length < 3) return null;
          const name = typeof args[0] === 'string' ? args[0] : null;
          if (!name) return null;
          const params = Array.isArray(args[1]) ? args[1].filter((v) => typeof v === 'string') : [];
          this.fns[name] = { params, body: args[2] };
          return null;
        }
        case 'call': {
          const name = this.evaluate(args[0], evalScope);
          if (typeof name !== 'string' || name.length === 0) return null;
          const fn = this.fns[name];
          if (fn) {
            const callLocals = Object.assign({}, evalScope.locals || {});
            for (let i = 0; i < fn.params.length; i += 1) {
              callLocals[fn.params[i]] = args[i + 1] === undefined ? null : this.evaluate(args[i + 1], evalScope);
            }
            return this.evaluate(fn.body, Object.assign({}, evalScope, { locals: callLocals }));
          }
          if (name.startsWith('x.')) {
            const evaluated = args.slice(1).map((a) => this.evaluate(a, evalScope));
            if (name === 'x.js' && evaluated.length > 0) {
              try {
                return (new Function(String(evaluated[0]))).call(window);
              } catch (e) {
                console.error('[JOSIE] x.js execution failed', e);
                return null;
              }
            }
            return this.callExternal(name, evaluated, evalScope);
          }
          if (name === 'api.request') {
            const cfg = args.length > 1 ? this.evaluate(args[1], evalScope) : null;
            return this.apiRequest(cfg);
          }
          if (name === 'api.get') {
            const url = args.length > 1 ? this.evaluate(args[1], evalScope) : '';
            const headers = args.length > 2 ? this.evaluate(args[2], evalScope) : null;
            return this.apiRequest({ method: 'GET', url, headers });
          }
          if (name === 'api.post') {
            const url = args.length > 1 ? this.evaluate(args[1], evalScope) : '';
            const json = args.length > 2 ? this.evaluate(args[2], evalScope) : null;
            const headers = args.length > 3 ? this.evaluate(args[3], evalScope) : null;
            return this.apiRequest({ method: 'POST', url, json, headers });
          }
          return null;
        }
        case 'map': {
          const list = this.evaluate(args[0], evalScope);
          if (!Array.isArray(list)) return [];
          const expr = args[1];
          return list.map((item, index) => this.evaluate(expr, Object.assign({}, evalScope, {
            locals: Object.assign({}, evalScope.locals || {}, { item, index })
          })));
        }
        case 'filter': {
          const list = this.evaluate(args[0], evalScope);
          if (!Array.isArray(list)) return [];
          const expr = args[1];
          const out = [];
          for (let i = 0; i < list.length; i += 1) {
            const item = list[i];
            const keep = this.evaluate(expr, Object.assign({}, evalScope, {
              locals: Object.assign({}, evalScope.locals || {}, { item, index: i })
            }));
            if (this.truthy(keep)) out.push(item);
          }
          return out;
        }
        case 'for_each': {
          const list = this.evaluate(args[0], evalScope);
          if (!Array.isArray(list)) return null;
          const expr = args[1];
          let last = null;
          for (let i = 0; i < list.length; i += 1) {
            last = this.evaluate(expr, Object.assign({}, evalScope, {
              locals: Object.assign({}, evalScope.locals || {}, { item: list[i], index: i })
            }));
          }
          return last;
        }
        case 'reduce': {
          const list = this.evaluate(args[0], evalScope);
          if (!Array.isArray(list)) return null;
          const expr = args[1];
          let acc = args.length > 2 ? this.evaluate(args[2], evalScope) : null;
          for (let i = 0; i < list.length; i += 1) {
            acc = this.evaluate(expr, Object.assign({}, evalScope, {
              locals: Object.assign({}, evalScope.locals || {}, { item: list[i], index: i, acc })
            }));
          }
          return acc;
        }
        case 'match': {
          const value = this.evaluate(args[0], evalScope);
          for (let i = 1; i + 1 < args.length; i += 2) {
            const pat = args[i];
            if (pat === '_' || pat === '"_"') return this.evaluate(args[i + 1], evalScope);
            const patVal = this.evaluate(pat, evalScope);
            if (patVal === '_' || patVal === value) return this.evaluate(args[i + 1], evalScope);
          }
          return null;
        }
        case 'w.event.value': return evalScope.event ? evalScope.event.value : null;
        case 'w.event.key': return evalScope.event ? evalScope.event.key : null;
        case 'w.event.prevent': {
          if (evalScope.event && evalScope.event.raw && typeof evalScope.event.raw.preventDefault === 'function') {
            evalScope.event.raw.preventDefault();
            return true;
          }
          return false;
        }
        case 'w.after': {
          const ms = Number(this.evaluate(args[0], evalScope) || 0);
          const expr = args[1];
          const next = Object.assign({}, evalScope, { event: null });
          window.setTimeout(() => {
            this.evaluate(expr, next);
            this.flushReactiveNow();
            if (this._subs.size === 0) this.renderTree(document.body, { locals: {}, event: null });
          }, Number.isFinite(ms) ? Math.max(0, ms) : 0);
          return null;
        }
        case 'w.raf': {
          const expr = args[0];
          const next = Object.assign({}, evalScope, { event: null });
          const run = () => {
            this.evaluate(expr, next);
            this.flushReactiveNow();
            if (this._subs.size === 0) this.renderTree(document.body, { locals: {}, event: null });
          };
          if (typeof window.requestAnimationFrame === 'function') window.requestAnimationFrame(run);
          else window.setTimeout(run, 16);
          return null;
        }
        case 'log': {
          const values = args.map((a) => this.evaluate(a, evalScope));
          if (window.console && console.log) console.log('[JOSIE]', ...values);
          return values.length > 0 ? values[0] : null;
        }
        case 'effect': return this.evaluate(['do'].concat(args), evalScope);
        case 'len': {
          const value = this.evaluate(args[0], evalScope);
          if (Array.isArray(value) || typeof value === 'string') return value.length;
          if (value && typeof value === 'object') return Object.keys(value).length;
          return 0;
        }
        case 'push': {
          const list = this.evaluate(args[0], evalScope);
          const item = this.evaluate(args[1], evalScope);
          if (!Array.isArray(list)) return [item];
          return list.concat([item]);
        }
        case 'get': {
          const collection = this.evaluate(args[0], evalScope);
          const key = this.evaluate(args[1], evalScope);
          if (Array.isArray(collection)) {
            const idx = Number(key);
            return Number.isInteger(idx) ? (collection[idx] ?? null) : null;
          }
          if (collection && typeof collection === 'object') {
            const mapKey = String(key);
            return Object.prototype.hasOwnProperty.call(collection, mapKey) ? collection[mapKey] : null;
          }
          return null;
        }
        case 'util.concat': return args.map((a) => String(this.evaluate(a, evalScope) ?? '')).join('');
        case 'util.lower': return String(this.evaluate(args[0], evalScope) ?? '').toLowerCase();
        case 'util.upper': return String(this.evaluate(args[0], evalScope) ?? '').toUpperCase();
        case 'util.contains': {
          const hay = this.evaluate(args[0], evalScope);
          const needle = this.evaluate(args[1], evalScope);
          if (typeof hay === 'string') return hay.includes(String(needle));
          if (Array.isArray(hay)) return hay.includes(needle);
          return false;
        }
        case 'util.template': {
          let out = String(this.evaluate(args[0], evalScope) ?? '');
          for (let i = 1; i < args.length; i += 1) {
            if (!out.includes('{}')) break;
            out = out.replace('{}', String(this.evaluate(args[i], evalScope) ?? ''));
          }
          return out;
        }
        case 'util.to_int': {
          const n = Number.parseInt(String(this.evaluate(args[0], evalScope) ?? '0'), 10);
          return Number.isFinite(n) ? n : 0;
        }
        case 'util.to_float': {
          const n = Number.parseFloat(String(this.evaluate(args[0], evalScope) ?? '0'));
          return Number.isFinite(n) ? n : 0;
        }
        case 'util.to_string': return String(this.evaluate(args[0], evalScope) ?? '');
        case 'util.json_parse': {
          try { return JSON.parse(String(this.evaluate(args[0], evalScope) ?? '')); } catch (_) { return null; }
        }
        case 'util.json_stringify': {
          const value = this.evaluate(args[0], evalScope);
          const pretty = Number(args.length > 1 ? this.evaluate(args[1], evalScope) : 0);
          const space = Number.isFinite(pretty) ? Math.max(0, Math.min(8, Math.trunc(pretty))) : 0;
          try { return JSON.stringify(value, null, space); } catch (_) { return ''; }
        }
        case 'util.trim': return String(this.evaluate(args[0], evalScope) ?? '').trim();
        case 'util.str_len': return String(this.evaluate(args[0], evalScope) ?? '').length;
        case 'util.get_path': {
          const base = this.evaluate(args[0], evalScope);
          const path = String(this.evaluate(args[1], evalScope) ?? '');
          if (!path) return base;
          return this.readBySegments(base, path.split('.'));
        }
        case 'util.as_array': {
          const value = this.evaluate(args[0], evalScope);
          if (value == null) return [];
          return Array.isArray(value) ? value : [value];
        }
        case 'util.to_bool': return this.truthy(this.evaluate(args[0], evalScope));
        case 'util.replace': {
          const s = String(this.evaluate(args[0], evalScope) ?? '');
          const from = String(this.evaluate(args[1], evalScope) ?? '');
          const to = String(this.evaluate(args[2], evalScope) ?? '');
          if (from.length === 0) return s;
          return s.replace(from, to);
        }
        case 'util.replace_all': {
          const s = String(this.evaluate(args[0], evalScope) ?? '');
          const from = String(this.evaluate(args[1], evalScope) ?? '');
          const to = String(this.evaluate(args[2], evalScope) ?? '');
          if (from.length === 0) return s;
          return s.split(from).join(to);
        }
        case 'util.split': {
          const s = String(this.evaluate(args[0], evalScope) ?? '');
          const delim = String(this.evaluate(args[1], evalScope) ?? '');
          if (delim.length === 0) return s.split('');
          return s.split(delim);
        }
        case 'util.join': {
          const arr = this.evaluate(args[0], evalScope);
          const delim = String(this.evaluate(args[1], evalScope) ?? '');
          if (!Array.isArray(arr)) return '';
          return arr.map((v) => (v == null ? '' : String(v))).join(delim);
        }
        case 'util.regex_match': {
          const s = String(this.evaluate(args[0], evalScope) ?? '');
          const p = String(this.evaluate(args[1], evalScope) ?? '');
          const opts = this.parseRegexOpts(args.length > 2 ? this.evaluate(args[2], evalScope) : null);
          try { return this.buildRegex(p, opts, false).test(s); } catch (_) { return false; }
        }
        case 'util.regex_find': {
          const s = String(this.evaluate(args[0], evalScope) ?? '');
          const p = String(this.evaluate(args[1], evalScope) ?? '');
          const opts = this.parseRegexOpts(args.length > 2 ? this.evaluate(args[2], evalScope) : null);
          try {
            const m = s.match(this.buildRegex(p, opts, false));
            return m && m[0] !== undefined ? m[0] : null;
          } catch (_) { return null; }
        }
        case 'util.regex_find_all': {
          const s = String(this.evaluate(args[0], evalScope) ?? '');
          const p = String(this.evaluate(args[1], evalScope) ?? '');
          const opts = this.parseRegexOpts(args.length > 2 ? this.evaluate(args[2], evalScope) : null);
          try {
            const re = this.buildRegex(p, opts, true);
            const out = [];
            let m;
            while ((m = re.exec(s)) !== null) {
              out.push(m[0]);
              if (m[0] === '') re.lastIndex += 1;
            }
            return out;
          } catch (_) { return []; }
        }
        case 'util.regex_replace': {
          const s = String(this.evaluate(args[0], evalScope) ?? '');
          const p = String(this.evaluate(args[1], evalScope) ?? '');
          const r = String(this.evaluate(args[2], evalScope) ?? '');
          const opts = this.parseRegexOpts(args.length > 3 ? this.evaluate(args[3], evalScope) : null);
          try {
            const re = this.buildRegex(p, opts, !!opts.all);
            return s.replace(re, r);
          } catch (_) { return s; }
        }
        default: {
          if (op.startsWith('x.')) {
            const evaluated = args.map((a) => this.evaluate(a, evalScope));
            return this.callExternal(op, evaluated, evalScope);
          }
          return node.map((v) => this.evaluate(v, evalScope));
        }
      }
    },

    evaluateStep(step, scope) {
      if (!step || typeof step !== 'object') return null;
      const op = typeof step.op === 'string' ? step.op : '';
      if (step.when && !this.truthy(this.evaluate(step.when, scope))) {
        return null;
      }

      if (op === 'set') {
        const target = step.into || (Array.isArray(step.args) ? this.evaluate(step.args[0], scope) : null);
        const expr = Array.isArray(step.args) ? step.args[0] : step.do;
        if (typeof target !== 'string' || expr === undefined) return null;
        return this.evaluate(['set', target, expr], scope);
      }

      if (op === 'call') {
        const from = typeof step.from === 'string' ? step.from : null;
        if (!from) return null;
        const args = Array.isArray(step.args) ? step.args : [];
        return this.evaluate(['call', from].concat(args), scope);
      }

      if (op === 'return') {
        if (typeof step.from === 'string') return this.resolvePath(step.from, scope);
        if (Array.isArray(step.args) && step.args.length > 0) return this.evaluate(step.args[0], scope);
        return null;
      }

      if (op === 'do' || op === 'if' || op === 'match' || op === 'map' || op === 'filter' || op === 'for_each' || op === 'reduce' || op === 'get') {
        const args = Array.isArray(step.args) ? step.args : [];
        return this.evaluate([op].concat(args), scope);
      }

      if (step.do !== undefined) return this.evaluate(step.do, scope);
      if (Array.isArray(step.args) && step.args.length > 0) return this.evaluate(step.args[0], scope);
      return null;
    },

    runStep(stepId, scope) {
      const step = this.steps[stepId];
      if (!step) return null;
      if (typeof step === 'function') {
        return step(scope || { locals: {}, event: null });
      }
      return this.evaluateStep(step, scope || { locals: {}, event: null });
    },

    conditionPasses(raw, scope, last) {
      if (raw == null) return true;
      if (typeof raw === 'boolean') return raw;
      const condScope = Object.assign({}, scope, {
        locals: Object.assign({}, scope.locals || {}, { last })
      });
      if (typeof raw === 'string') {
        if (raw.startsWith('!')) {
          const val = this.resolvePath(raw.slice(1), condScope);
          return !this.truthy(val);
        }
        return this.truthy(this.resolvePath(raw, condScope));
      }
      return this.truthy(this.evaluate(raw, condScope));
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
      } else if (Array.isArray(action)) {
        out = this.evaluate(action, scope);
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
              continue;
            }
            if (entry.expr !== undefined) {
              last = this.evaluate(entry.expr, scope);
              continue;
            }
          }
          out = last;
        } else if (action.expr !== undefined) {
          out = this.evaluate(action.expr, scope);
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

      const textPath = root.getAttribute('j-text');
      if (textPath) {
        const value = this.resolvePath(textPath, localScope);
        root.textContent = value == null ? '' : String(value);
      }

      const modelPath = root.getAttribute('j-model');
      if (modelPath) {
        const value = this.resolvePath(modelPath, localScope);
        const next = value == null ? '' : String(value);
        if (root.value !== undefined && root.value !== next) {
          root.value = next;
        }
      }

      for (const attr of Array.from(root.attributes || [])) {
        if (!attr.name.startsWith('j-attr:')) continue;
        const prop = attr.name.slice('j-attr:'.length);
        const value = this.resolvePath(attr.value, localScope);
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
        const value = this.evaluate(sourceExpr, scope);
        list = Array.isArray(value) ? value : [];
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
          this.bindEvents(node, childScope);
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
          const value = this.resolvePath(keyExpr, childScope);
          if (value != null) return String(value);
        } else {
          const value = this.evaluate(keyExpr, childScope);
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
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', () => JOSIE.init(window.__JOSIE_BOOTSTRAP__ || {}));
    } else {
      JOSIE.init(window.__JOSIE_BOOTSTRAP__ || {});
    }
  }
})(window);
