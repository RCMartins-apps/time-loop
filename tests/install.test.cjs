const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync('public/install.js', 'utf8');

function setup({ storage = new Map(), path = '/time-loop/', ios = false, blockedStorage = false } = {}) {
  const events = {};
  const elements = {};
  function element() {
    return { hidden: true, textContent: '', dataset: {}, handlers: {},
      addEventListener(name, handler) { this.handlers[name] = handler; },
      setAttribute() {}, showModal() { this.open = true; }, close() { this.open = false; } };
  }
  for (const id of ['install-shortcut', 'install-help', 'install-game', 'hide-install-shortcut', 'close-install-help', 'install-help-text', 'main-div']) elements[id] = element();
  const install = element(); install.dataset.installAction = 'install';
  const toggle = element(); toggle.dataset.installAction = 'toggle-shortcut';
  let mounted = [];
  const window = { addEventListener: (name, handler) => { events[name] = handler; },
    matchMedia: () => ({ matches: false, addEventListener() {} }) };
  const document = { currentScript: { src: `https://example.com${path}public/install.js` },
    getElementById: id => elements[id], querySelectorAll: () => mounted,
    addEventListener: (name, handler) => { events[`document:${name}`] = handler; } };
  let mutationCallback;
  vm.runInNewContext(source, { URL, window, document, navigator: { userAgent: ios ? 'iPhone' : 'Chrome', platform: '', maxTouchPoints: 0 },
    localStorage: { getItem: key => { if (blockedStorage) throw Error('blocked'); return storage.get(key); },
      setItem: (key, value) => { if (blockedStorage) throw Error('blocked'); storage.set(key, value); } },
    MutationObserver: class { constructor(callback) { mutationCallback = callback; } observe() {} }, console,
  });
  return { elements, events, install, toggle, storage,
    mountSettings() { mounted = [install, toggle]; mutationCallback(); },
    clickSettings(button) { events['document:click']({ target: { closest: () => button } }); },
    offerInstall() { let prompted = 0; events.beforeinstallprompt({ preventDefault() {}, prompt: async () => { prompted++; }, userChoice: Promise.resolve({ outcome: 'dismissed' }) }); return () => prompted; },
  };
}

test('hide persists across visits and does not hide the Settings install option', () => {
  const app = setup({ ios: true });
  assert.equal(app.elements['install-shortcut'].hidden, false);
  app.elements['hide-install-shortcut'].handlers.click();
  assert.equal(app.elements['install-shortcut'].hidden, true);
  const reload = setup({ storage: app.storage, ios: true });
  reload.offerInstall();
  assert.equal(reload.elements['install-shortcut'].hidden, true);
  reload.mountSettings();
  assert.equal(reload.install.textContent, 'Install game');
  assert.equal(reload.toggle.textContent, 'Show install shortcut');
  reload.clickSettings(reload.toggle);
  assert.equal(reload.elements['install-shortcut'].hidden, false);
});

test('a dismissed shortcut can still install from newly mounted Settings', () => {
  const app = setup(); const prompted = app.offerInstall();
  app.elements['hide-install-shortcut'].handlers.click();
  app.mountSettings(); app.clickSettings(app.install);
  assert.equal(prompted(), 1);
  assert.equal(app.elements['install-shortcut'].hidden, true);
});

test('Settings always provides guidance when a native install prompt is unavailable', () => {
  const app = setup(); app.mountSettings(); app.clickSettings(app.install);
  assert.equal(app.elements['install-help'].open, true);
  assert.match(app.elements['install-help-text'].textContent, /browser/);
});

test('shortcut preferences are isolated per preview and work with blocked storage', () => {
  const first = setup({ ios: true }); first.elements['hide-install-shortcut'].handlers.click();
  const preview = setup({ ios: true, storage: first.storage, path: '/time-loop/pr-preview/pr-10/' });
  assert.equal(preview.elements['install-shortcut'].hidden, false);
  const blocked = setup({ ios: true, blockedStorage: true });
  blocked.elements['hide-install-shortcut'].handlers.click();
  assert.equal(blocked.elements['install-shortcut'].hidden, true);
});
