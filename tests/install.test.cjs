const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync('public/install.js', 'utf8');

function setup({ ios = false } = {}) {
  const events = {};
  const elements = {};
  function element() {
    return { hidden: true, textContent: '', dataset: {}, handlers: {},
      addEventListener(name, handler) { this.handlers[name] = handler; },
      setAttribute() {}, showModal() { this.open = true; }, close() { this.open = false; } };
  }
  for (const id of ['install-help', 'close-install-help', 'install-help-text', 'main-div']) elements[id] = element();
  const install = element(); install.dataset.installAction = 'install';
  let mounted = [];
  const window = { addEventListener: (name, handler) => { events[name] = handler; },
    matchMedia: () => ({ matches: false, addEventListener() {} }) };
  const document = { currentScript: { src: 'https://example.com/time-loop/public/install.js' },
    getElementById: id => elements[id], querySelectorAll: () => mounted,
    addEventListener: (name, handler) => { events[`document:${name}`] = handler; } };
  let mutationCallback;
  vm.runInNewContext(source, { URL, window, document, navigator: { userAgent: ios ? 'iPhone' : 'Chrome', platform: '', maxTouchPoints: 0 },
    MutationObserver: class { constructor(callback) { mutationCallback = callback; } observe() {} }, console,
  });
  return { elements, events, install,
    mountSettings() { mounted = [install]; mutationCallback(); },
    clickSettings(button) { events['document:click']({ target: { closest: () => button } }); },
    offerInstall() { let prompted = 0; events.beforeinstallprompt({ preventDefault() {}, prompt: async () => { prompted++; }, userChoice: Promise.resolve({ outcome: 'dismissed' }) }); return () => prompted; },
  };
}

test('Settings triggers the native install prompt without adding a floating control', () => {
  const app = setup(); const prompted = app.offerInstall();
  app.mountSettings(); app.clickSettings(app.install);
  assert.equal(prompted(), 1);
  assert.equal(fs.readFileSync('index.html', 'utf8').includes('id="install-shortcut"'), false);
});

test('Settings provides instructions when the browser prompt is unavailable', () => {
  const app = setup(); app.mountSettings(); app.clickSettings(app.install);
  assert.equal(app.elements['install-help'].open, true);
  assert.match(app.elements['install-help-text'].textContent, /browser/);
});

test('installed games disable the Settings install action after remounting', () => {
  const app = setup(); app.events.appinstalled(); app.mountSettings();
  assert.equal(app.install.textContent, 'Game installed');
  assert.equal(app.install.disabled, true);
});
