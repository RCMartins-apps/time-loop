const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const manifest = JSON.parse(fs.readFileSync('public/manifest.webmanifest'));
const worker = fs.readFileSync('service-worker.js', 'utf8');

function setup(scope) {
  const handlers = {};
  const deleted = [];
  const cached = [];
  const self = { registration: { scope }, clients: { claim: async () => {} },
    addEventListener: (name, handler) => { handlers[name] = handler; } };
  const prefix = `time-loop:${scope}:`;
  vm.runInNewContext(worker, { self, URL, Request, fetch: async () => 'network',
    caches: {
      open: async () => ({ addAll: async requests => cached.push(...requests.map(r => r.url)), match: async () => 'cached-shell' }),
      keys: async () => [`${prefix}old`, `${prefix}__PWA_VERSION__`, 'unrelated-app', 'time-loop:https://example.com/time-loop/pr-preview/pr-10/:old'],
      delete: async key => deleted.push(key),
    },
  });
  return { handlers, deleted, cached };
}

test('manifest resolves installation to the correct production or preview directory', () => {
  for (const root of ['https://example.com/time-loop/', 'https://example.com/time-loop/pr-preview/pr-10/']) {
    const location = new URL('public/manifest.webmanifest', root);
    for (const field of ['id', 'scope', 'start_url']) assert.equal(new URL(manifest[field], location).href, root);
    for (const icon of manifest.icons) assert(fs.existsSync(`public/${icon.src}`));
  }
  assert.equal(manifest.display, 'standalone');
  assert(manifest.icons.some(icon => icon.sizes === '192x192'));
  assert(manifest.icons.some(icon => icon.sizes === '512x512'));
});

test('offline installation caches every required asset within its own scope', async () => {
  const { handlers, cached } = setup('https://example.com/time-loop/');
  let done;
  handlers.install({ waitUntil: task => { done = task; } });
  await done;
  assert(cached.includes('https://example.com/time-loop/public/js/time-loop.js'));
  for (const url of cached) {
    const file = new URL(url).pathname.replace('/time-loop/', '') || 'index.html';
    // Generated game JavaScript is produced before deployment.
    if (file !== 'public/js/time-loop.js') assert(fs.existsSync(file), file);
  }
});

test('updates delete only this installation’s old caches', async () => {
  const { handlers, deleted } = setup('https://example.com/time-loop/');
  let done;
  handlers.activate({ waitUntil: task => { done = task; } });
  await done;
  assert.deepEqual(deleted, ['time-loop:https://example.com/time-loop/:old']);
});

test('production cannot intercept preview navigation, while its own navigation works offline', async () => {
  const { handlers } = setup('https://example.com/time-loop/');
  let response;
  handlers.fetch({ request: { method: 'GET', mode: 'navigate', url: 'https://example.com/time-loop/pr-preview/pr-10/' }, respondWith: task => { response = task; } });
  assert.equal(response, undefined);
  handlers.fetch({ request: { method: 'GET', mode: 'navigate', url: 'https://example.com/time-loop/' }, respondWith: task => { response = task; } });
  assert.equal(await response, 'cached-shell');
});
