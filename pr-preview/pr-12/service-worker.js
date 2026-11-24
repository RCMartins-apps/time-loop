// The deployment workflow replaces this token with the source revision.
const CACHE_PREFIX = `time-loop:${self.registration.scope}:`;
const CACHE_NAME = `${CACHE_PREFIX}e4af918c0536dea51be13b9a81d29758b930eb23`;
const SHELL_PATHS = [
  './', './index.html', './public/js/time-loop.js', './public/tailwind.3.4.17.js',
  './public/custom.css', './public/install.js', './public/manifest.webmanifest',
  './public/icons/icon-192.png', './public/icons/icon-512.png',
  './public/icons/icon-maskable-512.png', './public/icons/apple-touch-icon.png',
];
const shellURLs = SHELL_PATHS.map(path => new URL(path, self.registration.scope).href);

self.addEventListener('install', event => {
  event.waitUntil(caches.open(CACHE_NAME).then(cache =>
    cache.addAll(shellURLs.map(url => new Request(url, { cache: 'reload' })))
  ));
  // Let an open game finish with its current version. Updates activate after it closes.
});

self.addEventListener('activate', event => {
  event.waitUntil((async () => {
    const keys = await caches.keys();
    await Promise.all(keys.filter(key => key.startsWith(CACHE_PREFIX) && key !== CACHE_NAME)
      .map(key => caches.delete(key)));
    await self.clients.claim();
  })());
});

self.addEventListener('fetch', event => {
  const url = new URL(event.request.url);
  if (event.request.method !== 'GET' || !url.href.startsWith(self.registration.scope)) return;
  // A production worker must never serve its shell for a PR preview.
  if (url.href.slice(self.registration.scope.length).startsWith('pr-preview/')) return;
  if (event.request.mode === 'navigate') {
    event.respondWith(caches.open(CACHE_NAME).then(async cache =>
      (await cache.match(self.registration.scope)) || fetch(event.request)
    ));
  } else if (shellURLs.includes(url.href)) {
    event.respondWith(caches.open(CACHE_NAME).then(async cache =>
      (await cache.match(event.request)) || fetch(event.request)
    ));
  }
});
