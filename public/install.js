(() => {
  'use strict';
  const appURL = new URL('../', document.currentScript.src);
  const shortcut = document.getElementById('install-shortcut');
  const dialog = document.getElementById('install-help');
  const preferenceKey = `time-loop:install-shortcut:${appURL.pathname}`;
  const standalone = () => window.matchMedia('(display-mode: standalone)').matches || navigator.standalone;
  const ios = /iPad|iPhone|iPod/.test(navigator.userAgent) ||
    (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1);
  let installPrompt;
  let installed = false;
  let hidden = false;
  try { hidden = localStorage.getItem(preferenceKey) === 'hidden'; } catch (_) { /* Keep the preference for this visit when storage is unavailable. */ }

  function sync() {
    shortcut.hidden = hidden || installed || standalone() || (!installPrompt && !ios);
    document.querySelectorAll('[data-install-action]').forEach(button => {
      const isInstall = button.dataset.installAction === 'install';
      const label = isInstall ? ((installed || standalone()) ? 'Game installed' : 'Install game') :
        (hidden ? 'Show install shortcut' : 'Hide install shortcut');
      if (button.textContent !== label) button.textContent = label;
      button.disabled = !!(installed || standalone());
      if (!isInstall) button.setAttribute('aria-pressed', String(!hidden));
    });
  }

  function hideShortcut(value) {
    hidden = value;
    try { localStorage.setItem(preferenceKey, hidden ? 'hidden' : 'visible'); } catch (_) { /* In-memory preference still works. */ }
    sync();
  }

  async function install() {
    if (standalone() || installed) return;
    if (installPrompt) {
      const prompt = installPrompt;
      installPrompt = undefined;
      // Choosing or dismissing the browser prompt also removes the floating shortcut.
      hideShortcut(true);
      try {
        await prompt.prompt();
        await prompt.userChoice;
      } catch (error) {
        console.warn('Installation unavailable:', error);
      }
    } else {
      document.getElementById('install-help-text').textContent = ios ?
        'Open this game in Safari, tap Share, then choose Add to Home Screen and tap Add.' :
        'Use your browser’s Install app option in the address bar or menu. If it is not offered, open the game in a supported browser such as Chrome or Edge. You can keep playing here without installing.';
      dialog.showModal();
    }
  }

  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register(new URL('service-worker.js', appURL), { scope: appURL.href })
      .catch(error => console.warn('Offline support unavailable:', error));
  }
  window.addEventListener('beforeinstallprompt', event => {
    event.preventDefault();
    installPrompt = event;
    sync();
  });
  window.addEventListener('appinstalled', () => {
    installPrompt = undefined;
    installed = true;
    sync();
  });
  window.matchMedia('(display-mode: standalone)').addEventListener('change', sync);
  document.getElementById('install-game').addEventListener('click', install);
  document.getElementById('hide-install-shortcut').addEventListener('click', () => hideShortcut(true));
  document.getElementById('close-install-help').addEventListener('click', () => dialog.close());
  // Settings are mounted and unmounted as the player switches tabs/layouts.
  document.addEventListener('click', event => {
    const button = event.target.closest('[data-install-action]');
    if (!button || button.disabled) return;
    if (button.dataset.installAction === 'install') install();
    else hideShortcut(!hidden);
  });
  new MutationObserver(sync).observe(document.getElementById('main-div'), { childList: true, subtree: true });
  sync();
})();
