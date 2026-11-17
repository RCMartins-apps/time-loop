(() => {
  'use strict';
  const scriptURL = new URL(document.currentScript.src);
  const appURL = new URL('../', scriptURL);
  const button = document.getElementById('install-game');
  const dialog = document.getElementById('install-help');
  const standalone = () => window.matchMedia('(display-mode: standalone)').matches || navigator.standalone;
  let installPrompt;

  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register(new URL('service-worker.js', appURL), { scope: appURL.href })
      .catch(error => console.warn('Offline support unavailable:', error));
  }

  window.addEventListener('beforeinstallprompt', event => {
    event.preventDefault();
    installPrompt = event;
    button.hidden = standalone();
  });
  window.addEventListener('appinstalled', () => {
    installPrompt = undefined;
    button.hidden = true;
  });
  window.matchMedia('(display-mode: standalone)').addEventListener('change', event => {
    if (event.matches) button.hidden = true;
  });

  const ios = /iPad|iPhone|iPod/.test(navigator.userAgent) ||
    (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1);
  if (ios && !standalone()) button.hidden = false;

  button.addEventListener('click', async () => {
    if (installPrompt) {
      const prompt = installPrompt;
      installPrompt = undefined;
      button.hidden = true;
      try {
        await prompt.prompt();
        await prompt.userChoice;
      } catch (error) {
        console.warn('Installation unavailable:', error);
      }
    } else if (ios) {
      dialog.showModal();
    }
  });
  document.getElementById('close-install-help').addEventListener('click', () => dialog.close());
})();
