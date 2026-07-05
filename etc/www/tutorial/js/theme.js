/*
 * Colour theme switcher for the Sonic Pi web tutorial.
 * Sets data-theme on <html> (light/dark/hc), remembers the choice in
 * localStorage, and defaults to the OS preference on first visit.
 */
(function () {
  var KEY = 'sonicpi-tutorial-theme';

  function preferred() {
    var saved;
    try { saved = localStorage.getItem(KEY); } catch (e) {}
    if (saved) return saved;
    if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) return 'dark';
    return 'light';
  }

  function apply(theme) {
    if (theme === 'light') {
      document.documentElement.removeAttribute('data-theme');
    } else {
      document.documentElement.setAttribute('data-theme', theme);
    }
    try { localStorage.setItem(KEY, theme); } catch (e) {}
    var btns = document.querySelectorAll('#theme-switch button');
    for (var i = 0; i < btns.length; i++) {
      btns[i].setAttribute('aria-pressed', btns[i].getAttribute('data-theme') === theme ? 'true' : 'false');
    }
  }

  // Apply as early as possible to avoid a flash of the wrong theme.
  apply(preferred());

  document.addEventListener('DOMContentLoaded', function () {
    apply(preferred());
    var btns = document.querySelectorAll('#theme-switch button');
    for (var i = 0; i < btns.length; i++) {
      btns[i].addEventListener('click', function () { apply(this.getAttribute('data-theme')); });
    }
  });
})();
