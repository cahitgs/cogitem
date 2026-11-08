// Cogitem app: response timing, hover trace, one answer per item, keyboard
// shortcuts in the administration (1-8 choose an option, Enter or Space
// continues after feedback).
(function () {
  window.cogStart = Date.now();
  window.cogHovers = [];
  window.cogItem = 0;
  window.cogLocked = false;
  window.cogN = 0;
  window.cogEnabled = false;

  function init() {
    Shiny.addCustomMessageHandler('cog-item-start', function (m) {
      window.cogStart = Date.now();
      window.cogHovers = [];
      window.cogItem = m.item;
      window.cogLocked = false;
    });
  }
  if (window.Shiny) init(); else document.addEventListener('DOMContentLoaded', init);

  window.cogHover = function (i) {
    if (!window.cogHovers) window.cogHovers = [];
    window.cogHovers.push({ i: i, t: Date.now() - window.cogStart });
  };

  // The first click locks the options until the server starts the next item,
  // and every event names the item it belongs to.
  window.cogPick = function (i, enabled) {
    if (!enabled || window.cogLocked) return;
    window.cogLocked = true;
    Shiny.setInputValue('admin_pick', {
      i: i, item: window.cogItem, rt: Date.now() - window.cogStart,
      hovers: window.cogHovers || []
    }, { priority: 'event' });
  };

  window.cogSkip = function () {
    if (window.cogLocked) return;
    window.cogLocked = true;
    Shiny.setInputValue('admin_skip', {
      item: window.cogItem, rt: Date.now() - window.cogStart,
      hovers: window.cogHovers || []
    }, { priority: 'event' });
  };

  document.addEventListener('keydown', function (e) {
    var tag = (e.target && e.target.tagName) || '';
    if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT' || e.ctrlKey || e.metaKey || e.altKey) return;
    if (/^[1-8]$/.test(e.key)) {
      var i = parseInt(e.key, 10);
      if (window.cogEnabled && i <= window.cogN) {
        window.cogPick(i, true);
        e.preventDefault();
      }
      return;
    }
    if (e.key === 'Enter' || e.key === ' ') {
      var next = document.getElementById('admin_next');
      if (next) {
        next.click();
        e.preventDefault();
      }
    }
  });
})();
