/**
 * This website contains so many items on the side bar that if we select
 * an item more to the bottom, then readers see the main content of the
 * page but lose track where they are in terms of left navigation, as the
 * active item is hidden from view.
 *
 * This snippet of JavaScript is intended to make the active left item always
 * scroll into view to improve user experience.
 *
 * On mobile devices, the left sidebar is hidden, but upon opening it, the
 * active menu item should also have been scrolled into the visible area of the
 * viewport.
 *
 * NOTE: This code should not touch the right ToC navigation. That is handled
 * correctly the the Furo theme JS itself through gumshoe-patched.js. See:
 *
 * - https://pradyunsg.me/furo/contributing/internals/#contents-sidebar
 */
(function improveLeftNav() {

function onWindowLoad() {
  var curNavItem = document.querySelector('.current.reference.internal');

  curNavItem.scrollIntoView({
    behavior: 'smooth',
    block: 'center',
    inline: 'nearest',
  });
}

window.addEventListener('load', onWindowLoad);
}());
