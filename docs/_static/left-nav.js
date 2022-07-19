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
 *
 * For some reason, `block: 'center'` causes the main content to scroll a bit as
 * well. After some research, it seems `block: 'nearest'` is the best we can do.
 * It does not center the active menu item, but still makes it visible and at
 * least the main content does not scroll anymore.
 *
 * - https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView
 * - https://stackoverflow.com/questions/11039885/scrollintoview-causing-the-whole-page-to-move
 * - https://stackoverflow.com/questions/48634459/scrollintoview-block-vs-inline
 */
(function improveLeftNav() {

function onWindowLoad() {
  var curNavItem = document.querySelector('.current.reference.internal');

  curNavItem.scrollIntoView({
    behavior: 'smooth',
    block: 'nearest',
    inline: 'start',
  });
}

window.addEventListener('load', onWindowLoad);
}());
