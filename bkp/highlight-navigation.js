/**
 * My attempt at making a navigation menu highlighter in VanillaJS :D
 *
 * This script _does not_ generate the navigation menu. That is done by
 * Asciidoctor (in this project). The script just highlights the navigation
 * item that matches the currently visible heading in th main content
 * area of the document.
 *
 * Inspired by these projects:
 *
 * JavaScript Garden
 * -----------------
 *
 *   http://bonsaiden.github.io/JavaScript-Garden/
 *
 * Tocify, used in Anki Docs
 * --------------------------
 *
 *   http://gregfranko.com/jquery.tocify.js/
 *   https://apps.ankiweb.net/docs/manual.html
 *
 */


const l = console.log.bind(console);

/**
* Limits the amount of times a function is invoked.
*
* @param {function} - the function to be throttled.
* @param {number} wait - in milliseconds.
*/
const throttle = function throttle (fn, wait = 0) {
  let time = Date.now();
  return function throttler () {
    if ((time + wait - Date.now() < 0)) {
      fn  ();
      time = Date.now();
    }
  }
};

/**
 * Ensures the input is output as an array.
 *
 * If the input object is already an array, return it as is, otherwise,
 * turn it into an array before returning.
 *
 * @param {HTMLElement|array[HTMLElement]} xOrXs
 * @return {array[HTMLElement]}
 */
const toArray = function toArray (xOrXs) {
  return xOrXs.constructor === Array ? xOrXs : [xOrXs];
};

/**
 * Adds a css class to the element(s) passed.
 *
 * @param {HTMLElements|array[HTMLElements]} elems
 * @param {string} klass
 * @return {array[HTMLElements]}
 */
const classAdd = function classAdd (elems, klass = 'active') {
  toArray(elems).forEach((elem) => {
    elem.classList.add(klass);
  });

  return elems;
};

/**
 * Removes a css class from the element(s) passed.
 *
 * @param {HTMLElements|array[HTMLElements]} elems
 * @param {string} klass
 * @return {array[HTMLElements]}
 */
const classRemove = function classRemove (elems, klass = 'active') {
  toArray(elems).forEach((elem) => {
    elem.classList.remove(klass);
  });

  return elems;
};

/**
 * Mark a link as active.
 *
 * @param {array} navLinks
 * @param {HTMLElement} heading'
 */
const markActive = function markActive (navLinks, heading) {
  for (let i = 0; i < navLinks.length; ++i) {
    const { href } = navLinks[i];
    if (href.includes(heading.getAttribute('id'))) {
      classRemove(navLinks)
      classAdd(navLinks[i])
      l('matched', heading);
      l('breaking markActive', );
      break;
    }
  }
}

// Get all navigation <a> elements.
const anchors = [...document.querySelectorAll('#header .toc2 a')];

// Get all headings on the content area.
const headings = [...document.querySelectorAll('#content h2, #content h3, #content h4')];

const run = function makeActive () {
  const { scrollX, innerHeight } = window;
  const { scrollTop } = document.documentElement;
  const scrollBottom = (window.innerHeight + window.pageYOffset) >= document.body.offsetHeight;

  // headings.forEach((heading) => {
  outter:
  for (let h = 0; h < headings.length; ++h) {
    const { top, bottom } = headings[h].getBoundingClientRect();

    if (scrollTop === 0) {
      classRemove(anchors);
      classAdd([anchors[0]]);
      return;
    }

    if (scrollBottom) {
      classRemove(anchors);
      classAdd([anchors[anchors.length - 1]]);
      return;
    }


    // l({
    //     text: headings[h].textContent,
    //     top: parseInt(top),
    //     bottom: parseInt(bottom),
    //     innerHeight: parseInt(innerHeight / 2),
    // });

    if (top >= 0 && bottom <= innerHeight / 2 ) {
      // l(parseInt(top), parseInt(bottom), parseInt(innerHeight / 2));
      markActive(anchors, headings[h]);
      break;
    }
  };
};

(function boot () {
  let timerId = null;

  window.addEventListener('scroll', function () {
    if (timerId !== null) clearTimeout(timerId);
    timerId = setTimeout(run, 50);
  }, false);

  // Run once initally.
  run();
})();
