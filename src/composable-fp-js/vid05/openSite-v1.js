import { showLogin, renderPage } from './openSite-helpers.js';

/**
 * Given the user, renders the welcome or the login page.
 *
 * ASSUME: If we have an user, it is a logged in user.
 *
 * @param {undefiend|object}
 * @return {string}
 */
function openSite(user) {
  return user ? renderPage() : showLogin();
}

export { openSite };
