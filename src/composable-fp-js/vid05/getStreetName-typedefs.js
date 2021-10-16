/**
 * @typedef {object} Street
 * @property {string} name
 *
 * @example
 * {
 *   name: 'Dagobah'
 * };
 */

/**
 * @typedef {object} Address
 * @property {number} id
 * @property {Street} street
 *
 * @example
 * {
 *   street: {
 *     name: 'Dagobah Street',
 *   },
 * };
 */

/**
 * @typedef {object} User
 * @property {number} id
 * @property {string} name
 * @property {Address} address
 *
 * @example
 * {
 *   id: 1,
 *   name: 'Yoda',
 *   address: {
 *     street: {
 *       name: 'Swampy Road',
 *     },
 *   }
 * };
 */
