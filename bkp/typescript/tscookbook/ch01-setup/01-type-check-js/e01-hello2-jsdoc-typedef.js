// @ts-check

import { log } from "../lib/loggers";

/**
 * @typedef {Object} Jedi
 * @property {number} id
 * @property {string} name
 * @property {number} level
 * @property {Array<string>} skills
 * @property {number} [damage = 0]
 */

/** @type Jedi */
const aayla = {
  id: 1,
  name: "Aayla Secura",
  level: 79,
  skills: ["The Force", "Lightsaber"],
  damage: 0,
};

/** @type Jedi */
const ahsoka = {
  id: 2,
  name: "Ahsoka Tano",
  level: 83,
  skills: ["The Force", "Lightsaber", "Teach"],
  damage: 0,
};

/**
 * Prints all jedi in the provided collection.
 *
 * @param {Array<Jedi>} collection
 * @returns {void}
 */
function printAll(collection) {
  collection.forEach(function print(jedi) {
    log(jedi);
  });
}

printAll([aayla, ahsoka]);

printAll([aayla, ahsoka, "Nope..."]);


printAll([{ id: 1, name: "Yoda" }, aayla, ahsoka]);

