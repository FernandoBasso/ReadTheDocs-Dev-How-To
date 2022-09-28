const log = console.log.bind(console);

const jedi = {
  id: 1,
  name: 'Ahsoka Tano',
  power: 96,
};

/**
 * Prints a jedi's attributes.
 *
 * ASSUME: The input object contains the properties
 * `name` and `power`. (garbage in, garbage out).
 *
 * @param {object} obj
 */
function display(obj) {
  log(`${obj.name} has power ${obj.power}!`);
}

/**
 * Prints a jedi's attributes.
 *
 * ASSUME: The input object contains the properties
 * `name` and `power`. (garbage in, garbage out).
 *
 * @param {object} obj
 */
function print({ name, power }) {
  log(`${name} has power ${power}!`);
}

function preview(obj) {
  const { name, power } = obj;
  log(`${name} has power ${power}!`);
}

display(jedi); // obj.name, obj.power
print(jedi);   // { name, power }
preview(jedi);
