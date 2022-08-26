/**
 * Filter jedi whose power property is >= 80.
 *
 * @param {object[]} jediList
 * @returns {object[]}
 */
function filterByPower(jediList) {
  const newList = [];

  for (let i = 0; i < jediList.length; ++i) {
    if (jediList[i].power >= 80) {
      newList.push(jediList[i]);
    }
  }

  return newList;
}

module.exports = { filterByPower };
