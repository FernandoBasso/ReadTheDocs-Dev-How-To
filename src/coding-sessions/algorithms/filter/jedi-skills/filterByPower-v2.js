function canJoinJediOrder(jedi) {
  return jedi.power >= 80;
}

/**
 * Filter jedi whose power property is >= 80.
 *
 * @param {object[]} jediList
 * @returns {object[]}
 */
function filterByPower(jediList) {
  const jediOrderList = jediList.filter(canJoinJediOrder);
  console.log('jediList', jediList);

  return jediOrderList;
}

module.exports = { filterByPower };
