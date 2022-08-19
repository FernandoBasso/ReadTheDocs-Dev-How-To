function isEven(n) {
  if (n % 2 === 1 || n % 2 === -1) {
    return false;
  }
  else {
    return true;
  }
}

module.exports = { isEven };
