function isEven(n) {
  if (Math.abs(n % 2) === 1) {
    return false;
  }
  else {
    return true;
  }
}

module.exports = { isEven };
