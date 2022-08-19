function isEven(n) {
  var comp = n % 2 === 0;
  
  if (comp) {
    return true;
  }
  else {
    return false;
  }
}

module.exports = { isEven };