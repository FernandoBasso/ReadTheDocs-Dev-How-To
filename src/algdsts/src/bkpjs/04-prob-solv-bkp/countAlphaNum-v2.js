/**
 * Solution by Colt Steele.
 */
function countAlphaNum(str) {
  if (str.length === 0) return {};

  let freq = {};

  for (let chr of str) {
    chr = chr.toLowerCase();

    if (/[a-z0-9]/.test(chr)) {
      freq[chr] = ++freq[chr] || 1;
    }
  }

  return freq;
}

export { countAlphaNum };
