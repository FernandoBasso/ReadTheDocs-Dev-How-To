/**
 * Solution by Colt Steele. I just added the types.
 */
function countAlphaNum(str: string): Record<string, number> {
  if (str.length === 0) return {};

  const freq: Record<string, number> = {};

  for (let chr of str) {
    chr = chr.toLowerCase();

    if (/[a-z0-9]/.test(chr)) {
      freq[chr] = ++freq[chr] || 1;
    }
  }

  return freq;
}

export { countAlphaNum };
