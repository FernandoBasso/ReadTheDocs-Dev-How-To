/**
 * Converts a 12-hour time format to a 24-hour (military) format.
 *
 * @sig String -> String
 */
function timeConv(time) {
  if (time.includes('AM')) {
    return time
      .replace('12', '00')
      .replace('AM', '');
  }

  if (time.includes('PM')) {
    if (/^12/.test(time))
      return time.replace('PM', '');

    return time.replace(/([01][0-9])/, function replacer(m, g1) {
      return Number(g1) + 12;
    }).replace('PM', '');
  }
}

export { timeConv };
