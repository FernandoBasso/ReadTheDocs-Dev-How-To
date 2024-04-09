/**
 * Injects data into a template string.
 *
 * The keys on the data must match the placeholders on the template.
 * For example, the template...
 *
 *   'You are {name} and is {age} years old.'
 *
 * ...must be accompanied by the data object:
 *
 *   { name: 'Yoda', age: 900 }
 *
 * @param {string} templ The string to inject data into.
 * @param {object} data The data to insert into the placeholders.
 * @return {string}
 */
export function inject(templ, data) {
  // If template does not contain “{” or data is not present
  // we simply return the unmodified input.
  if (!templ.includes('{') || data === undefined) {
    return templ;
  }

  // Turn keys into into a regex like ‘/{foo}|{bar}|{baz}/’.
  const keys = Object.keys(data);
  const regex = new RegExp('{' + keys.join('}|{') + '}');

  return templ.replace(regex, function fnMatch (match) {
    // Remove ‘{’ and ‘}’ from the match so we can access the
    // ‘data’ keys correctly.
    return data[match.replace(/}|{/g, '')];
  });
};


