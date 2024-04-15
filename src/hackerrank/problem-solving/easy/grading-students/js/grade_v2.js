/**
 * Given some integer `num`, find its next multiple of `multiplier`.
 *
 * ASSUME: Both values are integers.
 *
 *
 * @param {number} num
 * @param {number} multiplier
 * @returns {number}
 */
function getNextMultOf(multiplier, num) {
  return Math.ceil(num / multiplier) * multiplier;
}

function bind(fn, ...rest) {
  return fn.bind(null, ...rest);
}

const nextMultOf5 = bind(getNextMultOf, 5);

function round(grade) {
  const newGrade = nextMultOf5(grade);

  return (newGrade - grade < 3)
    ? newGrade
    : grade;
}

function gradeStudents(grades) {
  const result = [];

  for (let i = 0; i < grades.length; ++i) {
    const grade = grades[i];

    if (grade < 38) {
      result.push(grade);
    } else {
      result.push(round(grade));
    }
  }

  return result;
}

export { round, gradeStudents };


