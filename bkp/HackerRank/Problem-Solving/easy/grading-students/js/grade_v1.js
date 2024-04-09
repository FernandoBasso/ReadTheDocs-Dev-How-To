/**
 * Given some integer `num`, find its next multiple of `multiplier`.
 *
 * ASSUME: Both values are integers. If input are not integers, the
 *
 * ASSUME: The multiplier is not zero, which would cause `NaN` to
 * be returned.
 *
 * The result is always an integer, unless it returns `NaN` due to
 * incorrect inputs.
 *
 * @param {number} num
 * @param {number} multiplier
 * @returns {number}
 */
function getNextMultOf(multiplier, num) {
  return Math.ceil(num / multiplier) * multiplier;
}

/**
 * Conditionally rounds `grade` according to the rules.
 *
 * - If grade < 38, do nothing.
 * - If grade >= 38, then:
 *   - If difference between next multiple of the grade and the grade
 *     is less than 3, round up to the next multiple of 5.
 *   - Otherwise, do nothing.
 *
 * @param {number} grade
 * @returns {number}
 */
function round(grade) {
  const newGrade = getNextMultOf(5, grade);

  return (newGrade - grade < 3)
    ? newGrade
    : grade;
}

/**
 * Computes the student grades according to the grading rules.
 *
 * @param {Array<number>} grades
 * @returns {Array<number>}
 */
function gradeStudents(grades) {
  const newGrades = [];
  let i = 0, grade;

  while ((grade = grades[i++]) !== undefined)
    newGrades.push(grade < 38 ? grade : round(grade));

  return newGrades;
}

export { round, gradeStudents };

