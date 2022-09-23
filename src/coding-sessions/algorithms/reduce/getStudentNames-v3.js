/**
 * Gets a list of names of students.
 *
 * A more FP-like approach using reduce.
 *
 * @param {object[]} students
 * @returns {string[]}
 */
function getStudentNames(students) {
  return students.reduce((namesAcc, student) => {
    return [...namesAcc, student.name];
  }, []);
}

module.exports = { getStudentNames };
