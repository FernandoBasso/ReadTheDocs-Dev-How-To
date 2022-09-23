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
    namesAcc.push(student.name);
    return namesAcc;
  }, []);
}

module.exports = { getStudentNames };


/*
reduce(f)

f(acc, elem);
*/
