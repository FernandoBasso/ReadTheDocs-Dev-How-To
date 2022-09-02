const MIN_AVERAGE_TO_BE_APPROVED = 6.5;

/**
 * A predicate to check whether the student is approved or not.
 *
 * @param {object} student
 * @returns {boolean}
 */
function isApproved(student) {
  return student.average >= MIN_AVERAGE_TO_BE_APPROVED;
}

/**
 * A predicate to check whether the student failed.
 *
 * ASSUME: The `student` object contains an `average` property.
 *
 * @param {object} student
 * @returns {boolean}
 */
function isFailed(student) {
  return !isApproved(student);
}

/**
 * Returns an array students who satisfy the predicate.
 *
 * @param {object[]} students
 * @returns {object[]}
 */
function filterStudents(students, filterCallback) {
  return students.filter(filterCallback);
}

module.exports = {
  filterStudents,
  isApproved,
  isFailed,
};
