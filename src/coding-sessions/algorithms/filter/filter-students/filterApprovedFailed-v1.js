//
// Avoid magic constants!!!
//

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
 * @param {object} student
 * @returns {boolean}
 */
function isFailed(student) {
  // return student.average < 6.5;
  return !isApproved(student);
}

/**
 * Returns an array with approved students.
 *
 * NOTE: Approved students are those whose average is >= 6.5.
 *
 * @param {object[]} students
 * @returns {object[]}
 */
function filterApproved(students) {
  return students.filter(isApproved);
}

/**
 * Returns an array with students who failed.
 *
 * @param {object[]} students
 * @returns {object[]}
 */
function filterFailed(students) {
  return students.filter(isFailed);
}

module.exports = { filterApproved, filterFailed };
