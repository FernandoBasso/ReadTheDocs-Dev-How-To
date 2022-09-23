/**
 * Gets a list of names of students.
 *
 * A more FP-like approach using reduce.
 *
 * @param {object[]} students
 * @returns {string[]}
 */
function getStudentNames(students) {
  return students.reduce((namesAcc, { name }) => {
    return [...namesAcc, name];
  }, []);
}

module.exports = { getStudentNames };

/*

const UserProfile = (props) {
  return <div>{props.name}</div>;
}

const UserProfile = ({ name }) {
  return <div>{name}</div>;
}

*/
