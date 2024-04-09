/*

https://www.codewars.com/kata/delete-occurrences-of-an-element-if-it-occurs-more-than-n-times/train/javascript

*/


// deleteNth :: arrray-of[number] -> array-of[number]
// Produce array with at most N items of each value.
function deleteNth(arr) {

  return [];
}

const l = console.log.bind(console);


l(deleteNth([1,1,1,1],2));
// → [1, 1]

l(deleteNth([20,37,20,21],1));
// → [20,37,21]

l(deleteNth([1,1,3,3,7,2,2,2,2], 3));
// → [1, 1, 3, 3, 7, 2, 2, 2])


