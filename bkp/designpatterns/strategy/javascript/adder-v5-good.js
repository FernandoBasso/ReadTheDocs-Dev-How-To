var l = function log () {
    console.log.apply(console, arguments);
};

var isEven = function isEven (num) {
    return (num % 2) === 0;
};

var isOdd = function isOdd (num) {
    return (num % 2) !== 0;
};

/**
 * Adds numbers.
 *
 * @param array arr - an array of numbers.
 * @return Number - the total sum of the numbers.
 */
var addNums = function addNums (arr, filter) {

    var total, i, len;

    total = 0;
    len = arr.length;

    for (i = 0; i < len; ++i) {
        //
        // If filter was not provided, it means it should just
        // add up all the numbers.
        //
        if (!filter || filter(arr[i])) {
            total += arr[i];
        }
    }

    return total;
};


var numbers = [3, 9, -2, -5, 3.3, 4.4];

l(addNums(numbers));       // →  17.70...
l(addNums(numbers, isEven));                                // →  -2
l(addNums(numbers, isOdd));                                 // →  14.70...
l(addNums(numbers, function (num) { return false; }));      // →  0

