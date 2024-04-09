var l = function log () {
    console.log.apply(console, arguments);
};


/**
 * Adds numbers.
 *
 * @param array arr - an array of numbers.
 * @return Number - the total sum of the numbers.
 */
var addAllNums = function addAllNums (arr) {

    var total, i, len;

    total = 0;
    len = arr.length;

    for (i = 0; i < len; ++i) {
        total += arr[i];
    }

    return total;
};

/**
 * Adds even numbers.
 *
 * @param array arr - an array of numbers.
 * @return Number - the total sum of the numbers.
 */
var addEvenNums = function addEvenNums (arr) {

    var total, i, len;

    total = 0;
    len = arr.length;

    for (i = 0; i < len; ++i) {
        if (arr[i] % 2 === 0) {
            total += arr[i];
        }
    }

    return total;
};

/**
 * Adds odd numbers.
 *
 * @param array arr - an array of numbers.
 * @return Number - the total sum of the numbers.
 */
var addOddNums = function addOddNums (arr) {

    var total, i, len;

    total = 0;
    len = arr.length;

    for (i = 0; i < len; ++i) {
        if (arr[i] % 2 !== 0) {
            total += arr[i];
        }
    }

    return total;
};

/**
 * Adds no numbers.
 *
 * @param array arr - an array of numbers.
 * @return Number - the total sum of the numbers.
 */
var addNoNums = function addNoNums (arr) {
    return 0;
};

var numbers = [3, 9, -2, -5, 3.3, 4.4];

l(addAllNums(numbers));     // →  17.70...
l(addEvenNums(numbers));    // →  -2
l(addOddNums(numbers));     // →  14.70...
l(addNoNums(numbers));      // →  0

//
// Now, what if we want to sum only numbers greater than 5? Only even numbers
// less than 15? Only numbers with decimal points? Only negative numbers?
