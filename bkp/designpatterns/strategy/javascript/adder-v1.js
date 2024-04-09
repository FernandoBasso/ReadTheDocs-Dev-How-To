var l = function log () {
    console.log.apply(console, arguments);
};


/**
 * Adds numbers.
 *
 * @param array arr - an array of numbers.
 * @return Number - the total sum of the numbers.
 */
var addNums = function addNums (arr, selected) {

    var total, i, len;

    total = 0;
    len = arr.length;

    if (selected === 'all') {
        for (i = 0; i < len; ++i) {
            total += arr[i];
        }
    }
    else if (selected === 'even') {
        for (i = 0; i < len; ++i) {
            if (arr[i] % 2 === 0) {
                total += arr[i];
            }
        }
    }
    else if (selected === 'odd') {
        for (i = 0; i < len; ++i) {
            if (arr[i] % 2 !== 0) {
                total += arr[i];
            }
        }
    }

    return total;
}

var numbers = [3, 9, -2, -5, 3.3, 4.4];

l(addNums(numbers, 'all'));     // →  17.70...
l(addNums(numbers, 'even'));    // →  -2
l(addNums(numbers, 'odd'));     // →  14.70...
l(addNums(numbers, 'none'));    // →  0

//
// Now, what if we want to sum only numbers greater than 5? Only even numbers
// less than 15? Only numbers with decimal points? Only negative numbers?
//





