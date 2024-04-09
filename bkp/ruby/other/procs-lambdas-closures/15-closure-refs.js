var l = console.log.bind(console);

function runIt(callback) {
    callback();
}

var name = 'Yoda';

function printName() {
    l(name);
}

name = 'Luke';

runIt(printName);
// → Luke, just like in ruby.

//
// It seems some languages "lock down" the name value at the point
// the proc/lambda/callback was declared. But in Ruby and JavaScript
// at least, it follows the reference (variable `name` in this case
// and retrieve its value at the point we run the proc/lambda/callback.
//
