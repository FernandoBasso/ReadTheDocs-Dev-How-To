var l = console.log.bind(console);
function multiply(x, y) {
    return x * y;
}
l(multiply(3, 6));
// → 18
l(multiply(4, 11));
// → 44
