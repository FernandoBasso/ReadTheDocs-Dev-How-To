const log: Console["log"] = console.log.bind(console);

//
// • Excess Property Checks
// • Missing Property Checks
// • Direct Assignment
// • Non-direct (indirect) assignment
//

type Xenomorph = {
  height: number;
  speed: number;
};

//
// Here we are directly assigning excess properties, which
// will trigger excess property checks.
//
const alien: Xenomorph = {
  height: 2.1,
  speed: 11,
  power: 143,
};

//
// And here we are missing properties, which will also
// trigger property checks (missing property checks in
// in this case).
//
const queen: Xenomorph = {
  height: 4.5,
};

//
// This function takes a value of the type ‘Xenomorph’.
//
function printXeno({ height, speed }: Xenomorph): void {
  log(`Xeno info:\nheight: ${height}\nspeed: ${speed}`);
}

//
// Here, we are passing a value which was previously
// created. This is not a direct assignment, and does NOT
// trigger excess property checks.
//
printXeno(alien);

//
// Also passing a value that was previously created. ‘queen’
// has less properties than expected, but property checks
// do NOT kick in here either.
//
printXeno(queen);


//
// Access property checks are triggered when doing direct
// assignment, but not when reusing a previously created
// (assigned) value.
//
// A similar thing happens with missing property checks.
//
// If a function takes a value that has more properties than
// expected, they may be just ignored. But when a value is
// missing properties, that will likely cause problems at
// runtime. BEWARE of saying a given value is of some type
// when it is not really of that type.
//
