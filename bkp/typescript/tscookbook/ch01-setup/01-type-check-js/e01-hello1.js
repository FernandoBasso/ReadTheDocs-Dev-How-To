//
// Remove the escape in front of @ to see what happens.
//
// \@ts-check

import { log } from "../lib/loggers";

// <1>
let x = 1e3;

if (Math.random() < 0.5)
  x = "hello"; // <2>

log(x);

//              <3>
//                     <4>
function addVAT(price, vat = 0.2) {
  return price * (1 + vat);
}

//           <5>
addVAT(1000, "0.2");

//           <6>
addVAT(1000).toUpperCase();
