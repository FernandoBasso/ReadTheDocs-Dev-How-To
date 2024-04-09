/**
 * Solution from the instructor.
 */
function minSubArrLen(nums, n) {
  let sum = 0;
  let l = 0;
  let r = 0;
  let minLen = Infinity;

  while (l < nums.length) {
    // if current window doesn't add up to the given sum then
    // move the window to right
    if (sum < n && r < nums.length) {
      sum += nums[r];
      r++;
    }
    // if current window adds up to at least the sum given then
    // we can shrink the window
    else if (sum >= n) {
      minLen = Math.min(minLen, r - l);
      sum -= nums[l];
      l++;
    }
    // current total less than required total but we reach the end,
    // need this or else we'll be in an infinite loop
    else {
      break;
    }
  }

  return minLen === Infinity ? 0 : minLen;
}

//
// Much cleaner and elegant than mine. It uses less confusing conditions
// and updates variables in less places.
//
// This solution sometimes cause ‘minLen’ to be incorrect until some
// future iteration “fixes” it to a correct value that will work out in
// the end. The same for ‘sum’, which sometimes gets back to less than
// the previous ‘n’ value. But it is brilliant that it simplifies logic,
// and in the end everything works out magnificently.
//
//
export { minSubArrLen };

