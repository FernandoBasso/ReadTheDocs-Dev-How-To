export const MODNAME = "e13b-branded-valid-email";

const log: Console["log"] = console.log.bind(console);

////
// ValidEmail is now a branded type.
//
type ValidEmail = string & { __brand: "ValidEmail" };

////
// Changed the return type from boolean to email is ValidEmail.
//
function isValidEmail(email: string): email is ValidEmail {
  return email.includes("@");
}

function sendEmail(email: ValidEmail): void {
  log("Sending email to", email);
}

const emailOk: string = "me@example.com";
const emailErr: string = "invalid-email";

if (isValidEmail(emailOk))
  sendEmail(emailOk);
else
  log("Invalid email:", emailOk);

if (isValidEmail(emailErr))
  sendEmail(emailErr);
else
  log("Invalid email:", emailErr);

//
// Now the type guard check really works. The emails inside the
// if conditions will be really of type ValidEmail. In an else
// block they are back to simply string, as expected.
// 
