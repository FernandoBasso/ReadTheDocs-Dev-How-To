export const MODNAME = "e13a-branded-valid-email";

////
// ValidEmail doesn't really do anything. It is currently
// just an alias to string.
//
type ValidEmail = string;

function isValidEmail(email: string): boolean {
  return email.includes("@");
}

declare function sendEmail(email: ValidEmail): void;

const emailOk: string = "me@example.com";
const emailErr: string = "invalid-email";


////
// Expected to type-check is the email is valid.
//
if (isValidEmail(emailOk))
  sendEmail(emailOk);

////
// And the type-guard is not really helping either (so far).
//
if (isValidEmail(emailErr))
  sendEmail(emailErr);

//
// Because our type guard simply returns boolean, inside the
// if conditions, both emails are simply of type string.
// It does helps at runtime, but our types are still meh...
//
