//
// • Author: Fernando Basso
// • https://fernandobasso.dev/contact/
// • https://fernandobasso.gitlab.io/devhowto/
// • https://www.devhowto.dev/
// 

export const log: Console["log"] = console.log.bind(console);

type F = (s: string) => void;

type G = (s: string, f: F) => void;

//
// `fn1` is annotated to the type `G`.
// 
// We use `s` and `f` in the signature and inside the body of the
// function. Total match of signature with type of `G`. We declare `s`
// and `f` and actually use them inside the body of `fn1`.
//
const fn1: G = (s, f) => {
  log(s);
  f("Running f");
};

fn1();
// Expected 2 arguments but got 0.

fn1("hello");
// Expected 2 arguments but got 1.

fn1("hello", (s) => log(s));


// fn2` is annotated to the type `G`, but because we don't use the
// parameters inside the body of the function, the type checker doesn't
// complain that we are not matching the signature with the type of `G`.
//
// This is to make TypeScript more closely match the behavior of
// JavaScript, which let's we pass any number of arguments to functions.
//
const fn2: G = () => {};

//
// `fn3` signature mentions `s`, and we make use of `s` inside of the
// function. We do not mention the second parameter, and again, the type
// checker doesn't care about because it sees we are not making use of
// it.
//
const fn3: G = (s) => {
  log(s);
};

//
// Here, we are invoking
//
fn1();

fn1("a", () => null)

/*



*/

// vim: set textwidth=72:
