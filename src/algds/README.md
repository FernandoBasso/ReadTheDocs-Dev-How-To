# Algorithms and Data Structures

- [Algorithms and Data Structures](#algorithms-and-data-structures)
  - [Intro](#intro)
  - [Naming Files](#naming-files)
  - [Project Configuration](#project-configuration)
  - [JavaScript Unit Tests](#javascript-unit-tests)
  - [TypeScript and Deno](#typescript-and-deno)

## Intro

Project configuration and unit tests is my own work.
It is one of my goals to TDD as much as possible in all things I do.
I sometimes also use better identifier names (for functions and variables) because although I try to document (with comments) some important stuff.
I also strive for the best self-documenting code possible.
Always!

Some problems have more than one solution because I like to try multiple approaches.
In the future, I hope I come back and do implementations using ramda using a very FP-style approach.

Install [nvm](https://github.com/nvm-sh/nvm) and run these commands (just once):

```
nvm install --lts
npm install
```

## Naming Files

We use cameCase for naming the spec and solution files.

* Unit test files are named like `addUpTo.spec.js`. The extension is **.spec.js**.
* Implementation files will most likely always have a few different versions (solution approaches), and we name them like `addUpTo-v{1,2,3,...}.js`. Note the `v1`, `v2` etc. “version” thing. Examples would be `addUpTo-v1.js` and `addUpTo-v2.js`. The same unit test file is used to test different implementations of the solution.

## Project Configuration

Current Node and Jest as of 2021 have experimental support for ESM and we are using ESM in this repo. It requires `"type": "module"` in `package.json` and an empty transform object in jest configs. Check `jest.config.js` for links/docs.

## JavaScript Unit Tests

For all files:

```
npm run test
```

For a specific file under development or scrutiny:

```
npm run test path/to/file.spec.js
```

Can also use watch mode, and silence `console.log` to keep the output clean:

```
npm run test -- --watch --silent path/to/file.spec.js
```

Or, another nice one `--verbose` which shows a list of suite and passing tests (which would be suppressed otherwise):

```
npm run test -- --watch --silent
```

**NOTE**: Replace `--watch` with `--watchAll` if not inside a Git repo.

## TypeScript and Deno

```shell-session
$ deno run --import-map ./import-map.json **/*.ts
```

If we use an unstable Deno feature, we can also use `--unstable`. Also run `:CocLocalConfig` from vim/nvim if using unstable APIs.

Sometimes `--quiet` is nice too. Run `deno --help | cat` for more information.
