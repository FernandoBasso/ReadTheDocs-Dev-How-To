# Algorithms and Data Structures JavaScript Masterclass

- [Algorithms and Data Structures JavaScript Masterclass](#algorithms-and-data-structures-javascript-masterclass)
  - [Intro](#intro)
  - [Naming Files](#naming-files)
  - [Project Configuration](#project-configuration)
  - [JavaScript Unit Tests](#javascript-unit-tests)
  - [TypeScript and Deno](#typescript-and-deno)

## Intro

This repository contains my solutions, code and notes from [Colt Steele course on Udemy](https://www.udemy.com/course/js-algorithms-and-data-structures-masterclass).

Project configuration and unit tests is my own work (the code on the video lessons is written directly on Chrome, no tests). It is one of my goals to TDD as much as possible in all things I do. I sometimes also use better identifier names (for functions and variables) because although I try to document (with comments) some important stuff, I also strive for the best self-documenting code possible, always!

The [Github repo](https://github.com/FernandoBasso/JavaScript-Algorithms-and-Data-Structures-Masterclass) is just a push mirror of the main [repo on Gitlab](https://gitlab.com/programming-studies/javascript-algorithms-data-structures-masterclass).

I am **intentionally not using any linting** in this project because I want to have some freedom with formatting. Some presets try to prevent you from using bitwise operators, for example. There are legitimate cases where they are OK to use. I don't want to spend my time studying algorithms fighting the linter, disabling or tweaking rules, etc. And just to be clear, for a React, Vue.js, a Node + Express API, OK, I do use linters, but not in this algorithms and data structures study project.

Some problems have more than one solution because I like to try multiple approaches. In the future, I hope I come back and do implementations using ramda using a very FP-style approach.

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

If we use an usntable Deno feature, we can also use `--unstable`. Also run `:CocLocalConfig` from vim/nvim if using unstable APIs.

Sometimes `--quiet` is nice too. Run `deno --help | cat` for more information.
