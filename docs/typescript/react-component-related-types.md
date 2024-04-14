---
title: React Component Related Types | TypeScript React
description: Notes, ideas, concepts, tips and examples on React component Related types with TypeScript.
---

# React Component-Related Types

```{note}
Remember that the way the type system works in TypeScript depends on `tsconfig.json` configuration.
```

```{tip}
The following examples were written and tested with `typescript@5.4.5`, `@types/react@18.2.75`, and this config:
```

```json
  "compilerOptions": {
    "target": "es6",
    "module": "commonjs",
    "strict": true,
    "checkJs": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "jsx": "react-jsx",
    "forceConsistentCasingInFileNames": true,
    "rootDir": "./src",
    "outDir": "./dist"
  }
}
```

## React.ReactElement

Here's the type for `React.ReactElement`:

```typescript
declare namespace React {
  /**
   * Represents a JSX element.
   *
   * Where {@link ReactNode} represents everything that can be
   * rendered, `ReactElement` only represents JSX.
   *
   * @template P The type of the props object
   * @template T The type of the component or tag
   *
   * @example
   *
   * ```tsx
   * const element: ReactElement = <div />;
   * ```
   */
  interface ReactElement<
    P = any,
    T extends
        string 
      | JSXElementConstructor<any> = string
      | JSXElementConstructor<any>,
  > {
    type: T;
    props: P;
    key: string | null;
  }
}
```

Basically, an interface with two generic type parameter for props and type.
It matches an object with the properties `type`, `props`, and `key`.
E.g.:

```typescript
// OK
const elem: ReactElement = {
  type: "p",
  props: { className: "p-default", text: "Hello!" },
  key: "p1",
};
```

A `ReactElement` cannot be `undefined`, `null`, an array, or anything which is not an object which does not satisfy the expected structure (`type`, `props` and `key`).
It does, however, accept an element constructed with JSX syntax:

```typescript
// OK
const elem: ReactElement = <div />;
```

Even though `<div />` is technically of type `JSX.Element` (it is even inferred as `JSX.Element` if no explicit type annotation is provided), it is compatible with `ReactElement`.

## JSX.Element

`JSX.Element` is based off of `ReactElement` but with the `<any, any>` generic, which is to allow libraries to implement JSX in ways that suit them.

The type for `JSX.Element`
```typescript
declare global {
  namespace JSX {
    interface Element extends React.ReactElement<any, any> {}
  }
}
```


## React.ReactNode

The type for `ReactNode`:

```typescript
type ReactNode =
    | ReactElement
    | string
    | number
    | Iterable<ReactNode>
    | ReactPortal
    | boolean
    | null
    | undefined;
```

Note it is very broad.
It includes `ReactElement`, numbers, strings, `null`, `undefined`, and `Iterable<ReactNode>`:

```typescript
import { type ReactNode } from "react";

var node: ReactNode;
node = undefined;
node = null;
node = [];
node = [null, undefined];
node = <div />;
node = 1;
node = "hello";
node = [
  <div />,
  {
    type: "p",
    props: { className: "p-default", text: "Hello!" },
    key: "p1",
  },
];
node = createElement("h1", null, "Heading");
```

Values **not** compatible with `ReactNode` are objects that do not satisfy the type of `ReactElement` or arrays of such incompatible objects:

```typescript
// NOK
import { type ReactNode } from "react";

let node: ReactNode;
node = {};
node = { id: 1 };

// NOK
let nodes: Array<ReactNode>;
nodes = [{}, { name: "Ahsoka Tano "}];
```

## Links and Resources

- https://stackoverflow.com/questions/58123398/when-to-use-jsx-element-vs-reactnode-vs-reactelement
- https://legacy.reactjs.org/blog/2014/10/14/introducing-react-elements.html

