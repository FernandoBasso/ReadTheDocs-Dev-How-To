# Constant Pointers vs Pointers

[TOC]

Two basic ways to create an array of chars or string:

``` c
char hello[10] = "Hello";

char *world = "World";
```

1.  `hello` is a constant pointer to char.

2.  `world` is a normal pointer.

## Constant Pointer

Pointer arithmetic is not allowed with constant pointers:

``` c
char hello[] = "Hello";
++hello;
// ⇒ main.c:9:8: error: cannot increment value of type 'char [10]'
// ⇒   hello++;
// ⇒   ~~~~~^
```

But constant pointers allow changing values in memory locations:

``` c
char hello[10] = "Hello";
hello[0] = 'X';
*(hello + 4) = 'Z';
printf("%s\n", hello);
// ⇒ XellZ
```

## Pointer

With (normal) pointers, pointer arithmetic is possible:

``` c
char *world = "World";
++world;
printf("%s\n", world);
// ⇒ orld
```

But can’t change the value at a given memory location. This produces a segmentation fault:

``` c
char *world = "World";
world[1] = 'X';
*(world + 1) = 'X';
// ⇒ Segmentation fault (core dumped).
```

We get segmentation fault either using subscript notation or pointer notation.

