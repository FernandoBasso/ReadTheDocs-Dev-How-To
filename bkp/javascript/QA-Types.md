# JavaScript Types Exercises



## Question 1

```
function* f() {
  yield 1;
}

var g = f();
l(g);
```

What is the output?

### Answer

It is the so called “Generator Object” (not the value 1).

When we invoke a generator function, it creates a generator object and pauses execution before the first actual line of code inside the body of the function.

## Question 2

```
function* f() {
  yield 1;
}

var g = f();
l(g); // <1>
```

Why doesn't <1> print 1? How to make it print the value 1?

### Answer

When we invoke a generator function, it creates a generator object and pauses execution before the first actual line of code inside the body of the function.

To print the yielded value itself, we must invoke `next()`:

```
l(g.next());
```



