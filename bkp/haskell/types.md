# Types





[TOC]

## 01 'z' "Z"

Q: What is the difference between writing `'z'` and `"z"`?

A: Single quotes are used for the type `Char`, and double quotes for the type `[Char]`, that is, list of char.

## 02 [Char]

Q: What is the type alias for `[Char]`?

A: `String`.

## 03 type

Q: What does `:type a` print?

A: An error unless there is a variable `a` is in scope.

## 04 print and putStrLn on multibyte

Q: What do the last two lines print? Explain.

```ghci
λ> c = '✔'
print c
putStrLn [c]
```

A: `c` is defined to be `✔`, which is a multibyte character. `print` displays its decimal representation, `\100004`, while `putStrLn` displays its “human” representation, that is the visual “✔” itself.

## 05 ::

Q: What is `::` used for? How to read it aloud?

Q: `::` is used in type definitions. It is read aloud as “has the type”, like in `n :: Integer` is read “n has the type Integer”.

## 06 putStrLn

Q: What is the problem with code below? How to fix?

```ghci
λ> c = 'z'
λ> putStrLn c
```

A: Because `c` is `Char` and `putStrLn` expects `String` (which is `[Char]`), we'll see an error rather "z" printed to STDOUT. A fix could be to enclose the `c` inside `[]` to create a list of one char, which is a string: `putStrLn [c]`.



