# Haskell List Comprehensions



## Acronym Generator :)

```
λ> [c | c <- "World Wide Web", elem c ['A'..'X']]
"WWW"
```



Let's make a function:

```hs
acron :: [Char] -> [Char]
acron s = [c | c <- s, elem c ['A'..'X']]
```



```
λ> acron "World Wide Web"
"WWW"

λ> acron "Self-Contained Underwater Breathing Apparatus"
"SCUBA"

λ> acron "National Aeronautics and Space Administration"
"NASA"
```



## Only Vowels



```hs
vowels :: String -> String
vowels s = [c | c <- s, elem c "aeiouAEIOU"]
```



```
λ> vowels "Haskell"
"ae"

λ> vowels "Lara Croft The Angel Of Darkness"
"aaoeAeOae"
```
