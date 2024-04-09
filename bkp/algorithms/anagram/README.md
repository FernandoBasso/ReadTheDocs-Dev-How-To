# Anagram

An anagram is a type of word play, the result of rearranging the letters of a word or phrase to produce a new word or phrase, using all the original letters exactly once; for example orchestra can be rearranged into carthorse.

## Challenge

Devise a function that gets one parameter 'w' and returns all the anagrams for 'w' from the file word list file. The file wl.txt contains a huge list of valid words and the idea is to find the anagrams of ‘w’ included in such a list.


Given a function or method `anagram` with the paramter `horse`, the output should be a list or array like:


```
['heros', 'horse', 'shore']
```

You can assume the absolute/relative path to wl.txt to be fixed and, therefore hardcore in the code, or you can also change the firm of the method to for example anagrams("horse", ./wl.txt).
