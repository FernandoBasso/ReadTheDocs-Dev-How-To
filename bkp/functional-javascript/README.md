# Functional Programming

- [Intro](#intro)
- [General Concepts and Ramda](#general-concepts-and-ramda)
- [Resources Used](#resources-used)

## Intro
A place to explain concepts, show examples, take notes and store snippets of functional programming code in general.

## General Concepts and Ramda

A lot of examples in this repo make use of [Ramda](https://ramdajs.com). It is very important to bear in mind that I make everything possible to follow a specific convetion when it comes to the order of arguments: _data should always come last_.

That is, functions will always receive the data they operate on as their last argument. That way, we can curry and partial-apply functions as much as we want, first always passing config/setup arguments, with data to be operated on comming last. It makes it a breeze to use pipe and compose in general.

## Resources Used
A lot of stuff here come from books, tutorials, videos, etc. What follows is a list resources I have used so far to study about this topic.

- [ramda docs](https://ramdajs.com/docs/)
- [Thinking in Ramda, by Randy Coulman](http://randycoulman.com/blog/categories/thinking-in-ramda/)
- [Ramda JS Tutorial - Youtube Playlist by Christopher Okhravi](https://www.youtube.com/playlist?list=PLrhzvIcii6GMeyUfpn-o5xVCH3_UykrzI)
- [Streaming Logs with Transducers and Ramda](http://simplectic.com/blog/2015/ramda-transducers-logs/)
- [Why Ramda](https://fr.umio.us/why-ramda/)

