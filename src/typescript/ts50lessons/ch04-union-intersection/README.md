# 04 Union and Intersection Types



**IMPORTANT**: Always hover over the types or try the intellisense to get a better understanding of the types!



- Lesson 22: Modeling Data
- Lesson 23: Moving in the Type Space
- Lesson 24: Working With Value Types
- Lesson 25: Dynamic Unions
- Lesson 26: Object Types and Type Predicates
- Lesson 27: Down at the Bottom: never
- Lesson 28: Undefined and Null

https://livebook.manning.com/book/programming-with-types/about-this-book/


What Is an RSVP?

The term "RSVP" comes from the French expression répondez s'il vous plaît, meaning "please respond." If RSVP is written on an invitation, it means the host has requested that the guest respond to say if they plan to attend the party.


type:

- classification of data
- meaning of data
- operations that can be performed on that data
- set of allowed values


`any` and `unknown` are TOP TYPES. They encompass the wholse set of all available types (and values for that matter).

`string & number` produce an empty set because there is no common values between those two types.

`undefined` and `null` are bottom values. Values that bear not actual value.

In theory is a real absence of a value, like trying to get a property that doesn’t exist in an object. `null` shouldn’t even exist… We should just have `nil` like in some other lispy languages or Ruby.

- [Playground numbers and null and undefined](https://www.typescriptlang.org/play?#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMJBhtO3PgNIVR9VOIDmAS0QAbYsPFnUwKdLQB3RA6YstXHsBf-JGVd3QNlPUFAFQlpECEtUE2wAYUUGSzwAbQAieJMsgF0IAF4IKLS8cVzxYTNyAAoy1HSASgBuCJBPAFliAGs8CFp4dAHADAIY9DNVADl4S0sk5Dw5XtpRiDNaCEVxJg71ZipUFAhRgA8ARnX9U7OAJkBMAghiEdP4cjxeWrwyB4AaCDCJAQFADRAATw4AzkSxWBE8mwgAHkANIQFwWZAbRC7TrSF4QS7YQjwAC2wgI7XSsXuxLJFPQ7T2kCowLwZw4limFks4MGeHYtRMIKWpJBlFG7ykX0IPyeGFOJPmDwRW0Ix2ecjkeA4ymE6VxYHx6EJAGY6eSCMUIFLPt8yFSBYSACwWhnWpWWJmdSAAFWQiMR+K5ZAgACkAMqlVBScTUYEWCAcdAx+DarajabEaYPQ2gY0QdAXa2XCAAaggFyZuTqRbaoCAA)

- [TS Playground: some notes on tuples](https://www.typescriptlang.org/play?#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMJBhtO3PgNIVR9VOIDmAS0QAbYsPFnUwKdLQB3RA6YstXHsBf-JGVd3QNlPUFAFQlpECEtUE2wAYUUGSzwAbQAieJMsgF0IAF4IKLS8cVzxYTNyAAoy1HSASgBuCJBPABV4DnTaUtIAcljhPAha-nR0PDJxJgiy2IArWbNiiGyATVQyYiyAGggATgAGU-z2yAgbgD0AfgXILuRxxABPDnHUXghADAJVmQzIBMAgmAwAcgB5LoQFBvXrpf4ZGLoWomI6EeAAWzG6HywPEEAAkrEzLRPH86ii0RAAD4QTE4gjNDL4o4oYik2hHUgQYjTYjvCAuNC0cZ4dJYvBKAZyUieMYQPAWV7oPn-amEEygjDqv6M3EEhZPZhUJDC8ZywgQeBi-5LUFiAiIezRQkvMkQLHEADWeAGcNhCLwnmQxGk4wj6CFtC+cjMvDMclhn39wpVEBmxAohEsQukxEs8H9806Sz5yAYPuIG2ysErqGrEC6pFQhwgAGYAOyFOilVKIK7MG4QB4miDg1CxP7IxCorUY7G4-GlsDjl7jGaIeBEFNfCA-WHIT28eCEVSusHq7d9b6-Gt-YSoJp4UjAzz6dWn88uxRHzmwr6aaEFOEAcPyxBYgMn5bju0T-NIqBmGQRqdN+F5-raeBdAkJjpAAysoiB4HUyHYAazLYBkT4vqQRx1M0xQAHwQIhyGFAA3qANzpLEMScuMJS8IWYrtNxmZ4Nuu4ZMJlhivRjFFCxHEQPxxEbAAhGp4wAL6XKAOmgEAA)

