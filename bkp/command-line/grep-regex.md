# Grep Regex

## -w, --word-regexp

The [man page](https://www.gnu.org/software/grep/manual/grep.html#Matching-Control) says:

> Select only those lines containing matches that form whole words. The test is that the matching substring must either be at the beginning of the line, or preceded by a non-word constituent character. Similarly, it must be either at the end of the line or followed by a non-word constituent character. **Word constituent characters are letters, digits, and the underscore**. This option has no effect if -x is also specified.

(emphasis mine) Then it says:

> Because the -w option can match a substring that does not begin and end with word constituents, it differs from surrounding a regular expression with ‘\<’ and ‘\>’. For example, although ‘grep -w @’ matches a line containing only ‘@’, ‘grep '\<@\>'’ cannot match any line because ‘@’ is not a word constituent.

So, word constituents **do not** include “@”. Why then `-w @` **does match** a word?
