# grep - Global Regular Expression and Print

As a developer, one of the main things one wants to do is to search for stuff in a directory, for given file types or extensions. Most docs and tutorials never focus on that and forum questions tend to be a better source of information for things like this. Why do we have documentation and constantly have to search the web?


## Intro

- https://www.gnu.org/software/grep/


## Search in files with given extensions

```
$ grep --color -nrH --null \
    --include '*.md' --include '*.hs' \
    -e 'case.*of' ../../
```
