# tr

**NOTE**: BusyBox doesn't have the long options for the `tr` program.



## Translate [] to ()

```
$ echo 'for [[j=0; j < 5; ++j]]' | tr '[]' '()'
for ((j=0; j < 5; ++j))
```



## Delete lowercase a to z

Use `--delete` or `-d`:

```
$ echo 'May The Force' | tr --delete 'a-z'
M T F
```



