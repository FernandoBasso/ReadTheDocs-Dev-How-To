---
description: Examples and tips on hexdump, xxd and od command line utilities
---



## Hexdump

https://www.suse.com/c/making-sense-hexdump/



```shell-session
$ echo abAB | hexdump -C
00000000  61 62 41 42 0a                                    |abAB.|
```

