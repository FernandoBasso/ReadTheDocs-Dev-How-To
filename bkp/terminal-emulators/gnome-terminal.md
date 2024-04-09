---
description: Tips and examples on Gnome Terminal
---

## Set Title

Since gnome-terminal 3.14, it is no longer possible to set the tittle using the UI. It has to be done from the command line using a escape sequence.

- [gnome bugzilla: the --title option is nonfunctional](https://bugzilla.gnome.org/show_bug.cgi?id=740188)
- [gnome bugzilla: Tidy up profile preferences](https://bugzilla.gnome.org/show_bug.cgi?id=724110)
- [askubuntu: How to Change Gnome Terminal Title](https://askubuntu.com/questions/22413/how-to-change-gnome-terminal-title)
- [askubuntu: How to change Terminal Title in ubuntu 16.04 [duplicate]](https://askubuntu.com/questions/774532/how-to-change-terminal-title-in-ubuntu-16-04)
- 

With bash:

```shell-session
PROMPT_COMMAND='echo -ne "\033]0;λ Always Be Awesome! u03bb\007"'
```

Works if bash is running in sh/posix mode:

```shell-session
printf "\e]2;λ Always Be Awesome! λ\a"
```
