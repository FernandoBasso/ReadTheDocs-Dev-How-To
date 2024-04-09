---
description: Tmux Examples and Useful Commands
---





Working Directory
-----------------


Start a new session specifying the working directory with `-c`:

```shell-session
tmux new-session foobar -c ~/Projects/foobarproj
```

Or attach to the session specifying the working directory, which sets the working directory for new windows and panes:

```shell-session
tmux attach-session -t foobar -c ~/Projects/foobarproj/subdir/subsubdir
```

Or, from inside a tmux session.

```tmux-session
C-a :
attach-session -t . -c my/sub/dir
```

The dot “`.`” means “the current session”.

See [this](https://unix.stackexchange.com/questions/268386/how-to-change-default-new-window-directory-from-within-the-tmux) and [this](https://unix.stackexchange.com/questions/268386/how-to-change-default-new-window-directory-from-within-the-tmux).


Auto-Completion
---------------

There is bash (or other shell) completion scripts. For internal tmux commands, that is, tmux own command prompt, (`C-a :`), tmux Tab-completes unambiguous commands. That is, `C-b :s Tab` won't offer completions, because there are more than one command that starts with “s”. Add “o”, making it “so” and trying Tab again works, because there is only one command that starts with “so” (as of this writing Fri 13 Dec 2019 10:48:44 AM -03). Hitting Tab repeatedly doesn't cycle through possible completions, like many other programs do.