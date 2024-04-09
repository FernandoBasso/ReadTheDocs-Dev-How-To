# rsync experiments
Created Friday 06 April 2018

	$ tree -C ./dir1/
	./dir1/
	├── main.c
	├── sub1
	│   ├── libstr.o
	│   └── sub
	│       └── notes1.txt
	└── sub2
	    ├── lib_parser.c
	    └── sub
	        ├── ipsum.adoc
	        └── lorem.md

**WRONG**: Copies everything, not just the directories `sub`:
	$ rsync -rav --include='+ sub' ./dir1 dst/

Anything that "does not match is not skipped", says the manual.

**WRONG**: Attempt to ignore everything that is not `sub`. Copies nothing because now rsync won't even look inside `dir1`. It works recursively. It dosn't see `sub`:
	$ rsync -rav --include='+ sub' --exclude='- *' ./dir1 dst

**WRONG** STILL: So, first need to tell rsync that it is okay to look inside every directory with ```--include='+ */```, then try to find `sub` inside any of those directories with ```--include='+ sub/``'`, and ignore all the rest wiht ```--exclude='- *``'`:
	$ rsync -rav --include='+ */' --include='+ sub/' --exclude='- *' ./dir1 dst

But the above still doesn't copy files inside the directores ```sub/```. It ends up creating only directories up to any `sub' directory.

**CORRECT**: This works. Exactly same command line as the previous one, with just a new added ```*``` in ```--include='+ sub/*``'`:
	$ rsync -av --include='+ */' --include='+ sub/*' --exclude='- *' dir1/ dest


--include-from
--------------

Now supose we want `sub` only if they are descendant of `sub2`.

First, let's see the source structure:
	$ tree -C dir1/
	dir1/
	├── main.c
	├── sub
	│   └── foo.txt
	├── sub1
	│   ├── libstr.o
	│   └── sub
	│       └── notes1.txt
	└── sub2
	    ├── lib_parser.c
	    └── sub
	        ├── ipsum.adoc
	        └── lorem.md

Then `patterns.txt` contains:
	+ */
	+ sub2/sub/*
	- *

And the command line:
	$ rsync -avv --include-from=patterns.txt dir1/ dest

And those end up creating empty directories on the destination because ```+ */``` matches all directories. So, it transfers files inside ```sub2/sub/```, all other directories (again, because of ```+ */``` rule), and then ignores all the rest.
	$ tree -C dest/
	dest/
	├── sub
	├── sub1
	│   └── sub
	└── sub2
	    └── sub
	        ├── ipsum.adoc
	        └── lorem.md

We need to prune empty dirs from the transfer:
	$ rsync -avv --prune-empty-dirs --include-from=patterns.txt dir1/ dest

Now we are talking business:
	$ tree -C dest/
	dest/
	└── sub2
	    └── sub
	        ├── ipsum.adoc
	        └── lorem.md


select only certain subdirs
---------------------------

Given this file structure:
	$ tree -C proj1/
	proj1/
	├── libs
	│   └── lib1.h
	├── main.c
	├── sub
	│   └── foo.txt
	├── sub1
	│   ├── libstr.o
	│   └── sub
	│       └── notes1.txt
	└── sub2
	    ├── lib_parser.c
	    ├── libs
	    │   └── lib2.c
	    └── sub
	        ├── ipsum.adoc
	        └── lorem.md

We want to backup only files inside any ```libs``` directories, and nothing else. We try this:

* ```+ */``` → allow rsync to look inside every directory and subdirectory.
* ```+ libs/*``` → and look for "libs/<anythign else inside>"
* ```- *``` → and exclude all files (except those already matched above


Remember, anything that does not match is not skipped. So, ```+ */``` means that all dirs match. So, transfer all dirs. Then transfers all "libs/<files here>", and ignore all files ```*```, but not the directories already matched.

	$ rsync -av --include='+ */' --include='+ libs/*' --exclude='- *' proj1/ dest
	sending incremental file list
	created directory dest
	./
	libs/
	libs/lib1.h
	sub/
	sub1/
	sub1/sub/
	sub2/
	sub2/libs/
	sub2/libs/lib2.c
	sub2/sub/
	
	sent 421 bytes  received 116 bytes  1,074.00 bytes/sec
	total size is 10  speedup is 0.02
	
	$ tree -C dest/
	dest/
	├── libs
	│   └── lib1.h
	├── sub
	├── sub1
	│   └── sub
	└── sub2
	    ├── libs
	    │   └── lib2.c
	    └── sub
	
	7 directories, 2 files

As we can see, rsync copied all directories, not only ```libs``` directories, and we ended up with unwanted and empty directories.

We can't allow rsync to look inside all directories without also transfering them. But we can make a combination of rules and options to copy only "/libs/<files here>" and nothing else.
	$ rsync -av --prune-empty-dirs --include='+ */' --include='+ libs/*' --exclude='- *' proj1/
	 dest
	building file list ... done
	created directory dest
	./
	libs/
	libs/lib1.h
	sub2/
	sub2/libs/
	sub2/libs/lib2.c
	
	sent 369 bytes  received 93 bytes  924.00 bytes/sec
	total size is 10  speedup is 0.02
	
	$ tree -C dest/
	dest/
	├── libs
	│   └── lib1.h
	└── sub2
	    └── libs
	        └── lib2.c
	
	3 directories, 2 files


