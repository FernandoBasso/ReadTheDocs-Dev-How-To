---
title: Rsync Notes, Examples and Tips
---



# rsync

Created Tuesday 03 April 2018

:)

extremely important
-------------------
If you want to match/include/transfer only some specific stuff, you have to:

* include those things you want to transfer.
* include directories leading to them, one by one, or with ```+ */```.
* exclude all the rest with ```- *```.
* and likely you'll also want ```--prune-empty-dirs``` so you don't end up with empty directories on the destination end.


Something like
```sell-session
$ rsync -av \
    --fileter="+ */" \
    --filter="+ *.c" \
    --filter="+ *.h" \
    --filter="- *' \
    --prune-empty-dirs \
    ./source/ /backups/myproj/
```

If you include a directory, it doesn't automatically include its contents. In recent versions, ```--include='directory/***``'` will do that.

For each file, the first matching rule applies (__and anything never matched is included__).
<https://unix.stackexchange.com/questions/2161/rsync-filter-copying-one-pattern-only>

man page
--------

### Include/Exclude Pattern Rules
	man rsync
	/^INCL RET


### important stuff from man page

The INCLUDE/EXCLUDE PATTERN RULES is arguably the most important section of the man page to read if you need to understand the intricacies of selecting particular files and directories to transfer. All examples found on the web are very simple stuff that at most explain command line options like -a, -r, -p, -P, etc. but nothing that would help understanding how to include and exclude specific stuff.

	-a, --archive
	      This is equivalent to -rlptgoD. It is a quick way of saying  you
	      want  recursion  and want to preserve almost everything (with -H
	      being a notable omission).  The  only  exception  to  the  above
	      equivalence  is when --files-from is specified, in which case -r
	      is not implied.
	
	      Note that -a does not preserve hardlinks, because finding multi‐
	      ply-linked files is expensive.  You must separately specify -H.


* `-r` → recursive
* `-l` → ?
* `-p` → preserve permissions
* `-t` → preserve time modifications


``--archive`` archive mode; equals ``-rlptgoD`` (no ``-H``, ``-A``, ``-X``)

* -r --recursive
* -p --perms
* -t --times
* -g --group
* -o --owner
* -D same as --devices --special




#### trailing “/” in pattern rule
<http://man7.org/linux/man-pages/man1/rsync.1.html#INCLUDE/EXCLUDE_PATTERN_RULES>

   o      if  the  pattern  ends with a / then it will only match a directory, not a regular file, symlink, or device.

That is why we do ```+ */``` sometimes, to allow rsync to look inside any directories and subdirectories.

#### ** and dir/*** in pattern rule
   o      if the pattern contains a / (not counting a trailing /) or a
  "**", then it is matched against the full pathname, including
  any leading directories. If the pattern doesn’t contain a / or
  a "**", then it is matched only against the final component of
  the filename.  (Remember that the algorithm is applied
  recursively so "full filename" can actually be any portion of
  a path from the starting directory on down.)

   o      a trailing "dir_name/***" will match both the directory (as if
  "dir_name/" had been specified) and everything in the
  directory (as if "dir_name/**" had been specified).  This
  behavior was added in version 2.6.7.

trailing star/double star in --exclude:
<https://lists.samba.org/archive/rsync/2006-February/014617.html>


#### the star or glob *

In many places, the ```*``` character is expanded by the shell, not by rsync. So, the line
	rsync -av *.pdf dest/
expands to a list of .pdf files in the current directores and pass a list of filenames to rsync. That is, rsync doesn't see "*.pdf", it sees something like "file1.pdf", "file2.pdf", "file3.pdf", etc.


#### trailing slash source caveat
<https://wiki.archlinux.org/index.php/rsync#Trailing_slash_caveat>
The trailing slash caveat is also mentioned in tmux's man page.

`cp` on Arch Linux is part of GNU Coreutils.

`cp` on BSDs gives special treatment to source directories with a trailing slash "/".

Rsync  uses the BSDs `cp` behavior.

Creates a `libs` directory inside the `backup` directory. The result is `backup/libs`.
	rsync -r libs backup

Copies the contents of `libs` directory into `backup`:
	rsync -r libs/ backup

NOTE: It doesn't make any difference if the trailing slash is used on the destination directory. That trailing slash only matters for the source directory.


!! emtpy string gotcha !!
-------------------------

If you have "$dryrun" on the command, the empty string "" causes rsync to copy everything and ignore any filters you have, it seems.






Example _without_ trailing slash on source directory
----------------------------------------------------
	$ tree -C .
	.
	└── libs
	    ├── helpers.c
	    └── parser.c
	
	1 directory, 2 files
	$ rsync -rv libs backup
	sending incremental file list
	created directory backup
	libs/
	libs/helpers.c
	libs/parser.c
	
	sent 217 bytes  received 87 bytes  608.00 bytes/sec
	total size is 25  speedup is 0.08
	$ tree -C .
	.
	├── backup
	│   └── libs
	│       ├── helpers.c
	│       └── parser.c
	└── libs
	    ├── helpers.c
	    └── parser.c
	
	3 directories, 4 files

We **did not use the trailing slash on `libs` **(the source directory).


Example _with_ trailing slash on source directory
-------------------------------------------------

	----
	$ tree -C .
	.
	└── libs
	    ├── helpers.c
	    └── parser.c
	
	1 directory, 2 files
	$ rsync -rv libs/ backup
	sending incremental file list
	created directory backup
	./
	helpers.c
	parser.c
	
	sent 203 bytes  received 86 bytes  578.00 bytes/sec
	total size is 25  speedup is 0.09
	$ tree -C .
	.
	├── backup
	│   ├── helpers.c
	│   └── parser.c
	└── libs
	    ├── helpers.c
	    └── parser.c
	
	2 directories, 4 files
	$
	----

Notice that we **did indeed use the trailing slash on `libs/`**, the source directory.


example mistaken usage
----------------------

If you have `backup/libs` directory, and you do

	rsync -r libs backup/libs`

you'll end up with ```backup/libs/libs```. Since we used ```libs``` without the trailing slash, `rsync` copied the directory itself into ```backup/libs```. If we use the trailing slash on the source, then it copies the files inside that source to the destionation directory:

	rsync -r libs/ backup/libs

produces ```backup/libs/<other files and dirs here>```.

### incorrect/mistaken result

	----
	$ tree -C .
	.
	├── backup
	└── libs
	    ├── helpers.c
	    └── parser.c
	
	2 directories, 2 files
	$ rsync -rv libs backup/libs
	sending incremental file list
	created directory backup/libs
	libs/
	libs/helpers.c
	libs/parser.c
	
	sent 217 bytes  received 92 bytes  618.00 bytes/sec
	total size is 25  speedup is 0.08
	$ tree -C .
	.
	├── backup
	│   └── libs
	│       └── libs
	│           ├── helpers.c
	│           └── parser.c
	└── libs
	    ├── helpers.c
	    └── parser.c
	
	4 directories, 4 files
	$ 
	----


### correct/intended result

	----
	$ tree -C .
	.
	├── backup
	└── libs
	    ├── helpers.c
	    └── parser.c
	
	2 directories, 2 files
	$ rsync -rv libs/ backup/libs
	sending incremental file list
	created directory backup/libs
	./
	helpers.c
	parser.c
	
	sent 203 bytes  received 91 bytes  588.00 bytes/sec
	total size is 25  speedup is 0.09
	$ tree -C .
	.
	├── backup
	│   └── libs
	│       ├── helpers.c
	│       └── parser.c
	└── libs
	    ├── helpers.c
	    └── parser.c
	
	3 directories, 4 files
	$ 
	----


-i, --itemize-changes
---------------------
<https://stackoverflow.com/questions/4493525/rsync-what-means-the-f-on-rsync-logs>
Read the man and look for the section on ```--itemize-changes``` to understand the output rsync gives you with the ```-i``` option.
	blah blah >f.st...... /path/to/foo-bar.blah
	blah blah >f.st...... /path/to/foo-bar.blah
	blah blah .d..t...... /path/to/foo-bar.blah
	blah blah >f+++++++++ /path/to/foo-bar.blah
	blah blah >f+++++++++ /path/to/foo-bar.blah


include vs exclude
------------------
<https://stackoverflow.com/questions/19296190/rsync-include-from-vs-exclude-from-what-is-the-actual-difference>
Rsync builds an ordered list of include/exclude options as specified on the command line. Rsync checks each file and directory name against each exclude/include pattern in turn. The first matching pattern is acted on. If it is an exclude pattern, then that file is skipped. If it is an include pattern then that filename is not skipped. **If no matching include/exclude pattern is found then the filename is not skipped.**

So, to include only certain files, first include those files, and exclude all the rest. Includes must come first, then excludes. And we need to include all subdirectories otherwise files that would match inside those directories are will not match.

Include all subdirs, which allows to select all *.cfg files inside any subdir, and exclude everything else.
	--include="*/" --include="*.cfg" --exclude="*"


### when to use which
Since rsync does not skip any thing that does not match, that is, it includes things not explicitly excluded, for most cases you'll be better off to just exclude things you don't want to transfer rather than selectively including everything you want and then excluding what you don't want. This is especially true for web projects using php or ruby projects (with and without a framework).

So, for a Laravel project you transfer everything *except* a few files.

	# We don't want to override production uploads with
	# things we upload locally while testing in development.
	- public/uploads/
	# We'll use a different .env on the server.
	- .env
	- .git



### --include-from vs --exclude-from

[08:11:18] <Fernando-Basso> I'm confused again. It seems I can use `-' in a --include-from file and and a + in an --exclude-from file.
[08:11:50] <Fernando-Basso> Besides that, not sure in which situations it would be more advisable to use one of the other (or a combination of them).
[11:05:45] <BasketCase> Fernando-Basso: if you use the +/- syntax the options work the same

rsync script and sshpass
------------------------

Error:

	$ sshpass -p s3cr3t bash ./0noup/upsync.sh
	Host key verification failed.
	rsync error: unexplained error (code 255) at rsync.c(642) [sender=3.1.3]

Just connect once withouth sshpass so the host is added to known hosts file, then it will work.




Shared Hosting Services and Rsync
---------------------------------

### Locaweb

Locaweb supports rsync:
<https://wiki.locaweb.com.br/index.php?title=Rsync,_uma_forma_ágil_e_segura_como_o_SFTP_de_se_fazer_transferências_de_arquivos&redirect=no>

#### Parâmetros
As opções que utilizaremos neste exemplo serão "ravzup", onde:


* -r de cópia recursiva
* -a de modo de arquivamento
* -v de modo verbose, para mostrar na tela tudo o que ele está fazendo
* -z para compactar o arquivo durante a transferência (e descompactar no destino)
* -u modo update. Se o arquivo não foi atualizado, pula para o próximo, poupando tempo.
* -p preserva as permissões dos arquivos


NOTE: ```-r``` (```--recursive```) is  not needed because we are already using ```-a``` (```--archive```).

#### Sincronismo Local-Remoto
Supondo que você queira sincronizar seu diretório "/home/SEU_USUARIO/Desktop/MeuSite" do seu computador na pasta "public_html" do servidor, digite o comando a seguir "em seu computador":

	rsync -ravzup /home/SEU_USUARIO/Desktop/MeuSite/* SEU_LOGIN_FTP@ftp.seudominio:~/public_html/

Caso não tenha criado uma Chave para acesso SSH ou SFTP, será solicitada sua senha de FTP, do contrário, se você definiu um KeyPassphrase, ele será solicitado, se não a cópia ocorrerá diretamente sem solicitar nenhum dado.

NOTE: ```-r``` (```--recursive```) is  not needed because we are already using ```-a``` (```--archive```).
NOTE: There are some situations in which using ```--archive``` does not automatically imply ```--recursive```.


<https://wiki.locaweb.com.br/pt-br/Chave>
<https://wiki.locaweb.com.br/pt-br/SSH>
<https://wiki.locaweb.com.br/pt-br/SFTP>


Chat Conversations
------------------
[15:25:40] <Fernando-Basso> I'm confused. I have this command line: rsync -aivv --recursive --include-from=./0noup/test-patterns.txt ./
[15:26:33] <Fernando-Basso> If in test-patterns.txt I have only the like + foo/, it seems it is uploading other files than those in foo/ directory.
[15:27:18] <BasketCase> Fernando-Basso: includes only override excludes that follow
[15:28:18] <BasketCase> by default everything is included
[15:29:04] <Fernando-Basso>  What if I did '- *' in the last line?
[15:29:22] <Fernando-Basso> Like, '+ css/***\n- *' ?
[15:29:26] <BasketCase> then you would only get a dir named foo if it is in the top of the tree
[15:29:38] <BasketCase> it won't look inside of excluded dirs
[15:30:02] <BasketCase> + css/***   + */    - *
[15:30:08] <BasketCase> and add --prune-empty-dirs

Chat with BasketCase:

'risking' means it is eligible for ```--delete```.
risk, R files that match the pattern are not protected.





*****

[11:10:35] <BasketCase> sshpass is evil
[11:11:20] <Fernando-Basso> Yeah, but I am reluctant to send pub keys to shared host services...
[11:11:38] <BasketCase> why?  public keys should be public
[11:12:06] <Fernando-Basso> Yeah, I did not learn enough about this stuff yet.
[11:12:25] <Fernando-Basso> I don't want to use my personal keys with stuff from work.
[11:12:28] <BasketCase> I keep mine on my web site for anyone to see (so I can wget or curl them)
[11:13:05] <Fernando-Basso> Need to create another key and learn how to make ssh/rsync use that one instead for work related stuff.
[11:13:10] <BasketCase> keep separate keys for every system you ssh FROM not to
[11:13:45] <Fernando-Basso> My work computer is also my personal computer, since I work remotely.
[11:13:51] <BasketCase> same her
[11:13:53] <BasketCase> e
[11:14:07] <BasketCase> I have desktop key and laptop key
[11:14:47] <Fernando-Basso> How would I make rsync use a different key?
[11:15:05] <azarus> rsync -e "ssh -i <your key>"
[11:15:09] <BasketCase> if you must make more than one key make them host specific...
[11:15:14] <BasketCase> in ~/.ssh/config:
[11:15:17] <BasketCase> Host hostname
[11:15:25] <BasketCase>   IdentityFile /path/to/file
[11:15:38] <BasketCase> hostname can also be *.domain
[11:15:47] <azarus> I use the ssh config file like aliases :3
[11:15:53] <azarus> Host vps
[11:16:02] <azarus>     Hostnbame realhost.name
[11:16:14] <azarus> Hostname*
[11:16:44] <BasketCase> I have DNS so I rarely do that
[11:17:17] <azarus> Sometimes I also do:
[11:17:19] <azarus> Host phone
[11:17:25] <azarus>     Hostname localhost
[11:17:31] <azarus>     Port 8022
[11:17:43] <azarus> because my phone has a sshd running on it that can be forwarded
[11:18:02] <azarus> to and from*
[11:20:33] <BasketCase> your -e method does also work.  that is a good idea if you need to make a less protected ssh key that is only allowed to rsync for cron jobs and similar





*****

[18:13:48] <Fernando-Basso> I wonder about permissions for rsync'ing ~/.ssh, ~/.gnupg, ~/.mozilla, and some other directories with sensitive information.
[18:14:19] <Fernando-Basso> Would --perms be advisable?
[18:16:14] <Fernando-Basso> Ah, -a implies -p.
[18:28:12] <BasketCase> that is how I do backups
[18:29:49] <specing> I don't see why one would have to handle these dirs in any special manner
[18:30:04] <specing> and -a should be used for backups anyway
[18:30:09] <specing> I add -HAX too
[18:47:59] <Fernando-Basso> All right folks, thanks.



*****


### ignore timestamp
rsync \
"${try}"  \
--itemize-changes \
--verbose \
--archive \
--compress \
--checksum \
--update \
--filter='- .git/' \
--filter='- .gitignore' \
--filter='- .editorconfig' \
--filter='- README.adoc' \
--filter='- .htaccess' \
--filter='- .env' \
--filter='- node_modules/' \
--filter='- storage/' \
--filter='- public/uploads/' \


sending incremental file list
.f..t...... public/mix-manifest.json
.f..t...... public/assets/css/admin/bootstrap-custom-admin.css
.f..t...... public/assets/css/admin/main-admin.css
.f..t...... public/assets/css/site/bootstrap-custom-site.css
<fc.t...... public/assets/css/site/main-site.css
.f..t...... public/assets/fa47/font-awesome.css
.d..t...... public/assets/images/shared/
.f..t...... public/assets/images/shared/favicon.ico
.f..t...... public/assets/images/shared/form-contact-bg.png
.f..t...... public/assets/images/shared/logo1.png
.f..t...... public/assets/images/shared/spinner.gif
.f..t...... public/assets/images/shared/sprite-vbsmidia-logo-squares-lightgray-white.png
.f..t...... public/assets/images/shared/sprite-vbsmidia-logo-squares.png
.d..t...... public/assets/images/site/banners/
.f..t...... public/assets/images/site/banners/banner-solucoes-que-geram-valor.jpg
.f..t...... public/assets/images/site/banners/estrutura-da-empresa.jpg
.f..t...... public/assets/images/site/banners/fluig.jpg
.f..t...... public/assets/images/site/banners/pqc.jpg
[17:24:59] <Fernando-Basso> Even by using --checksum, rsync sends files based on times, it seems. <https://pastebin.com/raw/eV65s0Hq>
[17:29:25] <BasketCase> it only sent 1 of those files.  the others it just corrected the wrong timestamp.
[17:30:48] <BasketCase> (not that --checksum is a good idea)
[18:27:01] <Fernando-Basso> My webpack build setup generates the assets again every time I start it. So, the timestamps change. I wish to ignore time modification.
[18:37:07] <BasketCase> then don't use --archive

*****



Resources and Links
-------------------

Has some links on the top that makes navigation a bit easier.
<http://man7.org/linux/man-pages/man1/rsync.1.html>

Link with several sections like examples, faq, documentation, etc:
<https://rsync.samba.org/>

Topic in #rsync freenode IRC channel (as of 2018/04/04):
"""
burning fingers since '08 | <https://sanitarium.net/rsyncfaq/> | for help with rsync and (if you're lucky) derived tools | DO: post full command line & errors, see what -i thinks, read manpage & google, wait for answer
"""

