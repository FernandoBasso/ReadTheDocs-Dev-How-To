---
title: File Conversion on The Command Line
description: Examples and tips on doing file conversion on the command line
---



# File Conversion

[TOC]

## Asciidoc[tor] to Markdown

Pandoc can't convert directly from Asciidoc to Markdown. So, first convert Asciido[ctor] files to Docbook and then convert Docbook files to Markdown (gfm here):

```bash
for f in *.adoc ; do asciidoctor -b docbook "$f" ; done

for f in *.xml ; do
	pandoc -s "$f" -f docbook "$f" -t gfm --wrap=none -o "${f%.*}.md"
done
```

Remove the .xml files (and possibly the .adoc files):

```shell-session
rm -v *.xml
```

