---
title: Useful Unicode Symbols | Character Encodings
description: A list of useful Unicode symbols with examples on how to type and or print them from the command line and in a few programming languages.
---

# Useful Unicode Symbols

```text
👁   \U0001f4a1 eye, toggle plain text password view
✔   \u2714 check mark
✔   \u2713 chec mark
✕   \u2715
✗   \u2717
⏳  \u23f3 sands of time
‴   \u2034 triple prime
‷   \u2037 reversed triple prime
💦  \U0001f4a6 splash sweat
😓  \U0001f613 splash cold sweat

♩ 9833, Hex 2669, Octal 23151
♪  S9834, Hex 266a, Octal 23152
♫  9835, Hex 266b, Octal 23153
♬ 9836, Hex 266c, Octal 23154
```

> ♫ Join us and share the software, ♪
> ♪ you'll be free hackers... ♩ ♬

```
elem.textcontent = '\u{0001f4a9}';

html (use the &#x notation):
	<input name='utf8' value='&#x2714;'>

Print from shell:

```shell-session
echo -e '\u2713'     # u (lowercase)
echo -e '\U0001f4a9' # U (UPPERCASE)
```

Get hex from char:

```shell-session
$ echo -n 💩 | hexdump
```

Get info on a char:

- vim: ga on char
- emacs: C-8 C-x = on char

Make the menu key as the compose key on Linux:

```shell-session
$ setxkbmap -option compose:menu
```

```
<Multi_key> <less> <3>                  : "♥"   U2665 # BLACK HEART SUIT
<Multi_key> <colon> <parenright>        : "☺"   U263A # WHITE SMILING FACE
<Multi_key> <colon> <parenleft>         : "☹"   U2639 # WHITE FROWNING FACE
```

Enter Chars by UTF-8 code points (terminal, browsers, etc):

Hit ctrl+shift+u followed by their unicode hexadecimal codepoint.
Examples:

```text
203d          ‽     INTERROBANG
2713          ✓     (vim ^kOK)
2714          ✔
2717          ✗     (vim ^kxx)
2718          ✘
2190          ←
2192          →
0001f4a9      💩
2639          ☹
263a          ☺
2665          ♥
2605          ★
2606          ☆
262a          ☪
066d          ٭
2260          ≠     (vim ^k=!)
2400          ␀
2420          ␠

 ✓ ✗ ← → ☹ ☺ ♥ ★ ☆ ☪ 

<< ✓ ✗ ← → ☹ ☺ ♥ ★ ☆ ☪ ٭ ≠ ␀ ␠ >>

## tree command chars

```text
│   ├── tasklist
│   │   ├── buffer
│   │   │   ├── get.vim
│   │   │   └── set.vim
│   │   └── node.vim
│   └── tasklist.vim

│ \u2502, not the normal "pipe" char.
─ \u2500, not a "dash" or "minus" char.
├ \u251c
└ \u2514
```

## Ruby

```rb
'ção'.length    # 3    works...
'ção'.upcase    # çãO  incorrect...
'ÇÃO'.downcase  # ÇÃo  incorrect...
```

Using rails active support:

```
'ção'.mb_chars.length.to_s          # 3
'ção'.mb_chars.upcase.to_s          # ÇÃO
'ÇÃO'.mb_chars.downcase.to_s        # ção
```

There is also UnicodeUtils gem.
