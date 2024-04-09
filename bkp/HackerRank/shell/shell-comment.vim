"
" Matches text between
"
"     : <<-'////'
"     text
"     ...
"     ////
"
" This is a null statement in bash which I'm using to create a comment-like
" piece of text.
"

"syntax match ShellDoc /^: <<-'\/\/\/\/'\_.\{-}\/\/\/\/$/hs=s+11,he=e-4
"highlight link ShellDoc Comment

syntax match ShellDoc /^: <<-'\/\/\/\/'\_.\{-}\/\/\/\/$/
highlight link ShellDoc Comment

