#!/usr/bin/env bash

# Place where I manipulate files when I an studying my Anki cards
# and need to run quick snippets of code to assert my knowledge of
# the things I am committing to the long-term memory.
dir="${HOME}/Projects/proghowto/ruby"
ses='ruby-studies'

if [ ! -d "$dir" ] ; then
    echo "Directory \`$dir\` not found."
    echo "The project “$ses” doesn't seem to be on this machine."
    echo "Bailing out..."
    exit 0
fi

tmux -f ~/Projects/dotfiles/.tmux.conf new-session -d -s "$ses" -c "$dir"

tmux rename-window 'vim'

# airline themes:
# xtermlight        - blueish
# luna              - greenish, red when file edited.
# base16_ocean      - grayish, no different color when file is not saved
# light             - green/pinky
tmux send-keys "vim +'colorscheme mytheme1' +'AirlineTheme dark_minimal' +NERDTree" C-j

tmux new-window -t "${ses}:2" -n 'ruby -w' -c "$dir" \; \
    send-keys 'ruby -w'

tmux new-window -t "${ses}:3" -n 'pry' -c "$dir" \; \
    send-keys 'pry'

tmux new-window -t "${ses}:4" -n 'shell' -c "$dir"

tmux select-window -t "${ses}:1"

tmux -2 attach-session -t "${ses}"

