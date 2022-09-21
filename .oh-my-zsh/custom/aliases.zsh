###############
### ALIASES ###
###############

# Set shell variable
SHELL=/bin/zsh

# dotfiles
alias config="/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"

# cd
alias dt="cd ~/Desktop"
alias dl="cd ~/Downloads"
alias docs="cd ~/Documents"

# vim
alias vim="nvim"

# Changing "ls" to "exa"
alias ls='exa -al --color=always --group-directories-first' # my preferred listing
alias la='exa -a --color=always --group-directories-first'  # all files and dirs
alias ll='exa -l --color=always --group-directories-first'  # long format
alias lt='exa -aT --color=always --group-directories-first' # tree listing
alias l.='exa -a | egrep "^\."'

# Colorize grep output
alias grep='grep --color=auto'

# Confirm overwrite
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

# Flags
alias df='df -h'      # human-readable sizes
alias free='free -m'  # show sizes in MB

# Calendar
alias kal="cal -ymw"

# Weather
alias weather="curl wttr.in"
