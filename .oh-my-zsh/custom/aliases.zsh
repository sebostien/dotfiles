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
if hash exa 2>/dev/null; then
  alias ls='exa -al --color=always --group-directories-first'
  alias lg='exa -al --group-directories-first --git' # Git status
  alias lgi='exa -al --group-directories-first --gitignore --git' # Obey gitignore
  alias l.='exa -a | egrep "^\."' # dotfiles

  alias lt='exa -aT --color=always --level=2 --group-directories-first' # tree listing
  alias llt='exa -aT --color=always --level=4 --group-directories-first' # more depth tree listing
else
  alias l='ls -lah'
  alias ll='ls -alF'
  alias la='ls -A'
fi

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
