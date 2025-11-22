#!/bin/bash
# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# dotfiles
alias config="/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"

export EDITOR=nvim
export LANG=en_US.UTF-8

export PATH=$PATH:/home/sn/scripts
export PATH=$PATH:/home/sn/go/bin
export PATH=$PATH:/home/sn/.local/bin

# Include mason in path
if [ -d "$HOME/.local/share/nvim/mason/bin"] ; then
  PATH="$HOME/.local/share/nvim/mason/bin:$PATH"
fi

. "$HOME/.cargo/env"
