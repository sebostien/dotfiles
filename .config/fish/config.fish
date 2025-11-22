fish_add_path $HOME/scripts
fish_add_path $HOME/go/bin
fish_add_path $HOME/.cargo/bin
fish_add_path $HOME/.local/bin
fish_add_path $HOME/.local/share/pnpm
fish_add_path $HOME/.local/share/nvm/v22.1.0/bin/


# Include mason in path
if test -d "$HOME/.local/share/nvim/mason/bin"
  fish_add_path $HOME/.local/share/nvim/mason/bin
end

if test -f /usr/bin/nvim 
  set -x MANPAGER '/usr/bin/nvim +Man!'
end

set -x SHELL /usr/bin/fish
set -x EDITOR nvim
set -x VISUAL nvim
set -x LANG en_US.UTF-8

function fish_greeting
end

if status is-interactive
  zoxide init --cmd cd fish | source
  starship init fish | source
  navi widget fish | source # CTRL-G
  atuin init fish --disable-up-arrow | source # CTRL-R
  bits completions fish | source
end
