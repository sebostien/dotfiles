function config --wraps='git --git-dir=$HOME/dotfiles --work-tree=$HOME' --description 'Git dotfiles'
  git --git-dir=$HOME/dotfiles --work-tree=$HOME $argv
end
