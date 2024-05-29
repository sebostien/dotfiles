function lg --wraps=eza --description 'ls git status'
  eza -al --group-directories-first --git $argv
end
