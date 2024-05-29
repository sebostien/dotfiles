function lgi --wraps=eza --description 'ls respect gitignore'
  eza -al --group-directories-first --gitignore --git $argv
end
