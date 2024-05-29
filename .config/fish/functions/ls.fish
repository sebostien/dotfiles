function ls --wraps=eza --description 'alias ls eza'
  eza -al --color=always --group-directories-first $argv
end

