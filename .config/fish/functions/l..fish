function l. --wraps=eza --description 'ls dotfiles'
  eza -a | grep -E "^\."
end
