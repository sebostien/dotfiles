function lt --wraps=eza --description 'list tree depth {n}'
  set d 2
  if count $argv > /dev/null
    set d $argv[1]
  end
  eza -aT --color=always --level=$d --group-directories-first
end
