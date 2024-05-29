function wttr --wraps='curl wttr.in' --description 'Weather info'
  curl wttr.in $argv
end
