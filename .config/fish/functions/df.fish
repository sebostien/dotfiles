function df --wraps 'df -h' --description 'human-readable sizes'
  command df -h $argv
end
