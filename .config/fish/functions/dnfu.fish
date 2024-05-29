function dnfu --wraps='sudo dnf upgrade' --description 'alias dnfu sudo dnf upgrade'
  sudo dnf upgrade $argv
end
