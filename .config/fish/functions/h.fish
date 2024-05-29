function h --description 'fuzzy history'
  atuin history list --format "{command}" | fzf
end
