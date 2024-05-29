function fish_setup --description 'Install fish plugins'
  set --universal nvm_default_packages pnpm yarn

  # Completions
  zellij setup --generate-completion fish > ~/.config/fish/completions/zellij.fish
  atuin gen-completions --shell fish > ~/.config/fish/completions/atuin.fish
end
