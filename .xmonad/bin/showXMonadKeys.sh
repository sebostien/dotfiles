#!/bin/zsh

FILE=./showKeys

cd ~/.xmonad/lib/

if [[ -f "$FILE" ]]; then
else
  # Compile haskell file
  ghc -o ./showKeys ./Main.hs
fi

./showKeys | \
  yad \
    --text-info \
    --back=#282c34 \
    --fore=#46d9ff \
    --title=Keybindings \
    --undecorated \
    --margins=4 \
    --geometry=720x960
