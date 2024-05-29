#!/bin/bash

cargo install zellij

cd /home/sn/.config/zellij/ || exit

mkdir -p plugins/

wget --output-document=./plugins/zjstatus.wasm https://github.com/dj95/zjstatus/releases/latest/download/zjstatus.wasm
wget --output-document=./plugins/zellij_forgot.wasm https://github.com/karimould/zellij-forgot/releases/latest/download/zellij_forgot.wasm
wget --output-document=./plugins/zellij-jump-list.wasm https://github.com/blank2121/zellij-jump-list/releases/latest/download/zellij-jump-list.wasm
