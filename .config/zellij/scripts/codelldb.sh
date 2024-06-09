#!/bin/bash

PORT=13000

while sleep 1; do
  echo "Started codelldb on port $PORT"
  ~/.local/share/nvim/mason/bin/codelldb --port $PORT
  echo "Restarting..."
done
