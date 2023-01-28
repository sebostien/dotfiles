#!/bin/sh

curl -s ifconfig.co/json | \
  jq '.["ip", "country"]' | \
  sed -e 'N;s/\n//' -e 's/\"\"/ - /' -e 's/\"//g'
