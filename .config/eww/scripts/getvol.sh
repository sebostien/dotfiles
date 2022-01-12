#!/bin/sh
amixer sget Master | grep Mono: | awk -F'[][]' '{ print $2 }' | tr -d '%'