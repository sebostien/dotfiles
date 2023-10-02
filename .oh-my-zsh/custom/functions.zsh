#################
### FUNCTIONS ###
#################

## print hex value of a number
function hex() {
   emulate -L zsh
   if [[ -n "$1" ]]; then
       printf "%x\n" $1
   else
       print 'Usage: hex <number-to-convert>'
       return 1
   fi
}

## Search DuckDuckGo
## From: https://github.com/sineto/web-search
function ddg() {
  url="https://duckduckgo.com/"

  if [[ $# -eq 0 ]]; then
    xdg-open "$url"
  fi

  url="${url}?q="
  while [[ $# -gt 0 ]]; do
    url="$url$1+"
    shift
  done

  # Remove last '+'
  url="${url%?}"
  xdg-open "$url"
}
