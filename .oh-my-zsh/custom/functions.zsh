#################
### FUNCTIONS ###
#################

## print hex value of a number
hex() {
   emulate -L zsh
   if [[ -n "$1" ]]; then
       printf "%x\n" $1
   else
       print 'Usage: hex <number-to-convert>'
       return 1
   fi
}
