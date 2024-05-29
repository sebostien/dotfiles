function hex --description 'Convert number to hex'
  if count $argv > /dev/null
    printf "%x\n" $argv[1]
  else
    printf 'Usage: hex <number-to-convert>'
    return 1
  end
end
