#!/bin/bash

##############
#### Help ####
##############
print_help()
{
   echo "Send notifications to ntfy."
   echo
   echo "Syntax: ntfy.sh [OPTIONS] [description...]"
   echo
   echo "OPTIONS"
   echo "-T, --title        Title for the notification"
   echo "-p, --priority     Priority, in the range [0-5]"
   echo "-t, --tags         Tags to use, see https://docs.ntfy.sh/emojis/"
   echo "-m, --markdown     Use markdown for description"
   echo "-h, --help         Show help"
   echo
}

##############

NTFY_TOKEN=$(~/.token NTFY_TOKEN)
NTFY_TOPIC=$(~/.token NTFY_TOPIC)
NTFY_URL=$(~/.token NTFY_URL)

TITLE="$NTFY_URL"
PRIORITY="default"
TAGS="computer"
MARKDOWN=0

MESSAGE_PARTS=()

while [[ $# -gt 0 ]]; do
  case $1 in
    -h|--help)
      print_help
      exit 1
      ;;
    -p|--priority)
      PRIORITY="$2"
      shift
      shift
      ;;
    -T|--title)
      TITLE="$2"
      shift
      shift
      ;;
    -t|--tags)
      TAGS="$2"
      shift
      shift
      ;;
    -m|--markdown)
      MARKDOWN=1
      shift
      ;;
    -*)
      echo "Unknown option $1"
      exit 1
      ;;
    *)
      MESSAGE_PARTS+=("$1")
      shift
      ;;
  esac
done

if [ ${#MESSAGE_PARTS} -eq 0 ]; then
  MESSAGE="No description"
else
  MESSAGE=$(IFS=" "; echo "${MESSAGE_PARTS[*]}")
fi

TITLE="-H \"Title: $TITLE\""
TAGS="-H \"X-Tags: $TAGS\""
PRIORITY="-H \"X-Priority: $PRIORITY\""
MESSAGE="-d \"$MESSAGE\""

if [ $MARKDOWN -eq 1 ]; then
  MARKDOWN="-H \"Markdown: yes"\"
else
  MARKDOWN=""
fi

COMMAND="curl $NTFY_URL/$NTFY_TOPIC -u :$NTFY_TOKEN $TITLE $MESSAGE $PRIORITY $MARKDOWN $TAGS"
bash -c "$COMMAND"
