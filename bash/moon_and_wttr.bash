#!/bin/bash
#set -euo pipefail
#IFS=$'\n\t'

# display the weather by default in my location
function wttr()
{
    # change Paris to your default location
    local request="wttr.in/${1-47.305464,-122.215806}"
    [ "$COLUMNS" -lt 125 ] && request+='?n'
    curl -H "Accept-Language: ${LANG%_*}" --compressed "$request"
}

# display the current phase of the moon
function moon()
{
    # change Paris to your default location
    local request="wttr.in/${1-Moon}"
    [ "$COLUMNS" -lt 125 ] && request+='?n'
    curl -H "Accept-Language: ${LANG%_*}" --compressed "$request"
}
