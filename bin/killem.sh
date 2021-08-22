#!/usr/bin/env bash

to_kill=$1
first_char=${to_kill:0:1}
rest_chars=${to_kill:1}

ps_output=`ps aux | grep [$first_char]$rest_chars | grep -v killem`

printf "$ps_output"

echo
echo
echo "SHOULD I KILL THESE? (yes/anything else)"
read SHOULD_KILL

if [[ "$SHOULD_KILL" ==  "yes" ]];
then
    pids=`echo -e "$ps_output" | awk '{print $2}'`
    echo killing $pids
    kill -9 $pids
else
    echo wont kill
fi
