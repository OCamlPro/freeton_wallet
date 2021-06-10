#!/bin/bash

CONTEST_ADDR="$1"
SUBMISSION_ID="$2"
MARK="$3"
COMMENTS_FILE="$4"

if [ "$#" -ne 4 ]; then
    echo "USAGE:"
    echo "voteForCommented.sh <CONTEST_ADDRESS> <SUBMISSION_ID> <MARK> <COMMENTS_FILE>"
    exit 0
fi

COMMENT=$(xxd -p $4 | tr -d '\n')

ft account --create contest --address "$CONTEST_ADDR" -f --contract FreeTonContest
ft account contest

ft call --sign fabrice contest voteForCommented \
	    "{\"id\":$SUBMISSION_ID,\"mark\":$MARK,\"comment\":\"$COMMENT\"}"
