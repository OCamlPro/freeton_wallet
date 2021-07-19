#!/bin/bash

JURY_ACCOUNT="$1"
CONTEST_ADDR="$2"
SUBMISSION_ID="$3"
MARK="$4"
COMMENTS_FILE="$5"

if [ "$#" -ne 5 ]; then
    echo "USAGE:"
    echo "voteForCommented.sh <JURY_ACCOUNT> <CONTEST_ADDRESS> <SUBMISSION_ID> <MARK> <COMMENTS_FILE>"
    exit 0
fi

COMMENT=$(xxd -p "$COMMENTS_FILE" | tr -d '\n')

echo ft switch to mainnet
ft switch to mainnet 

echo ft account create contest --address "$CONTEST_ADDR" -f --contract FreeTonContest
ft account create contest --address "$CONTEST_ADDR" -f --contract FreeTonContest || exit 2

echo ft account info contest
ft account info contest || exit 2

echo ft call --sign $JURY_ACCOUNT contest voteForCommented \
      "{\"id\":$SUBMISSION_ID,\"mark\":$MARK,\"comment\":\"$COMMENT\"}"
ft call --sign $JURY_ACCOUNT contest voteForCommented \
	    "{\"id\":$SUBMISSION_ID,\"mark\":$MARK,\"comment\":\"$COMMENT\"}" || exit 2
