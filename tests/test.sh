#!/bin/sh

SRCDIR=$HOME/GIT/FREETON/TONLABS/samples/solidity

ft contract --build $SRCDIR/9_PiggyBank.sol -f || exit 2
ft contract --build $SRCDIR/9_PiggyBank_Owner.sol -f || exit 2
ft contract --build $SRCDIR/9_PiggyBank_Stranger.sol -f || exit 2

ft switch sandbox1 && ft node --stop
ft switch testnet
ft switch --remove sandbox1
ft switch --create sandbox1 || exit 2

ft node --start || exit 2
sleep 10

ft node --give user1 || exit 2

ft account --create customBank --contract 9_PiggyBank || exit 2
ft multisig -a user1 --transfer 10 --to customBank --sponsor || exit 2

ft contract --deploy 9_PiggyBank_Owner || exit 2
ft contract --deploy 9_PiggyBank_Stranger || exit 2
ft contract --dst customBank --deploy 9_PiggyBank --params '{ "own" : "%{account:address:9_PiggyBank_Owner}", "lim": 1000000 }' || exit 2

ft call 9_PiggyBank_Owner addToDeposit '{ "bankAddress": "%{account:address:customBank}", "amount": 1000000 }' || exit 2

ft call --local customBank getData && exit 2

