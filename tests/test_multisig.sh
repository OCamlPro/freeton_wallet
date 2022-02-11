#!/bin/bash

FT="ft --echo"

$FT switch to sandbox2 && $FT node stop
$FT switch to testnet
$FT switch remove sandbox2

$FT switch create sandbox2 || exit 2
$FT node start || exit 2
sleep 10

$FT node give user1 || exit 2

$FT multisig new MULTISIG user1 user2 user3 --credit 900 || exit 2
$FT multisig info MULTISIG || exit 2

echo
echo TRANSACTION 1
echo

$FT multisig transfer 100 --from MULTISIG --to user2 --src user1 --sponsor || exit 2
$FT multisig list transactions MULTISIG || exit 2
$FT multisig confirm transaction MULTISIG --src user2 --all || exit 2
$FT account state MULTISIG

echo
echo UPDATE 1
echo

$FT multisig update MULTISIG --src user1 --reset-custodians user1 user3 user4 -o updateId.txt --subst "@%{res:updateId}" || exit 2
$FT multisig confirm update MULTISIG --src user2 --all || exit 2

$FT multisig list updates MULTISIG || exit 2
$FT multisig execute update MULTISIG $(cat updateId.txt) --src user1 || exit 2
$FT multisig info MULTISIG || exit 2

echo
echo UPDATE 2
echo

$FT multisig update MULTISIG --src user1 --keep-custodians add: user2 del: user3 -o updateId.txt --subst "@%{res:updateId}" || exit 2
$FT multisig confirm update MULTISIG --src user4 --all || exit 2

$FT multisig list updates MULTISIG || exit 2
$FT multisig execute update MULTISIG $(cat updateId.txt) --src user4 || exit 2
$FT multisig info MULTISIG || exit 2

exit 0
