
Sub-commands and Arguments
==========================
Common arguments to all sub-commands:


* :code:`-q` or :code:`--quiet`   Set verbosity level to 0

* :code:`--switch STRING`   Set switch

* :code:`-v` or :code:`--verbose`   Increase verbosity level

Overview of sub-commands::
  
  account
    Get account info (local or from blockchain), or create/modify/delete accounts.
  
  call
    Manage contracts
  
  client
    Call tonos-cli, use -- to separate arguments
  
  config
    Modify configuration
  
  contract
    Manage contracts
  
  genaddr
    Generate new addr (default is for a SafeMultisigWallet, use 'ft list' for more)
  
  init
    Initialize with TON Labs binary tools
  
  inspect
    Monitor a given account
  
  list
    List known contracts
  
  multisig
    Manage a multisig-wallet (create, confirm, send)
  
  node
    Manage local nodes
  
  output
    Call tonos-cli, use -- to separate arguments
  
  switch
    Change current switch
  
  test
    For testing only
  
  utils
    Some useful tools
  
  watch
    Monitor a given account


drom account
~~~~~~~~~~~~~~

Get account info (local or from blockchain), or create/modify/delete accounts.



**DESCRIPTION**


This command can perform the following actions:

* 1.
  Display information on given accounts, either locally or from the blockchain

* 2.
  Create new accounts

* 3.
  Add information to existing accounts

* 4.
  Delete existing accounts


**DISPLAY LOCAL INFORMATION**


Examples:
::
  
  ft account --list
::
  
  ft account my-account --info


**DISPLAY BLOCKCHAIN INFORMATION**


Accounts must have an address on the blockchain.

Examples:
::
  
  ft account my-account
::
  
  ft account


**CREATE NEW ACCOUNTS**


Examples:
::
  
  ft account --create account1 account2 account3
::
  
  ft account --create new-account --passphrase "some known passphrase"
::
  
  ft account --create new-account --contract SafeMultisigWallet
::
  
  ft account --create new-address --address 0:1234...

Only the last one will compute an address on the blockchain, since the contract must be known.


**COMPLETE EXISTING ACCOUNTS**


Examples:
::
  
  ft account old-account --contract SafeMultisigWallet


**DELETE EXISTING ACCOUNTS**


Examples:
::
  
  ft account --delete account1 account2

**USAGE**
::
  
  drom account ARGUMENTS [OPTIONS]

Where options are:


* :code:`ARGUMENTS`   Name of account

* :code:`--address STRING`   Address for account

* :code:`--contract STRING`   Contract for account

* :code:`--create`   Create new account

* :code:`--delete`   Delete old accounts

* :code:`--info`   Display account parameters

* :code:`--keyfile STRING`   Key file for account

* :code:`--list`   List all accounts

* :code:`--live`   Open block explorer on address

* :code:`--multisig`   Contract should be multisig

* :code:`--passphrase STRING`   Passphrase for account

* :code:`--surf`   Contract should be TON Surf contract

* :code:`--wc INT`   WORKCHAIN The workchain (default is 0)

* :code:`--whois STRING`   ADDR Returns corresponding key name


drom call
~~~~~~~~~~~

Manage contracts


**USAGE**
::
  
  drom call ARGUMENTS [OPTIONS]

Where options are:


* :code:`ARGUMENTS`   ACCOUNT METH [PARAMS] arguments

* :code:`-o STRING` or :code:`--output STRING`   FILE Save result to FILE (use - for stdout)

* :code:`--run`   Run locally

* :code:`--sign STRING`   ACCOUNT Sign message with account


drom client
~~~~~~~~~~~~~

Call tonos-cli, use -- to separate arguments


**USAGE**
::
  
  drom client ARGUMENTS [OPTIONS]

Where options are:


* :code:`ARGUMENTS`   Arguments to tonos-cli

* :code:`--exec`   Do not call tonos-cli, the command is in the arguments

* :code:`--stdout STRING`   FILE Save command stdout to file


drom config
~~~~~~~~~~~~~

Modify configuration


**USAGE**
::
  
  drom config [OPTIONS]

Where options are:


* :code:`--deployer STRING`   ACCOUNT Set deployer to account ACCOUNT


drom contract
~~~~~~~~~~~~~~~

Manage contracts


**USAGE**
::
  
  drom contract [OPTIONS]

Where options are:


* :code:`--build STRING`   Build a contract and remember it

* :code:`--create STRING`   ACCOUNT Create ACCOUNT by deploying contract (with --deploy)

* :code:`--deploy STRING`   CONTRACT Deploy contract CONTRACT

* :code:`--force`   Override existing contracts

* :code:`--import STRING`   CONTRACT Deploy contract CONTRACT

* :code:`--list`   List known contracts

* :code:`--new STRING`   NAME Create template file for contract NAME

* :code:`--newi STRING`   NAME Create template file for interface NAME

* :code:`--params STRING`   PARAMS Constructor/call Arguments ({} by default)

* :code:`--replace STRING`   ACCOUNT Replace ACCOUNT when deploying contract (with --deploy)

* :code:`--sign STRING`   ACCOUNT Sign with account ACCOUNT


drom genaddr
~~~~~~~~~~~~~~

Generate new addr (default is for a SafeMultisigWallet, use 'ft list' for more)


**USAGE**
::
  
  drom genaddr ARGUMENT [OPTIONS]

Where options are:


* :code:`ARGUMENT`   Name of key

* :code:`--contract STRING`   Name of contract

* :code:`--create`   Create new key

* :code:`--surf`   Use TON Surf contract

* :code:`--wc INT`   WORKCHAIN The workchain (default is 0)


drom init
~~~~~~~~~~~

Initialize with TON Labs binary tools


**USAGE**
::
  
  drom init [OPTIONS]

Where options are:


* :code:`--clean`   Clean before building

* :code:`--client`   Only build and install the client, not solc&linker


drom inspect
~~~~~~~~~~~~~~

Monitor a given account


**USAGE**
::
  
  drom inspect [OPTIONS]

Where options are:


* :code:`-2`   Verbosity level 2

* :code:`-3`   Verbosity level 3

* :code:`-a STRING`   ACCOUNT Inspect account TR_ID on blockchain

* :code:`-b STRING`   BLOCK Inspect block TR_ID on blockchain

* :code:`--bn STRING`   LEVEL Inspect block at LEVEL on blockchain

* :code:`-h`   Inspect head

* :code:`--limit INT`   LIMIT Limit the number of results to LIMIT

* :code:`-m STRING`   MSG_ID Inspect message MSG_ID on blockchain

* :code:`--shard STRING`   SHARD Block info level/head for this shard

* :code:`--shard-account STRING`   ACCOUNT Block info level/head for this shard

* :code:`--shard-block STRING`   BLOCK_ID Block info level/head for this shard

* :code:`-t STRING`   TR_ID Inspect transaction TR_ID on blockchain


drom list
~~~~~~~~~~~

List known contracts


**USAGE**
::
  
  drom list [OPTIONS]

Where options are:



drom multisig
~~~~~~~~~~~~~~~

Manage a multisig-wallet (create, confirm, send)



**DESCRIPTION**


This command is used to manage a multisig wallet, i.e. create the wallet, send tokens and confirm transactions.


**CREATE MULTISIG**


Create an account and get its address:
::
  
  # ft account --create my-account
  # ft genaddr my-account

Backup the account info off-computer.

The second command will give you an address in 0:XXX format. Send some tokens on the address to be able to deploy the multisig.

Check its balance with:
::
  
  # ft account my-account

Then, to create a single-owner multisig:
::
  
  # ft multisig -a my-account --create

To create a multi-owners multisig:
::
  
  # ft multisig -a my-account --create owner2 owner3 owner4

To create a multi-owners multisig with 2 signs required:
::
  
  # ft multisig -a my-account --create owner2 owner3 --req 2

To create a multi-owners multisig not self-owning:
::
  
  # ft multisig -a my-account --create owner1 owner2 owner3 --not-owner

Verify that it worked:
::
  
  # ft account my-account -v


**GET CUSTODIANS**


To get the list of signers:
::
  
  # ft multisig -a my-account --custodians"


**SEND TOKENS**


Should be like that:
::
  
  # ft multisig -a my-account --transfer 100.000 --to other-account

If the target is not an active account:
::
  
  # ft multisig -a my-account --transfer 100.000 --to other-account --parrain

To send all the balance:
::
  
  # ft multisig -a my-account --transfer all --to other-account


**LIST WAITING TRANSACTIONS**


Display transactions waiting for confirmations:
::
  
  # ft multisig -a my-account --waiting


**CONFIRM TRANSACTION**


Get the transaction ID from above, and use:
::
  
  # ft multisig -a my-account --confirm TX_ID

**USAGE**
::
  
  drom multisig ARGUMENTS [OPTIONS]

Where options are:


* :code:`ARGUMENTS`   Owners of contract for --create

* :code:`-a STRING` or :code:`--account STRING`   ACCOUNT The multisig account

* :code:`--confirm STRING`   TX_ID Confirm transaction

* :code:`--contract STRING`   CONTRACT Use this contract

* :code:`--create`   Deploy multisig wallet on account

* :code:`--custodians`   List custodians

* :code:`--debot`   Start the multisig debot

* :code:`--not-owner`    Initial account should not be an owner

* :code:`--parrain`    Transfer to inactive account

* :code:`--req INT`   REQ Number of confirmations required

* :code:`--surf`   Use Surf contract

* :code:`--to STRING`   ACCOUNT Target of a transfer

* :code:`--transfer STRING`   AMOUNT Transfer this amount

* :code:`--waiting`    List waiting transactions

* :code:`--wc INT`   WORKCHAIN The workchain (default is 0)


drom node
~~~~~~~~~~~

Manage local nodes


**USAGE**
::
  
  drom node [OPTIONS]

Where options are:


* :code:`--give STRING`   ACCOUNT Give 1000 TON from giver to ACCOUNT ('all' for user*)

* :code:`--start`   Start network node

* :code:`--stop`   Stop network node

* :code:`--web`   Open Node GraphQL webpage


drom output
~~~~~~~~~~~~~

Call tonos-cli, use -- to separate arguments


**USAGE**
::
  
  drom output [OPTIONS]

Where options are:


* :code:`--addr STRING`   ACCOUNT Output address of account

* :code:`--file STRING`   FILE Output content of file after substitution

* :code:`--keyfile STRING`   ACCOUNT Output key file of account

* :code:`--list-subst`   List all substitutions

* :code:`-o STRING`   FILE Save command stdout to file

* :code:`--string STRING`   FILE Output string after substitution


drom switch
~~~~~~~~~~~~~

Change current switch


**USAGE**
::
  
  drom switch ARGUMENT [OPTIONS]

Where options are:


* :code:`ARGUMENT`   New switch config

* :code:`--create`   Create switch as new

* :code:`--remove`   Remove switch

* :code:`--url STRING`   URL URL of new switch


drom test
~~~~~~~~~~~

For testing only


**USAGE**
::
  
  drom test ARGUMENTS [OPTIONS]

Where options are:


* :code:`ARGUMENTS`   args

* :code:`--test INT`   NUM Run test NUM


drom utils
~~~~~~~~~~~~

Some useful tools


**USAGE**
::
  
  drom utils [OPTIONS]

Where options are:


* :code:`--of-base64 STRING`   STR Translates from base64

* :code:`--of-boc STRING`   STR Parse boc in base64 format


drom watch
~~~~~~~~~~~~

Monitor a given account


**USAGE**
::
  
  drom watch [OPTIONS]

Where options are:


* :code:`--account STRING`   ACCOUNT Output account of account

* :code:`--from STRING`   ID Start with blockid ID

* :code:`--timeout INT`   TIMEOUT Timeout in seconds