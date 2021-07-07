Simple Use Cases
================

List and Confirm Transactions on a Multisig
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here is an example of interactions with a Multisig contract::

  # switch to mainnet network
  ft switch to mainnet
  
  # register the contract
  ft account create formet_subgov --contract SetcodeMultisigWallet2 \
      --address 0:1ec958fd022ab1d479dd722283fe5fd1d9de7196ee7f09f96b68e435776548c1
  
  # show available methods for this ABI
  ft contract abi SetcodeMultisigWallet2
  
  # start a transaction with payload
  ft multisig transfer 10 from formet_subgov --to other-account \
      this-method '{ "src": "%{account:address:formet_subgov}" }'
  
  # list awaiting transactions
  ft call formet_subgov getTransactions --local
  
  # confirm, signing with a specific account
  ft call formet_subgov  --sign me-as-custodian \
      confirmTransaction '{  "updateId": "0x6086aedc2b637541" }'

Experiment with HelloDebot
~~~~~~~~~~~~~~~~~~~~~~~~~~

If you want to experiment with the Helloworld DeBot from
https://github.com/tonlabs/debots/tree/main/helloworld , the following
list of commands will deploy and start the debot.

First, let's start by preparing a local network with TONOS SE::
  
  # Create a sandbox network, called sandbox1
  ft switch create sandbox1
  
  # Start the corresponding node
  ft node start
  # wait 5 seconds for the node to start
  sleep 5
  
  # user1 is the "deployer" for contracts, give it some tokens
  ft node give user1

Now, we can build and deploy the debot::
  
  # build the contract
  ft contract build helloDebot.sol -f
  
  #deploy the contract
  ft contract deploy helloDebot --create my-first-debot -f
  
  # set the ABI of the debot
  ft call my-first-debot setABI '{ "dabi": "%{hex:read:contract:abi:helloDebot}" }'
  
  # set the ICON of the debot (from local file hellodebot.png)
  ft call my-first-debot setIcon \
     '{ "icon": "%{hex:string:data:image/png;base64,%{base64:file:hellodebot.png}}" }'
  
  # execute the debot
  ft client -- debot fetch %{account:address:my-first-debot}


Watch an Account in real-time
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The :code:`ft watch` sub-command can be used to watch messages
received by an account. It can be very useful when debugging smart
contracts::

  $ ft watch my-contract [--from BLOCKID] [--timeout DELAY]

The account is either an account managed by the wallet, or an address.

If :code:`--from BLOCKID` is specified, the command will display
everything that happened since the given :code:`BLOCKID` (it might
take some time as the command crawls every block of the account's
shard).

By default, the command will exit if nothing happens during 15
minutes. The argument :code:`--timeout SECONDS` can be used to
increase or decrease this delay.

Inspect the past of an account
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is also possible to do it at the end of execution::

  $ ft inspect --past my-contract

This command will print all the transactions, you may limit the number using
:code:` --limit NUM`.
