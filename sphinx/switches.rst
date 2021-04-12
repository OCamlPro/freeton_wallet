
Managing Multiple Networks
==========================

Switching between Networks
~~~~~~~~~~~~~~~~~~~~~~~~~~

All accounts managed by :code:`ft` are associated with a given
network, also called switches::

The sub-command to switch between networks is :code:`ft switch`::

  $ ft switch
  Loading wallet file /home/lefessan/.ft/mainnet/wallet.json
  * testnet
    - tonlabs (current if network was selected)
      url: https://net.ton.dev
  * mainnet (current)
    - tonlabs (current)
      url: https://main.ton.dev
  * rustnet
    - tonlabs (current if network was selected)
      url: https://rustnet.ton.dev
  * fldnet
    - tonlabs (current if network was selected)
      url: https://fld.ton.dev
  * sandbox1
    - node (current if network was selected)
      url: http://0.0.0.0:7081

Here, the output tells us that :code:`ft` knows about 5 networks, 4 of
whom are public ones from the default configuration (:code:`testnet`,
:code:`mainnet`, :code:`rustnet` and :code:`fldnet`) and one is a
local one called :code:`sandbox1`.

The output also tells us that :code:`mainnet` is currently selected
(so that all commands apply to this network by default), and that it
uses the (predefined) node called :code:`tonlabs` at URL
:code:`https://main.ton.dev`.

It is then possible to switch to a different network with::

  $ ft switch testnet
  Switched to network "testnet"

Now, all commands would apply to the :code:`testnet` network by default.

It is possible to specify the network to which a particular command
applies using the early :code:`--switch` argument::

  $ ft --switch NETWORK SUBCOMMAND [ARGUMENTS]

or the :code:`FT_SWITCH` environment/shell variable::

  $ FT_SWITCH=testnet ft SUBCOMMAND [ARGUMENTS]

Creating and Removing Public Networks
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to create a new network using::

  $ ft switch --create fabnet --url https://fab.ton.dev 

This will create a new public network called :code:`fabnet`, with a
node called :code:`node` with the URL :code:`https://fab.ton.dev`.
This network is immediately promoted as the default network.

It is possible to remove an existing network using::

  $ ft switch --remove fabnet

However, this command will fail if the network is the current one, so
you must switch to another network before using it.

Beware that removing a network removes all the keys associated with
it.

Creating Sandbox Networks
~~~~~~~~~~~~~~~~~~~~~~~~~

When creating new networks, :code:`ft` behaves differently when
networks start with :code:`sandbox`. The name should be
:code:`sandboxNUM` where :code:`NUM` is a small number.

In this case, :code:`ft` will create a local docker network using
:code:`tonlabs/local-node` image, on local port :code:`7800+NUM`.

It will also create a remote account :code:`giver` corresponding to
the contracts holding the tokens on the local network, and 10 local
Surf accounts, called :code:`user0` to :code:`user9`. All these
accounts always have the same public and secret keys and raw addresses
on all sandbox networks, to ease debugging when restarting the
networks.

Creating the network will not start the node. Instead, you must call::

  $ ft node --start

to start the network.

To stop it, you must use::

  $ ft node --stop

If you want to query its Graphql playground interface, you may use::

  $ ft node --web

Finally, you may want to give tokens to local accounts (such as
:code:`user0`, etc.).

For that, you may use::

  $ ft node --give all

By default, this command will give 1000 TON to every local account
with a balance smaller than this amount, and try to deploy a Surf
smart contract if the account is not initialized.

You may also specify a given account, and a minimal balance in TON::

  $ ft node --giver user0:100000


  


