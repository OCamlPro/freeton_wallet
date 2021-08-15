Calling Commands with Substitutions
===================================

Displaying Wallet Information
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The sub-command :code:`ft output` can be used to display wallet
information, and save it into files.

For example, to display the address associated with a wallet, you
may use::

  $ ft output --addr user0
  0:108f6113fb0cad8c98b70e8ea3cfd12b52710ec20441d05ceb78cacb4f5566b7

You may save the output into a file::

  $ ft output --addr user0 -o user0.addr

You may output the keyfile associated with an account::

  $ ft output --keyfile user0 -o user0.keyfile
  $ cat user0.keyfile
  {
  "public": "fd9ee2babfa35b65917f732316dbb3d31935ccacd2a0aa92e043f8c762e0da28",
  "secret": "7883b5b2962f1a2d4891f52a16f125b855b6be61f84b162a4476a2f240c9e2c9"
  }

And finally, you may just load a file and display it after performing
some substitutions (see the last section here for a documentation of
these substitutions)::

  $ cat keyfile.in
  {
  "public-key": "%{account:pubkey:%{env:FTUSER}}",
  "private-key": "%{account:seckey:%{env:FTUSER}}"
  }
  $ FTUSER=user0 ft output --file keyfile.in
  {
  "public-key": "fd9ee2babfa35b65917f732316dbb3d31935ccacd2a0aa92e043f8c762e0da28",
  "private-key": "7883b5b2962f1a2d4891f52a16f125b855b6be61f84b162a4476a2f240c9e2c9"
  }

Calling Commands with substitutions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The sub-command :code:`ft client` can be used to call external
commands while performing substitutions on their arguments.

By default, :code:`ft client` will call :code:`tonos-cli`, passing its
arguments::

  $ ft client -- genphrase
  Calling /home/lefessan/.ft/bin/tonos-cli --config \
     /home/lefessan/.ft/sandbox1/tonos-cli.config genphrase
  Config: /home/lefessan/.ft/sandbox1/tonos-cli.config
  Succeeded.
  Seed phrase: "fancy current bean dice pet gasp you swift \
    balance unaware law fun"

Note that we must use :code:`--` before providing the arguments to the
external commands, to prevent :code:`ft` from interpreting them.

We can also call another command, using :code:`--exec` to prevent
:code:`ft` from calling :code:`tonos-cli`::

  $ ft client --exec -- echo '%{account:address:user0}'
  Calling echo 0:108f6113fb0cad8c98b70e8ea3cfd12b52710ec20441d05ceb78cacb4f5566b7
  0:108f6113fb0cad8c98b70e8ea3cfd12b52710ec20441d05ceb78cacb4f5566b7

This feature can be use to script many commands, for example::

  $ ft client -- genaddr  '%{contract:tvc:SetcodeMultisigWallet2}' \
    '%{contract:abi:SetcodeMultisigWallet2}' --genkey toto.surf

This command will generate a new key-pair, saved it into file
:code:`toto.surf`, which providing the command with the TVC and ABI
files for the contract :code:`SetcodeMultisigWallet2` (the one used by
the Surf web wallet).
  
Existing Substitutions
~~~~~~~~~~~~~~~~~~~~~~

Substitutions are written as :code:`%{SUBST}`, and can be recursive
(substitutions are allowed within SUBST itself). If the substitution
is unknown (bad syntax) or cannot be resolved, an error is thrown.

Here is a list of allowed expressions within SUBST:

* :code:`env:VARIABLE` :
  Replaced by environemnt (shell) variable :code:`VARIABLE`
* :code:`addr:zero` :
  For 0:0000000000000000000000000000000000000000000000000000000000000000

On wallet accounts, for a known account :code:`ACCOUNT`:

* :code:`account:address:ACCOUNT` :
  replaced by the raw address of the account
* :code:`account:wc:ACCOUNT` :
  replaced by the workchain ID of the account
* :code:`account:pubkey:ACCOUNT` :
  replaced by the pubkey of the account, without :code:`0x`
* :code:`account:seckey:ACCOUNT` :
  replaced by the secret of the account
* :code:`account:passphrase:ACCOUNT` :
  replaced by the passphrase of the account
* :code:`account:keyfile:ACCOUNT` :
  Generates a JSON keyfile in :code:`$HOME/.ft/tmp/`, and
  replaced by the file name
* :code:`account:contract:ACCOUNT` :
  replaced by the name of recorded contract of the account if known
* :code:`account:contract:tvc:ACCOUNT` :
  replaced by the TVC file name for the contract of the account
* :code:`account:contract:abi:ACCOUNT` :
  replaced by the ABI file name of the contract of the account
* :code:`account:payload:ACCOUNT:METH:PARAMS` :
  replaced by the base64 encoding of the message payload, corresponding to
  calling :code:`METH` with JSON arguments :code:`PARAMS`

On contracts:

* :code:`contract:tvc:CONTRACT` :
* :code:`contract:abi:CONTRACT` :
* :code:`contract:payload:CONTRACT:METH:PARAMS` :
  Output payload base64

Misc:

* :code:`node:url` :
  Current node URL
* :code:`ton:NUMBER` :
  Convert NUMBER of tons to nanotons
* :code:`file:FILENAME` :
  Read content of filename
* :code:`now` :
  current Unix date
* :code:`now[:CORRECTION]*` :
  Current Unix date with corrections, given by:

  * :code:`plus:NUM:PERDIODS` : add a delay
  * :code:`minus:NUM:PERIODS` : subtract a delay

  where :code:`PERIODS` is one of :code:`years`, :code:`days`, :code:`hours`,
  :code:`mins` or :code:`secs`.

  Example: :code:`now:plus:3:days:plus:4:hours`
        
Encoders, working on the rest of the substitution:

* :code:`string:SUBST`
  Take remaining SUBST without substituting, just as a string
* :code:`read:SUBST` :
  Do SUBST, then read it as a filename
* :code:`hex:SUBST` :
  Do SUBST, then convert to hexa
* :code:`of-hex:SUBST` :
  Do SUBST, then convert from hexa
* :code:`base64:SUBST` :
  Do SUBST, then convert to base64
* :code:`of-base64:SUBST` :
  Do SUBST, then convert from base64
* :code:`get-code:SUBST` :
  Do SUBST, interprete it as a TVC filename, whose code should be extracted
  using :code:`tvm_linker decode` and passed in base64
