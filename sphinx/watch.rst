Watching an Account
===================

The :code:`ft watch` sub-command can be used to watch messages
received by an account. It can be very useful when debugging smart
contracts::

  $ ft watch --account my-contract [--from BLOCKID] [--timeout DELAY]

The account is either an account managed by the wallet, or an address.

If :code:`--from BLOCKID` is specified, the command will display
everything that happened since the given :code:`BLOCKID` (it might
take some time as the command crawls every block of the account's
shard).

By default, the command will exit if nothing happens during 15
minutes. The argument :code:`--timeout SECONDS` can be used to
increase or decrease this delay.

It is also possible to do it at the end of execution::

  $ ft inspect --past my-contract

This command will print all the transactions, you may limit the number using
:code:` --limit NUM`.
