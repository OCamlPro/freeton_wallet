
Managing Multiple Accounts
==========================

The main job of :code:`ft` is to manage accounts: it can create new
key-pairs, associate them with names, compute addresses with key-pairs
and query information for such accounts.

Creating Accounts
~~~~~~~~~~~~~~~~~

To create a new account, you may use::

  $ ft account --create user10
  { "public": "fcb4663820d060d605d8642f590ea68b69e2bc039168a5887325e749385afe05",
  "secret": "b6056a37deac87aa4889ed092d593762fed7641adff1621281337ae2fa8da26b" }
  Key for user "user10" generated

To be able to use this account, you may want to associate a contract
with that account::

  $ ft account user10 --surf
  {
  "name": "user10",
  "passphrase": "pumpkin juice main spray accident wisdom scene organ lesson suit pen swear",
  "pair": {
    "public": "fcb4663820d060d605d8642f590ea68b69e2bc039168a5887325e749385afe05",
    "secret": "b6056a37deac87aa4889ed092d593762fed7641adff1621281337ae2fa8da26b"
  },
  "account": {
    "address": "0:28b049f29b97c5a473f68198f413d0e516509c60a842020ed5e92185eeae0ada",
    "contract": "SetcodeMultisigWallet2"
  }
  }

Here, we used the option :code:`--surf` to associate the same contract
that on the Surf wallet. It is equivalent to using :code:`--contract
SetcodeMultisigWallet2`. It works because :code:`ft` knowns about a
predefined set of contracts, among which the one used by Surf (you can
use :code:`ft list` to check the full list).
  
It is possible to query the balance for such an account::

  $  ft account user10
  Account "user10": 999.944_105_999 TONs

You can use the argument :code:`-v` to display more information.

However, FreeTON accounts do not exist until a smart contract has been
deployed on their address. Such a deployment is only possible if the
address balance has been credited with a minimal amount, that mainly
depends on the size of the smart contract. We will explain how to
perform these operations in the following Multisig section.

Note that you may create an account by providing only its address and
contract::

  $ ft account --create user13 \
    --address 0:28b049f29b97c5a473f68198f413d0e516509c60a842020ed5e92185eeae0ada \
    --contract SetcodeMultisigWallet2
  Account created.
  {
    "name": "user13",
    "account": {
      "address": "0:28b049f29b97c5a473f68198f413d0e516509c60a842020ed5e92185eeae0ada",
      "contract": "SetcodeMultisigWallet2"
    }
  }

Note that, since :code:`ft` does not know the key-pair for this
account, it won't be able to sign operations originating from this
account.

Multisig Accounts
~~~~~~~~~~~~~~~~~

Most accounts on FreeTON are Multisig accounts, i.e. accounts holding
a multi-signature contract. Most of them are actually allowing only
one signature, but still, they share the same contract code.

The most used Multisig contracts are:

* :code:`SafeMultisigWallet` : a multisig wallet, formally verified
* :code:`SetcodeMultisigWallet2` : an extension of the multisig
  wallet, allowing to change the set of owners. This is the contract
  used by the :code:`TON Surf` wallet

To manage Multisig accounts, you should use the :code:`ft multisig`
sub-command.

**CREATE MULTISIG CONTRACTS**

To create a Multisig account, the account address must already have
some tokens on it to pay for the smart contract deployment.

To create a single-owner multisig
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





