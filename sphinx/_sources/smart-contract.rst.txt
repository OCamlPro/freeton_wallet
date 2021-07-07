Deploying a Solidity Contract
=============================

Let's suppose that we have a simple Free TON Solidity smart contract::

  pragma ton-solidity >= 0.37.0;
  pragma AbiHeader expire;
  
  contract HelloWorld {
  
    string variable ;
    constructor(string arg) public { tvm.accept() ; variable = arg ; }
    
    function get_var() public view returns (string s)
    { tvm.accept(); s = variable ; }
  }

First, let's try to compile it::

  ─➤ ft contract build Basic.sol
  
  Calling /home/user/.ft/bin/solc Basic.sol
  Code was generated and saved to file Basic.code
  ABI was generated and saved to file Basic.abi.json
  Calling /home/user/.ft/bin/tvm_linker compile -o Basic.tvm Basic.code --abi-json Basic.abi.json --lib /home/user/.ft/bin/stdlib_sol.tvm
  TVM linker 0.1.0
  COMMIT_ID: 85973140d89b0da5c211b562be86fba5783815d1
  BUILD_DATE: 2021-03-04 16:02:46 +0100
  COMMIT_DATE: 2021-02-17 19:22:22 +0300
  GIT_BRANCH: master
  Calling cp -f Basic.sol Basic.abi.json /home/user/.ft/contracts
  Calling cp -f Basic.tvm /home/user/.ft/contracts/Basic.tvc
  Config loaded from /home/user/.ft/config.json
  Network: sandbox1
  Loading wallet file /home/user/.ft/sandbox1/wallet.json

We can see that :code:`ft` called :code:`solc` and then
:code:`tvm_linker` to compile and link the contract. The contract was
then copied in the contract database of :code:`ft` under the name
:code:`Basic` (as extracted from the filename).

We will now try to run it locally. For that, let's create a sandbox
network on the current computer::

  ╰─➤ ft switch create sandbox1
  
  Config loaded from /home/user/.ft/config.json
  Network: mainnet
  Loading wallet file /home/user/.ft/mainnet/wallet.json
  Calling docker create --name local-node-7081 -e USER_AGREEMENT=yes -p7081:80 tonlabs/local-node
  274bcd1a5ab7190ad7cb91052b2870dce4bc301affda5eecf91a85791eb6ecde
  Saving wallet file /home/user/.ft/mainnet/wallet.json
  Saving wallet file /home/user/.ft/sandbox1/wallet.json
  Saving config file /home/user/.ft/config.json

It is possible to create as many sandboxes as wanted, the only
constraint is for the name to start with :code:`sandbox`.

Now, let's start the node::
  
  ─➤ ft node start
  
  Config loaded from /home/user/.ft/config.json
  Network: sandbox1
  Loading wallet file /home/user/.ft/sandbox1/wallet.json
  Calling docker start local-node-7081
  local-node-7081

It is possible to check that the node is running using::

  ─➤ ft node web
  
  Config loaded from /home/user/.ft/config.json
  Network: sandbox1
  Loading wallet file /home/user/.ft/sandbox1/wallet.json
  Calling xdg-open http://0.0.0.0:7081/graphql

That command will open the browser on the GraphQL Playground page of
the server.

Now, we also have a set of preconfigured users, but we will need some
coins to be able to deploy the smart contract::

  ─➤ ft account info
  
  Config loaded from /home/user/.ft/config.json
  Network: sandbox1
  Loading wallet file /home/user/.ft/sandbox1/wallet.json
  Account "giver": 4_999_999_999.975_350 TONs
  Account "user9": not yet created
  Account "user8": not yet created
  Account "user7": not yet created
  Account "user6": not yet created
  Account "user5": not yet created
  Account "user4": not yet created
  Account "user3": not yet created
  Account "user2": not yet created
  Account "user1": not yet created
  Account "user0": not yet created

So, let's ask the giver for some coins::

  ╰─➤ ft node give user1
  
  Config loaded from /home/user/.ft/config.json
  Network: sandbox1
  Loading wallet file /home/user/.ft/sandbox1/wallet.json
  For key user1
  Generating external inbound message...
  
  MessageId: 71fa31ea4a0c986a1bbbed1e5a0e06b3ab61c2142ac2e5e273d1e386b4ece810
  Expire at: unknown
  Succeeded.
  call returned {}
  node url: http://0.0.0.0:7081
  Contract deployed at 0:f89872394a383dc289f27ded48f02a6269e19d02be5821ba8081c67a1070588a
  Saving wallet file /home/user/.ft/sandbox1/wallet.json
  Saving config file /home/user/.ft/config.json

We can now check the account of :code:`user1`::

  ╰─➤ ft account info user1
  
  Config loaded from /home/user/.ft/config.json
  Network: sandbox1
  Loading wallet file /home/user/.ft/sandbox1/wallet.json
  Account "user1": 1_000.944_105_999 TONs

(note that we could have used :code:`ft account user1 -v` to get a
complete view of the account, with all other fields)

Now, let's deploy the contract :code:`Basic`. For that, we will create
an account :code:`basic` and call the constructor with the argument
:code:`"Hello World"`::

  ─➤ ft contract deploy Basic --create basic --params '{ "arg": "%{hex:string:Hello World}" }'
  
  Config loaded from /home/user/.ft/config.json
  Network: sandbox1
  Loading wallet file /home/user/.ft/sandbox1/wallet.json

         <--------- key generation for basic ---------->
  
  { "public": "e017bc6792ee1c6cc6db8b4ee4edb00f2c958814a0f8aa46a7973b9493101c16",
        "secret": "2e25b0c601725e447693b5a3a20c8b7b06da4d0ebb94bb8a4453d33b85a9453e" }
  Key for user "basic" generated
  Account modified.
  { "name": "basic",
        "passphrase":
    "bus turtle safe isolate surface fee later cream banner buffalo soul mom",
        "pair":
    { "public":
        "e017bc6792ee1c6cc6db8b4ee4edb00f2c958814a0f8aa46a7973b9493101c16",
      "secret":
        "2e25b0c601725e447693b5a3a20c8b7b06da4d0ebb94bb8a4453d33b85a9453e" },
  "account":
    { "address":
        "0:8a33da5fb4e623f796a33efeae19374cf8646ad050ebf3aac0c5d6766d76f969",
      "contract": "Basic" } }
  
         <--------- token transfer to credit account ---------->
  
  Generating external inbound message...
  
  MessageId: 46fb33141f118e7d107f34875a9dc7341c05a4bab58c74248da4d302c29aa2ed
  Expire at: unknown
  Succeeded.
  call returned {
        "transId": "0"
        }
  
         <--------- contract deployment ---------->
  
  node url: http://0.0.0.0:7081
  Contract deployed at 0:8a33da5fb4e623f796a33efeae19374cf8646ad050ebf3aac0c5d6766d76f969
  Saving wallet file /home/user/.ft/sandbox1/wallet.json
  Saving config file /home/user/.ft/config.json

Here, several things happened:

1. We used the :code:`--create basic` option to create a new address for the contract with a key pair, associated with the account name :code:`basic` (you can use :code:`--replace basic` to recreate an address, while keeping the same account name, for example if you changed the contract code and rebuilt it before redeploying). When the :code:`--create/replace` options are used, 1 TON is sent automatically from the deployer account (:code:`user1` by default), unless the :code:`--sign SOURCE` option is specified.

2. We used an automatic substitution in the argument. Indeed, strings values must be passed as hexa, so we used the notation :code:`%{hex:string:STRING}` to convert the :code:`STRING` to hexa. Several substitutions are available, such as :code:`{base64:file:FILENAME}` to translate the content of a file to base64. See the substitutions section for more.

3. The contract was deployed and the constructor called with the argument.

We can now call the :code:`get_var` method::

  ╰─➤ ft call basic get_var '{}'
  
  Config loaded from /home/user/.ft/config.json
  Network: sandbox1
  Generating external inbound message...
  
  MessageId: 276b0b4c0b479932f78fa34d4b2e66e5b2f2d91530240190b0409fec940afc31
  Expire at: unknown
  Succeeded.
  call returned {
    "s": "48656c6c6f20576f726c64"
  }

Note that it is possible to use the :code:`--run` argument to run the
method locally::

  ─➤ ft call basic get_var --run
  
  Config loaded from /home/user/.ft/config.json
  Network: sandbox1
  Generating external inbound message...
  
  MessageId: a69e5049d7c4d44e3773bc245f988e7f929c57aa2a27a6542bcacd156fa7c36e
  Expire at: unknown
  Running get-method...
  Succeeded.
  call returned {
    "s": "48656c6c6f20576f726c64"
  }
