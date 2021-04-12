Exploring the Blockchain
========================

The :code:`ft inspect` sub-command can be used to inspect data on the
blockchain.

Arguments :code:`-2` and :code:`-3` can be used to increase the level
of information retrieved and displayed. The environment variable
:code:`FT_DEBUG_GRAPHQL` can be set to display the query sent to the
server.


Account Information
~~~~~~~~~~~~~~~~~~~

First, we can check the state of an account or address::

  $ ft inspect -a fabrice
  ACCOUNT: {
    "id": "0:24a44423bc7edc2598b50ae87267bd06bc53455328e837dae32b9b7592716de8",
    "acc_type_name": "Active",
    "balance": "99834388127",
    "code_hash": "207dc560c5956de1a2c1479356f8f3ee70a59767db2bf4788b1d61ad42cdad82"
  }

We can set the variable :code:`FT_DEBUG_GRAPHQL` to understand what happens::

  $ FT_DEBUG_GRAPHQL=1 ft inspect -a fabrice
  Graphql query (input): accounts(filter: { id:
    { eq: "0:24a44423bc7edc2598b50ae87267bd06bc53455328e837dae32b9b7592716de7" } })
    { id acc_type_name balance(format: DEC) code_hash }
  Server replied: {
  "accounts": [
    {
      "id": "0:24a44423bc7edc2598b50ae87267bd06bc53455328e837dae32b9b7592716de7",
      "acc_type_name": "Active",
      "balance": "99834388127",
      "code_hash": "207dc560c5956de1a2c1479356f8f3ee70a59767db2bf4788b1d61ad42cdad82"
    }
  ]
  }
  
  ACCOUNT: {
    "id": "0:24a44423bc7edc2598b50ae87267bd06bc53455328e837dae32b9b7592716de7",
    "acc_type_name": "Active",
    "balance": "99834388127",
    "code_hash": "207dc560c5956de1a2c1479356f8f3ee70a59767db2bf4788b1d61ad42cdad82"
  }

We can increase the level of verbosity with options :code:`-2` or :code:`-3`::

  $ ft inspect --a fabrice -2
  ACCOUNT: {
    "id": "0:24a44423bc7edc2598b50ae87267bd06bc53455328e837dae32b9b7592716de7",
    "acc_type_name": "Active",
    "balance": "99834388127",
    "code_hash": "207dc560c5956de1a2c1479356f8f3ee70a59767db2bf4788b1d61ad42cdad82"
  }

  ACCOUNT: {
  "id": "0:24a44423bc7edc2598b50ae87267bd06bc53455328e837dae32b9b7592716de7",
  "acc_type_name": "Active",
  "balance": "99834388127",
  "boc": "te6ccgECTQEAE3[ ... ]DcIdcNH91TEd3BBCKCEP////28sdwB8AGA==",
  "code": "te6ccgECSgEAEqkAAib/[ ... ]NH91TEd3BBCKCEP////28sdwB8AGA==",
  "code_hash": "207dc560c5956de1a2c1479356f8f3ee70a59767db2bf4788b1d61ad42cdad82",
  "data": "te6ccgEBAgEAmAAB30rKNy7ZaVq0LMi6f9f1bRHCQBYRwtUTu8KL61x/Q2OhAAABeByuyDOlZRuXbLStWhZkXT/r+raI4SALCOFqid3hRfWuP6Gx0IAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAICAAAAAAEBgBAEWgCVlG5dstK1aFmRdP+v6tojhIAsI4WqJ3eFF9a4/obHQgEA==",
  "data_hash": "45d6503b666bb3ddb9204acb43470d6a5e59cadc9f358acf8570d2166687edd9",
  "last_paid": 1618036237,
  "workchain_id": 0
  }

It is possible to query all accounts by specifying :code:`-a all`, for
example if you are using a local network for sandboxing.

Head of the blockchain
~~~~~~~~~~~~~~~~~~~~~~

Let's inspect the head (most recent block) of the blockchain::

  $ ft inspect -h
  BLOCK: {
  "id": "a1a5c27d450556ff293a5f735e35b5f44a965f4a8396f88c95fdce690160e0ae",
  "seq_no": 11462705,
  "gen_utime": 1618086569,
  "shard": "b800000000000000",
  "status_name": "Finalized",
  "tr_count": 0,
  "workchain_id": 0
  }

Actually, this is the most recent block of one of the shards, we may specify
a specific shard using :code:`--shard SHARD` or :code:`--shard-block BLOCKID`,
or better, using :code:`--shard-account ACCOUNT`::

  $ ft inspect -h --shard-account 0:24a444
  Querying last shard blockid for address
  Querying shard of block "f1c99bcee306eb50714866d7a95676799f45b238ee803c119ff3d03febd148c6"
  shard = "2800000000000000"
  BLOCK: {
  "id": "f1c99bcee306eb50714866d7a95676799f45b238ee803c119ff3d03febd148c6",
  "seq_no": 11453515,
  "gen_utime": 1618086998,
  "shard": "2800000000000000",
  "status_name": "Finalized",
  "tr_count": 0,
  "workchain_id": 0
  }

Block Information
~~~~~~~~~~~~~~~~~

To display information on a specific block knowing its block identifier::

  $ ft inspect -b 4247b3b27205e336da1159474d0df8d0d00a552b1959671c81556a9bf953ae3f -2
  
  BLOCK: {
  "id": "4247b3b27205e336da1159474d0df8d0d00a552b1959671c81556a9bf953ae3f",
  "seq_no": 7518515,
  "gen_utime": 1616521410,
  "in_msg_descr": [
    {
      "msg_type_name": "Immediately",
      "transaction_id": "30407b122b1bd97bb77837343f645dbd1367035084f84ecaeb3e5bb7466b7ad2"
    }
  ],
  "out_msg_descr": [],
  "shard": "8000000000000000",
  "status_name": "Finalized",
  "tr_count": 5,
  "workchain_id": -1
  }

To display information on a block knowing its number (without
:code:`--shard`, blocks for all shards will be displayed) ::

  $ ft inspect --bn 7518515 --shard c800000000000000
  BLOCK: {
  "id": "09dc90c72c038ac787027af6e058cd011fd6b76f1b419904625477ed890a49d0",
  "seq_no": 7518515,
  "gen_utime": 1608055824,
  "shard": "c800000000000000",
  "status_name": "Finalized",
  "tr_count": 0,
  "workchain_id": 0
  }

Transaction Information
~~~~~~~~~~~~~~~~~~~~~~~

To display information on a specific transaction::

 $ ft inspect -t 30407b122b1bd97bb77837343f645dbd1367035084f84ecaeb3e5bb7466b7ad2
 
 TRANSACTION: {
  "id": "30407b122b1bd97bb77837343f645dbd1367035084f84ecaeb3e5bb7466b7ad2",
  "aborted": false,
  "account_addr": "-1:3333333333333333333333333333333333333333333333333333333333333333",
  "balance_delta": "3137500000",
  "block_id": "4247b3b27205e336da1159474d0df8d0d00a552b1959671c81556a9bf953ae3f",
  "in_msg": "9e4ae0da4710d0d67e201bce55890921904dd8bb81a85e246ca9a6a5ff488496",
  "out_msgs": [],
  "status": 3,
  "status_name": "Finalized",
  "total_fees": "0",
  "tr_type": 0,
  "tr_type_name": "Ordinary"
  }
  
  IN MESSAGE: {
  "id": "9e4ae0da4710d0d67e201bce55890921904dd8bb81a85e246ca9a6a5ff488496",
  "dst": "-1:3333333333333333333333333333333333333333333333333333333333333333",
  "msg_type": 0,
  "msg_type_name": "Internal",
  "src": "-1:0000000000000000000000000000000000000000000000000000000000000000",
  "status": 5,
  "status_name": "Finalized",
  "value": "3137500000"
  }

Message Information
~~~~~~~~~~~~~~~~~~~

Messages information can be displayed, but not yet interpreted (method
called, parameters) because it would require to know the ABI of the
receiver.

To display information on a specific message knowing its identifier::

  $ ft inspect -m 9e4ae0da4710d0d67e201bce55890921904dd8bb81a85e246ca9a6a5ff488496
  
  MESSAGE: {
  "id": "9e4ae0da4710d0d67e201bce55890921904dd8bb81a85e246ca9a6a5ff488496",
  "dst": "-1:3333333333333333333333333333333333333333333333333333333333333333",
  "msg_type": 0,
  "msg_type_name": "Internal",
  "src": "-1:0000000000000000000000000000000000000000000000000000000000000000",
  "status": 5,
  "status_name": "Finalized",
  "value": "3137500000"
  }

To display the last messages received by an account (use :code:`-2` to display more information, such as transaction identifiers)::

  $ ft inspect -m fabrice --limit 2

  MESSAGE:
  {
  "id": "37d6aa21f9b330e5ebd8a0ff9bfc75fa126d368146f0858e6549253161965ec1",
  "dst": "0:24a44423bc7edc2598b50ae87267bd06bc53455328e837dae32b9b7592716de7",
  "msg_type": 0,
  "msg_type_name": "Internal",
  "src": "0:68ad5013a8fda69c4828602c5f9a42261eb03bcb5a1f71c764b159f8b50f0a7a",
  "status": 5,
  "status_name": "Finalized",
  "value": "1"
  }
  MESSAGE:
  {
  "id": "f80202f96ffbfeaeb4f56ea7ba2f322ae5d1b5ca84526977d8d39a5be4636727",
  "dst": "0:24a44423bc7edc2598b50ae87267bd06bc53455328e837dae32b9b7592716de7",
  "msg_type": 0,
  "msg_type_name": "Internal",
  "src": "0:02169cad26a640ff509200bd4c950373964c75ca3e3b7cacfcad967d84951763",
  "status": 5,
  "status_name": "Finalized",
  "value": "1"
  }

To display the last messages sent by an account::

  $ ft inspect -m ^fabrice --limit 2                                    [23:47]
  
  MESSAGE:
  {
  "id": "2a5a828db238bc4ac757597b70e180d6f14e6740952eaa59a474d5d32610b3ea",
  "dst": "0:68ad5013a8fda69c4828602c5f9a42261eb03bcb5a1f71c764b159f8b50f0a7a",
  "msg_type": 0,
  "msg_type_name": "Internal",
  "src": "0:24a44423bc7edc2598b50ae87267bd06bc53455328e837dae32b9b7592716de7",
  "status": 5,
  "status_name": "Finalized",
  "value": "10900500000000"
  }
  MESSAGE:
  {
  "id": "7dde07cd436d80ab3b47146f3e36430a0b5de4764c4b1e1d57a9b01cb787a43c",
  "dst": "",
  "msg_type": 2,
  "msg_type_name": "ExtOut",
  "src": "0:24a44423bc7edc2598b50ae87267bd06bc53455328e837dae32b9b7592716de7",
  "status": 5,
  "status_name": "Finalized"
  }

  

  
