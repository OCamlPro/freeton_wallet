(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2
open EZCMD.TYPES

let action_phase_errors = [
  32, "Action list invalid" ;
  33, "Too many actions" ;
  34, "Unsupported action" ;
  35, "Invalid source address" ;
  36, "Invalid destination address" ;
  37, "Too low balance to send outbound message (37) at action" ;
  38, "Too low extra to send outbound message (38) at action" ;
  39, "Message does not fit in buffer" ;
  40, "Message too large" ;
  41, "Library not found" ;
  42, "Library delete error" ;
]

let errors = [

  "TVM Errors", [
    "TVM terminated successfully", 0 ;
    "TVM terminated successfully:  alternative code", -2 ;
    "Stack underflow", -3 ;
    "Stack overflow", -4 ;
    "Integer overflow", -5 ;
    "Range check error", -6 ;
    "Invalid opcode", -7 ;
    "Type check error", -8 ;
    "Cell overflow", -9 ;
    "Cell underflow", -10 ;
    "Dictionary error", -11 ;
    "Unknown error", -12 ;
    "Fatal error", -13 ;
    "Out of gas: the contract is either low on gas, or its limit is exceeded",
    -14 ;
  ] ;

  "Action Phase Errors", List.map (fun (x,y) -> (y,x)) action_phase_errors ;

  "Solidity Runtime Errors", [

    "External inbound message has an invalid signature. See tvm.pubkey() and msg.pubkey().", 40 ;
    "Array index or index of <mapping>.at() is out of range.", 50 ;
    "Calling of contract's constructor that has already been called.", 51 ;
    "Replay protection exception. See timestamp in pragma AbiHeader.", 52 ;
    "See <address>.unpack().", 53 ;
    "Calling <array>.pop for empty array.", 54 ;
    "See tvm.insertPubkey().", 55 ;
    "External inbound message is expired. See expire in pragma AbiHeader.", 57 ;
    "External inbound message has no signature but has public key. See pubkey in pragma AbiHeader.", 58 ;
    "Inbound message has wrong function id. In contract there are no functions with such function id and there is no fallback function which could handle the message. See fallback.", 60 ;
    "Deploying StateInit has no public key in data field.", 61 ;
    "Reserved for internal usage.", 62 ;
    "See <optional(Type)>.get().", 63 ;
    "tvm.buildExtMSg() call with wrong parameters. See tvm.buildExtMsg().", 64 ;
    "Calling of unassigned variable of function type. See Function type.", 65 ;
    "Converting an integer to a string with width less than number length. See format().", 66 ;
    "See gasToValue and valueToGas.", 67 ;
    "There is no config parameter 20 or 21.", 68 ;
    "Calculating zero to the power of zero (0**0 in solidity style or 0^0).", 69 ;
    "string method substr was called with substr longer than the whole string.", 70 ;
    "Function marked by externalMsg is called by internal message.", 71 ;
    "Function marked by internalMsg is called by external message.", 72 ;
    "The value can't be converted to enum type.", 73 ;
  ];

  "Client SDK Errors", [
    "NotImplemented", 1 ;
    "InvalidHex", 2 ;
    "InvalidBase64", 3 ;
    "InvalidAddress", 4 ;
    "CallbackParamsCantBeConvertedToJson", 5 ;
    "WebsocketConnectError", 6 ;
    "WebsocketReceiveError", 7 ;
    "WebsocketSendError", 8 ;
    "HttpClientCreateError", 9 ;
    "HttpRequestCreateError", 10 ;
    "HttpRequestSendError", 11 ;
    "HttpRequestParseError", 12 ;
    "CallbackNotRegistered", 13 ;
    "NetModuleNotInit", 14 ;
    "InvalidConfig", 15 ;
    "CannotCreateRuntime", 16 ;
    "InvalidContextHandle", 17 ;
    "CannotSerializeResult", 18 ;
    "CannotSerializeError", 19 ;
    "CannotConvertJsValueToJson", 20 ;
    "CannotReceiveSpawnedResult", 21 ;
    "SetTimerError", 22 ;
    "InvalidParams", 23 ;
    "ContractsAddressConversionFailed", 24 ;
    "UnknownFunction", 25 ;
    "AppRequestError", 26 ;
    "NoSuchRequest", 27 ;
    "CanNotSendRequestResult", 28 ;
    "CanNotReceiveRequestResult", 29 ;
    "CanNotParseRequestResult", 30 ;
    "UnexpectedCallbackResponse", 31 ;
    "CanNotParseNumber", 32 ;
    "InternalError", 33 ;
    "InvalidHandle", 34
  ] ;

  "Crypto SDK Errors", [

    "InvalidPublicKey", 100 ;
    "InvalidSecretKey", 101 ;
    "InvalidKey", 102 ;
    "InvalidFactorizeChallenge", 106 ;
    "InvalidBigInt", 107 ;
    "ScryptFailed", 108 ;
    "InvalidKeySize", 109 ;
    "NaclSecretBoxFailed", 110 ;
    "NaclBoxFailed", 111 ;
    "NaclSignFailed", 112 ;
    "Bip39InvalidEntropy", 113 ;
    "Bip39InvalidPhrase", 114 ;
    "Bip32InvalidKey", 115 ;
    "Bip32InvalidDerivePath", 116 ;
    "Bip39InvalidDictionary", 117 ;
    "Bip39InvalidWordCount", 118 ;
    "MnemonicGenerationFailed", 119 ;
    "MnemonicFromEntropyFailed", 120 ;
    "SigningBoxNotRegistered", 121 ;
    "InvalidSignature", 122 ;
    "EncryptionBoxNotRegistered", 123 ;
  ] ;

  "Boc SDK Errors", [

    "InvalidBoc", 201 ;
    "SerializationError", 202 ;
    "InappropriateBlock", 203 ;
    "MissingSourceBoc", 204 ;
    "InsufficientCacheSize", 205 ;
    "BocRefNotFound", 206 ;
    "InvalidBocRef", 207

  ];

  "Abi SDK Errors", [

    "RequiredAddressMissingForEncodeMessage", 301 ;
    "RequiredCallSetMissingForEncodeMessage", 302 ;
    "InvalidJson", 303 ;
    "InvalidMessage", 304 ;
    "EncodeDeployMessageFailed", 305 ;
    "EncodeRunMessageFailed", 306 ;
    "AttachSignatureFailed", 307 ;
    "InvalidTvcImage", 308 ;
    "RequiredPublicKeyMissingForFunctionHeader", 309 ;
    "InvalidSigner", 310 ;
    "InvalidAbi", 311 ;
    "InvalidFunctionId", 312 ;

  ] ;

  "Tvm SDK Errors", [
    "CanNotReadTransaction", 401 ;
    "CanNotReadBlockchainConfig", 402 ;
    "TransactionAborted", 403 ;
    "InternalError", 404 ;
    "ActionPhaseFailed", 405 ;
    "AccountCodeMissing", 406 ;
    "LowBalance", 407 ;
    "AccountFrozenOrDeleted", 408 ;
    "AccountMissing", 409 ;
    "UnknownExecutionError", 410 ;
    "InvalidInputStack", 411 ;
    "InvalidAccountBoc", 412 ;
    "InvalidMessageType", 413 ;
    "ContractExecutionError", 414

  ];


  "Processing SDK Errors", [
    "MessageAlreadyExpired", 501 ;
    "MessageHasNotDestinationAddress", 502 ;
    "CanNotBuildMessageCell", 503 ;
    "FetchBlockFailed", 504 ;
    "SendMessageFailed", 505 ;
    "InvalidMessageBoc", 506 ;
    "MessageExpired", 507 ;
    "TransactionWaitTimeout", 508 ;
    "InvalidBlockReceived", 509 ;
    "CanNotCheckBlockShard", 510 ;
    "BlockNotFound", 511 ;
    "InvalidData", 512 ;
    "ExternalSignerMustNotBeUsed", 513 ;
  ] ;

  "Net SDK Errors", [

    "QueryFailed", 601 ;
    "SubscribeFailed", 602 ;
    "WaitForFailed", 603 ;
    "GetSubscriptionResultFailed", 604 ;
    "InvalidServerResponse", 605 ;
    "ClockOutOfSync", 606 ;
    "WaitForTimeout", 607 ;
    "GraphqlError", 608 ;
    "NetworkModuleSuspended", 609 ;
    "WebsocketDisconnected", 610 ;
    "NotSupported", 611 ;
    "NoEndpointsProvided", 612 ;
    "GraphqlWebsocketInitError", 613 ;
    "NetworkModuleResumed", 614

  ];

  "Debot SDK Errors", [

    "DebotStartFailed", 801 ;
    "DebotFetchFailed", 802 ;
    "DebotExecutionFailed", 803 ;
    "DebotInvalidHandle", 804 ;
    "DebotInvalidJsonParams", 805 ;
    "DebotInvalidFunctionId", 806 ;
    "DebotInvalidAbi", 807 ;
    "DebotGetMethodFailed", 808 ;
    "DebotInvalidMsg", 809 ;
    "DebotExternalCallFailed", 810 ;
    "DebotBrowserCallbackFailed", 811 ;
    "DebotOperationRejected", 812
  ]
]

let action list =
  match list with
  | [] ->
      List.iter (fun (section, errors) ->
          Printf.printf "Section %s:\n%!" section ;
          List.iter (fun (msg, code) ->
              Printf.printf "   * %d : %s\n%!" code msg
            ) errors
        ) errors ;
  | _ ->
      let t = Hashtbl.create 1000 in
      List.iter (fun (section, errors) ->
          List.iter (fun (msg, code) ->
              Hashtbl.add t code (section, msg)
            ) errors
        ) errors ;
      List.iter (fun error ->
          let error = int_of_string error in
          match Hashtbl.find_all t error with
          | exception Not_found ->
              Printf.printf "Error %d: unknown error\n%!" error
          | list ->
              List.iter (fun ( section, msg ) ->
                  Printf.printf "Error %d: %s -> %s\n%!" error section msg
                ) list
        ) list

let cmd =
  let list = ref [] in
  EZCMD.sub
    "print error"
    (fun () -> action !list )
    ~args:
      [
        [], Arg.Anons (fun l -> list := l),
        EZCMD.info ~docv:"ERROR CODe" "Error code to explain" ;
      ]
    ~doc: "Display error codes"
    ~man:[]
