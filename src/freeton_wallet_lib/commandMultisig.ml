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

open EzCompat
open Ezcmd.V2
open EZCMD.TYPES

open Types

(*
─➤ ft multisig --create validator validator validator2
Calling /home/lefessan/.ft/testnet/bin/tonos-cli --config /home/lefessan/.ft/testnet/tonos-cli.config deploy /home/lefessan/.ft/contracts/SafeMultisigWallet.tvc {"owners":[ "0x422c6c4f9ab510a8e8622c09c31babffe91af6e496cffd144d1e041d8b6c34ff", "0xf5bfbf398959566b6b538c151e1644ffb188dbdec8bd0acdc136c74422b18400" ],"reqConfirms":1} --abi /home/lefessan/.ft/contracts/SafeMultisigWallet.abi.json --sign /home/lefessan/.ft/tmpfile8dc1f8.tmp --wc 0
output:
 Config: /home/lefessan/.ft/testnet/tonos-cli.config
Input arguments:
     tvc: /home/lefessan/.ft/contracts/SafeMultisigWallet.tvc
  params: {"owners":[ "0x422c6c4f9ab510a8e8622c09c31babffe91af6e496cffd144d1e041d8b6c34ff", "0xf5bfbf398959566b6b538c151e1644ffb188dbdec8bd0acdc136c74422b18400" ],"reqConfirms":1}
     abi: /home/lefessan/.ft/contracts/SafeMultisigWallet.abi.json
    keys: /home/lefessan/.ft/tmpfile8dc1f8.tmp
      wc: 0
Connecting to https://net.ton.dev
Deploying...
Transaction succeeded.
Contract deployed at address: 0:2e87845a4b04137d59931198006e3dd4ef49a63b62299aea5425dcf222afa02cw
*)


let is_multisig_contract = function
  | "SafeMultisigWallet"
  | "SetcodeMultisigWallet"
  | "SetcodeMultisigWallet2"
    -> true
  | _ -> false

let check_key_contract key =
  match key.key_account with
  | Some { acc_contract = Some acc_contract ; _ } ->
      if is_multisig_contract acc_contract then
        acc_contract
      else
        Error.raise "Account's contract %S is not multisig" acc_contract;

      (* Account's contract is not set, let's use the minimal common ABI *)
  | _ ->
      "SafeMultisigWallet"

let get_custodians account =
  let config = Config.config () in
  let net = Config.current_network config in
  let key = Misc.find_key_exn net account in
  let contract = check_key_contract key in
  let address = Misc.get_key_address_exn key in

  Utils.call_contract config
    ~contract
    ~address
    ~meth:"getCustodians"
    ~params:"{}"
    ~local:true
    ()

let get_waiting account =
 let config = Config.config () in
  let net = Config.current_network config in
  let key = Misc.find_key_exn net account in
  let contract = check_key_contract key in
  let address = Misc.get_key_address_exn key in
  Utils.call_contract config
    ~contract
    ~address
    ~meth:"getTransactions"
    ~params:"{}"
    ~local:true
    ()

let create_multisig
    ?client
    ?(accounts=[])
    ?(not_owner=false)
    ?(req=1)
    ?wc
    ?contract
    account
  =
  let config = Config.config () in
  let net = Config.current_network config in
  let owners = StringSet.of_list accounts in
  let owners =
    if not_owner then owners else
      StringSet.add account owners in

  let owners = StringSet.to_list owners in

  let owners = List.map (fun name ->
      match Misc.find_key net name with
      | None ->
          Error.raise "Key %S does not exist" name
      | Some key ->
          match key.key_pair with
          | None -> Error.raise "Key %S has no key pair" name
          | Some pair ->
              match pair.secret with
              | None ->
                  (* We should add an option to allow this *)
                  Error.raise "Key %S has no secret" name
              | Some _ -> pair.public
    ) owners in

  let nowners = List.length owners in
  if req < 1 || req > nowners then
    Error.raise "Invalid --req %d, should be 0 < REQ <= %d (nbr owners)"
      req nowners;

  let params =
    Printf.sprintf "{\"owners\":[ \"0x%s\" ],\"reqConfirms\":%d}"
      ( String.concat "\", \"0x" owners )
      req
  in
  let key = Misc.find_key_exn net account in

  let contract =
    match key.key_account with
    | None ->
        begin
          match contract with
          | None -> "SafeMultisigWallet"
          | Some contract -> contract
        end
    | Some acc ->
        begin
          match wc with
          | None -> ()
          | Some _ ->
              if Misc.string_of_workchain wc <>
                 Misc.string_of_workchain  acc.acc_workchain then
                Error.raise {|Account address uses a different workchain. Clear it with  'ft account %s --contract ""|} key.key_name
        end ;
        match acc.acc_contract with
        | None ->
            begin
              match contract with
              | None -> "SafeMultisigWallet"
              | Some contract -> contract
            end
        | Some acc_contract ->
            match contract with
            | None -> acc_contract
            | Some contract ->
                if contract <> acc_contract then
                  Error.raise {|Account %s uses a different contract %S. Clear it with 'ft account %s --contract ""|} key.key_name acc_contract key.key_name ;
                contract
  in
  if not (is_multisig_contract contract) then
    Error.raise {|Contract %s is not a multisig contract|} contract;

  let wc = match wc with
    | Some _ -> wc
    | None ->
        match key.key_account with
        | None -> None
        | Some acc ->
            acc.acc_workchain
  in

  Utils.deploy_contract config ~key ~contract ~params ~wc ?client ()

let send_transfer ~account ?src ~dst ~amount ?(bounce=false) ?(args=[]) () =
  let config = Config.config () in
  let net = Config.current_network config in

  let src = match src with
      None -> account
    | Some src -> src
  in
  let src_key = Misc.find_key_exn net src in

  let account_addr, account_contract =
    match Utils.is_address account with
    | Some address -> address, "SafeMultisigWallet"
    | None ->
        let account_key = Misc.find_key_exn net account in
        let account_addr = Misc.get_key_address_exn account_key in
        let account_contract = check_key_contract account_key in
        ( account_addr, account_contract )
  in
  let dst_addr = Utils.address_of_account config dst in

  let nanotokens, allBalance =
    if amount = "all" then
      2_000_000L, (* MIN_VALUE is 1e06 in SetcodeMultisigWallet2 *)
      true
    else
      Misc.nanotokens_of_string amount, false
  in

  begin match Utils.get_account_info config account_addr with
    | Some (account_exists, account_balance) ->
        if not account_exists then
          Error.raise "Account %s does not exist yet." account ;
        if ( not allBalance ) && Z.of_int64 nanotokens >= account_balance then
          Error.raise
            "Balance %s nanotons of account %s is smaller than transferred amount %s"
            (Z.to_string account_balance) account amount
    | None ->
        Error.raise "Account %s does not exist yet." account
  end;

  let dst_exists = match Utils.get_account_info config dst_addr with
      Some (dst_exists, _ ) -> dst_exists
    | None -> false
  in
  if bounce && not dst_exists then
    Error.raise "Destination does not exist. Use --parrain option";

  let args = match args with
    | [ meth ; params ] -> Some ( meth, params )
    | [ meth ] -> Some ( meth, "{}" )
    | [] -> None
    | _ ->
        Error.raise "Too many params arguments"
  in
  let payload = match args with
    | Some ( meth , params ) ->
        let dst_key = Misc.find_key_exn net dst in
        let dst_contract = Misc.get_key_contract_exn dst_key in
        let abi_file = Misc.get_contract_abifile dst_contract in
        let abi = EzFile.read_file abi_file in
        Ton_sdk.ABI.encode_body ~abi ~meth ~params
    | None -> ""
  in

  let meth, params =
    if allBalance then
      let meth = "sendTransaction" in
      let params = Printf.sprintf
          {|{"dest":"%s","value":0,"bounce":%b,"flags":128,"payload":"%s"}|}
          dst_addr
          bounce
          payload
      in
      Printf.eprintf "Warning: 'all' balance only works with one-custodian multisigs\n%!";
      meth, params
    else
      let meth = "submitTransaction" in
      let params = Printf.sprintf
          {|{"dest":"%s","value":%Ld,"bounce":%b,"allBalance":%b,"payload":"%s"}|}
          dst_addr
          nanotokens
          bounce
          allBalance
          payload
      in
      meth, params
  in
  Utils.call_contract config ~contract:account_contract
    ~address:account_addr
    ~meth ~params
    ~local:false
    ~src:src_key
    ()

let send_confirm ~account ?src ~tx_id () =
  let config = Config.config () in
  let net = Config.current_network config in
  let address = Utils.address_of_account config account in
  let src = match src with
      None -> account
    | Some src -> src
  in
  let src = Misc.find_key_exn net src in
  let contract = "SafeMultisigWallet" in

  let meth = "confirmTransaction" in
  let params =
    Printf.sprintf
      {|{"transactionId":"%s"}|} tx_id
  in

  Utils.call_contract config ~contract
    ~address
    ~meth ~params
    ~local:false
    ~src
    ()

let action account args ~create ~req ~not_owner ~custodians ~waiting
    ~transfer ~dst ~bounce ~confirm ?wc ~debot ~contract ~src =

  let config = Config.config () in
  if debot then begin
    let account = match account with
      | None -> "debot-multisig"
      | Some account -> account in
    let address = Utils.address_of_account config account in
    CommandClient.action ~exec:false [ "debot" ; "fetch" ; address ] ;
  end ;

  let account = match account with
    | None ->
        Error.raise "The argument --account ACCOUNT is mandatory"
    | Some account -> account
  in

  if create && transfer != None then
    Error.raise "--create and --transfer cannot be used together";

  Subst.with_substituted_list config args (fun args ->
      if create then
        create_multisig account ~accounts:args ~not_owner ~req ?wc ?contract ;          if custodians then
        get_custodians account ;
      begin
        match transfer, dst with
        | Some amount, Some dst ->
            send_transfer ~account ?src ~dst ~bounce ~amount ~args ()
        | None, None ->
            ()
        | _ ->
            Error.raise "--transfer AMOUNT --to DEST"
      end;
      if waiting then
        get_waiting account ;
      begin
        match confirm with
        | None -> ()
        | Some tx_id ->
            send_confirm ~account ~tx_id ?src ()
      end;
      ()
    )

let cmd =
  let args = ref [] in
  let contract = ref None in
  let account = ref None in

  let create = ref false in
  let not_owner = ref false in
  let req = ref 1 in
  let custodians = ref false in
  let waiting = ref false in

  let wc = ref None in

  let src = ref None in
  let transfer = ref None in
  let dst = ref None in
  let bounce = ref true in
  let confirm = ref None in
  let debot = ref false in
  EZCMD.sub
    "multisig"
    (fun () ->
       action !account !args
         ~create:!create
         ~req:!req
         ~not_owner:!not_owner
         ~custodians:!custodians
         ~waiting:!waiting
         ~transfer:!transfer
         ~dst:!dst
         ~bounce:!bounce
         ~confirm:!confirm
         ~src:!src
         ?wc:!wc
         ~debot:!debot
         ~contract:!contract
    )
    ~args:
      [
        [], Arg.Anons (fun list -> args := list),
        EZCMD.info "Generic arguments" ;

        [ "a" ; "account" ], Arg.String (fun s -> account := Some s),
        EZCMD.info "ACCOUNT The multisig account";

        [ "src" ], Arg.String (fun s -> src := Some s),
        EZCMD.info "ACCOUNT The multisig account";

        [ "wc" ], Arg.Int (fun s -> wc := Some s),
        EZCMD.info "WORKCHAIN The workchain (default is 0)";

        [ "create" ], Arg.Set create,
        EZCMD.info "Deploy multisig wallet on account (use generic arguments for owners)";

        [ "not-owner" ], Arg.Set not_owner,
        EZCMD.info " Initial account should not be an owner";

        [ "parrain" ], Arg.Clear bounce,
        EZCMD.info " Transfer to inactive account";
        [ "bounce" ], Arg.Bool (fun b -> bounce := b),
        EZCMD.info "BOOL Transfer to inactive account";

        [ "custodians" ], Arg.Set custodians,
        EZCMD.info "List custodians";

        [ "waiting" ], Arg.Set waiting,
        EZCMD.info " List waiting transactions";

        [ "confirm" ], Arg.String (fun s -> confirm := Some s),
        EZCMD.info "TX_ID Confirm transaction";

        [ "contract" ], Arg.String (fun s -> contract := Some s),
        EZCMD.info "CONTRACT Use this contract";

        [ "surf" ], Arg.Unit (fun () ->
            contract := Some "SetcodeMultisigWallet2"),
        EZCMD.info "Use Surf contract";

        [ "req" ], Arg.Int (fun s -> req := s),
        EZCMD.info "REQ Number of confirmations required";

        [ "transfer" ], Arg.String (fun s -> transfer := Some s),
        EZCMD.info "AMOUNT Transfer this amount";

        [ "to" ], Arg.String (fun s -> dst := Some s),
        EZCMD.info "ACCOUNT Target of a transfer";

        [ "debot" ], Arg.Set debot,
        EZCMD.info "Start the multisig debot";

      ]
    ~doc: "Manage a multisig-wallet (create, confirm, send)"
    ~man:[
      `S "DESCRIPTION";
      `P "This command is used to manage a multisig wallet, i.e. create the wallet, send tokens and confirm transactions.";

      `S "CREATE MULTISIG";
      `P "Create an account and get its address:";
      `Pre {|# ft account --create my-account
# ft genaddr my-account|};
      `P "Backup the account info off-computer.";
      `P "The second command will give you an address in 0:XXX format. Send some tokens on the address to be able to deploy the multisig.";
      `P "Check its balance with:";
      `Pre {|# ft account my-account|};
      `P "Then, to create a single-owner multisig:";
      `Pre {|# ft multisig -a my-account --create|} ;
      `P "To create a multi-owners multisig:";
      `Pre {|# ft multisig -a my-account --create owner2 owner3 owner4|} ;
      `P "To create a multi-owners multisig with 2 signs required:";
      `Pre {|# ft multisig -a my-account --create owner2 owner3 --req 2|} ;
      `P "To create a multi-owners multisig not self-owning:";
      `Pre {|# ft multisig -a my-account --create owner1 owner2 owner3 --not-owner|} ;

      `P "Verify that it worked:";
      `Pre {|# ft account my-account -v|};

      `S "GET CUSTODIANS";
      `P "To get the list of signers:";
      `Pre {|# ft multisig -a my-account --custodians"|};

      `S "SEND TOKENS";
      `P "Should be like that:";
      `Pre {|# ft multisig -a my-account --transfer 100.000 --to other-account|};
      `P "If the target is not an active account:";
      `Pre {|# ft multisig -a my-account --transfer 100.000 --to other-account --parrain|};
      `P "To send all the balance:";
      `Pre {|# ft multisig -a my-account --transfer all --to other-account|};

      `S "LIST WAITING TRANSACTIONS";
      `P "Display transactions waiting for confirmations:";
      `Pre {|# ft multisig -a my-account --waiting|};

      `S "CONFIRM TRANSACTION";
      `P "Get the transaction ID from above, and use:";
      `Pre {|# ft multisig -a my-account --confirm TX_ID|};
    ]
