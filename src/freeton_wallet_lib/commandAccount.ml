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
open Types

(*
Known code hashes:
* DePool:
14e20e304f53e6da152eb95fffc993dbd28245a775d847eed043f7c78a503885
* SetcodeMultisigWallet2:
207dc560c5956de1a2c1479356f8f3ee70a59767db2bf4788b1d61ad42cdad82
* Giver:
fdfab26e1359ddd0c247b0fb334c2cc3943256a263e75d33e5473567cbe2c124

*)

type account_type = Uninit

let get_account_info config address =
  let addr = Misc.raw_address address in
  let open Ton_sdk in
  let level = if !Globals.verbosity > 1 then 3 else 1 in
  match
    Utils.post config
      ( REQUEST.account ~level addr )
  with
  | [] -> None
  |  [ account ] ->
      if !Globals.verbosity > 1 then
        Format.printf "%s@."
          (EzEncoding.construct ~compact:false ENCODING.accounts_enc [account]
          );
      begin
        match account.acc_code_hash with
        | None -> ()
        | Some code_hash ->
            match address with
            | RawAddress _ -> ()
            | Account acc ->
                match acc.acc_contract with
                | Some _ -> ()
                | None ->
                    match Misc.contract_of_code_hash ~code_hash with
                    | None -> ()
                    | Some contract ->
                        Printf.eprintf "Setting contract %S for %s\n%!"
                          contract acc.acc_address;
                        acc.acc_contract <- Some contract;
                        config.modified <- true
      end;
      Some account
  | _ -> assert false


let cut v =
  let rem = Int64.rem v 1_000L in
  let v = Int64.div v 1_000L in
  v, rem

let string_of_nanoton v =
  let v, nanotons = cut v in
  let v, mutons = cut v in
  let v, millitons = cut v in
  let v, tons = cut v in
  let v, thousandtons = cut v in
  let v, milliontons = cut v in
  let v, billiontons = cut v in
  assert (v = 0L);
  let tons =
    match billiontons, milliontons, thousandtons with
    | 0L, 0L, 0L -> Int64.to_string tons
    | 0L, 0L, _ -> Printf.sprintf "%Ld_%03Ld" thousandtons tons
    | 0L, _, _ -> Printf.sprintf "%Ld_%03Ld_%03Ld" milliontons thousandtons tons
    | _, _, _ -> Printf.sprintf "%Ld_%03Ld_%03Ld_%03Ld"
                   billiontons milliontons thousandtons tons
  in
  let nanotons = match nanotons, mutons, millitons with
    | 0L, 0L, 0L -> ""
    | 0L, 0L, _ -> Int64.to_string millitons
    | 0L, _, _ -> Printf.sprintf "%03Ld_%03Ld" millitons mutons
    | _, _, _ -> Printf.sprintf "%03Ld_%03Ld_%03Ld" millitons mutons nanotons
  in
  let s = Printf.sprintf "%s.%s" tons nanotons in
  s

let get_account_info config ~name ~address =
  match get_account_info config address with
  | None ->
      Printf.printf "Account %S: not yet created\n%!" name
  | Some account ->
      Printf.printf "Account %S: %s\n%!" name
        (match account.acc_balance with
         | None -> "no balance"
         | Some n ->
             Printf.sprintf "%s TONs (%s)"
               (string_of_nanoton (Z.to_int64 n))
               (match account.acc_type_name with
                | None -> "Non Exists"
                | Some s ->
                    match address with
                    | Account { acc_contract = Some contract; _ } ->
                        Printf.sprintf "%s: %s" contract s
                    | _ -> s
               ))

let get_key_info config key ~info =
  if info then
    let json = EzEncoding.construct ~compact:false Encoding.key key in
    Printf.printf "%s\n%!" json
  else
    let address = match key.key_account with
      | None ->
          Error.raise "Address %s has no address (use genaddr before)"
            key.key_name
      | Some account -> Account account
    in
    get_account_info config ~address ~name:key.key_name

let shorten_key s =
  let len = String.length s in
  Printf.sprintf "%s/%s" (String.sub s 0 4) (String.sub s (len-4) 4)

let shorten_addr s =
  let len = String.length s in
  Printf.sprintf "%s/%s" (String.sub s 0 6) (String.sub s (len-4) 4)

let get_account_info accounts ~list ~info =

  let config = Config.config () in
  let net = Config.current_network config in
  if list then
    List.iter (fun key ->
        Printf.printf "* %S%s%s%s\n"
          key.key_name
          (match key.key_passphrase with
           | Some _ -> " P"
           | None -> "")
          (match key.key_pair with
           | None -> ""
           | Some pair ->
               Printf.sprintf " %s%s"
                 (shorten_key pair.public)
                 (match pair.secret with
                  | None -> ""
                  | Some _ -> " P"))
          (match key.key_account with
           | None -> ""
           | Some acc ->
               Printf.sprintf " %s%s" (shorten_addr acc.acc_address)
                 (match acc.acc_contract with
                  | None -> ""
                  | Some s -> Printf.sprintf " (%s)" s)
          )
      ) net.net_keys
  else
  if info then
    match accounts with
    | [] -> List.iter (fun key ->
        match key.key_account with
        | None -> ()
        | Some _ ->
            get_key_info config key ~info
      ) net.net_keys
    | names ->
        List.iter (fun name ->
            match Misc.find_key net name with
            | None ->
                Error.raise "No key %S in network %S" name net.net_name
            | Some key ->
                get_key_info config key ~info
          ) names
  else
    match accounts with
    | [] ->
        List.iter (fun key ->
            match key.key_account with
            | None -> ()
            | Some account ->
                get_account_info config
                  ~address:( Account account ) ~name:key.key_name
          ) net.net_keys
    | _ ->
        List.iter (fun account ->
            let address = Utils.address_of_account config account in
            get_account_info config ~address ~name:account
          ) accounts

let whois address =
  let config = Config.config () in
  let net = Config.current_network config in
  let re = Re.Str.regexp address in
  List.iter (fun key ->
      match key.key_account with
      | None -> ()
      | Some acc ->
          match Re.Str.search_forward re acc.acc_address 0 with
          | exception Not_found -> ()
          | _ ->
              Printf.printf "%s is %S\n%!" acc.acc_address key.key_name)
    net.net_keys ;
  exit 0

let gen_passphrase config =
  if Globals.use_ton_sdk then
    Ton_sdk.CRYPTO.generate_mnemonic ()
  else
    let stdout = Misc.call_stdout_lines @@ Utils.tonoscli config ["genphrase"] in
    match stdout with
    | [ _ ; "Succeeded." ; seed ] ->
        begin match EzString.split seed '"' with
          | [ "Seed phrase: " ; seed_phrase ; "" ] ->
              seed_phrase
          | _ ->
              Error.raise "Could not parse seed phrase of tonos-cli genphrase"
        end
    | _ -> Error.raise "Could not parse output of tonos-cli genphrase: [%s]"
             (String.concat "|" stdout)

let gen_keypair config passphrase =
  if Globals.use_ton_sdk then
    Ton_sdk.CRYPTO.generate_keypair_from_mnemonic passphrase
  else
    let tmpfile = Misc.tmpfile () in
    Misc.call @@ Utils.tonoscli config
      [ "getkeypair" ; tmpfile; passphrase ];
    let keypair = Misc.read_json_file Encoding.keypair tmpfile in
    Sys.remove tmpfile;
    keypair

let gen_address config keypair contract ~wc =
  Misc.with_contract contract
    (fun ~contract_tvc ~contract_abi ->

       if Globals.use_ton_sdk then
         let abi = EzFile.read_file contract_abi in
         Ton_sdk.CRYPTO.generate_address
           ~tvc_file:contract_tvc
           ~abi ~keypair
           ()
       else

       Misc.with_keypair keypair (fun ~keypair_file ->

           let stdout = Misc.call_stdout_lines @@
             Utils.tonoscli config ["genaddr" ;
                                   contract_tvc ;
                                   contract_abi ;
                                   "--setkey" ; keypair_file ;
                                   "--wc" ; Misc.string_of_workchain wc
                                  ] in
           Misc.find_line_ok (function
               | [ "Raw" ; "address:" ; s ] -> Some s
               | _ -> None) stdout
         ))

let genaddr config contract key ~wc =

  let key_pair =
    match key.key_pair with
    | None ->
        Error.raise "Cannot genaddr without  keypair for %S" key.key_name
    | Some key_pair -> key_pair
  in
  let addr = gen_address config key_pair contract ~wc in
  Printf.printf "Address (%s for %s=%s...): %s\n%!"
    contract key.key_name
    (String.sub key_pair.public 0 10) addr;
  key.key_account <- Some {
      acc_address = addr ;
      acc_contract = Some contract ;
      acc_workchain = wc ;
    };
  config.modified <- true

let add_account config
    ~name ~passphrase ~address ~contract ~wc ~keyfile =
  let net = Config.current_network config in
  let key_name = name in
  Misc.check_new_key_exn net name;

  let contract = Option.map Misc.fully_qualified_contract contract in

  let subst, _ = Subst.subst_string config in
  let key_passphrase = Option.map subst passphrase in

  let key_pair =
    match keyfile, key_passphrase with
    | Some _, Some _ ->
        Error.raise "--passphrase and --keyfile are incompatible"
    | None, None -> None
    | None, Some passphrase ->
        Some ( gen_keypair config passphrase )
    | Some file, None -> Some ( Misc.read_json_file Encoding.keypair file )
  in

  let key_account = match address, contract, key_pair with
    | Some _, Some _, Some _ ->
        Error.raise "--address is incompatible with combining --contract and --keyfile/--passphrase"
    | Some acc_address, _ , _ ->
        Some { acc_address ;
               acc_contract = contract ;
               acc_workchain = wc ; }
    | None, Some contract, Some keypair ->
        let acc_address = gen_address config keypair contract ~wc in
        Some { acc_address ;
               acc_contract = Some contract ;
               acc_workchain = wc ;}
    | None, Some _, None ->
        Error.raise "--contract CONTRACT requires either --address, --keyfile or --passphrase"
    | None, None, None -> None
    | None, None, Some _ -> None
  in
  let key = { key_name ; key_account ; key_passphrase ; key_pair } in
  net.net_keys <- key :: net.net_keys ;
  config.modified <- true;
  Printf.eprintf "Account %S created.\n%!" key_name;
  get_key_info config key ~info:true;
  ()

let change_account config
    ~name ?passphrase ?address ?contract ?keyfile ?wc () =
  let net = Config.current_network config in
  let key = match Misc.find_key net name with
    | None -> Error.raise "Unknown account %S cannot be modified\n%!" name
    | Some key -> key
  in

  begin
    let passphrase = match passphrase with
      | Some "" | Some "none" ->
          if key.key_passphrase <> None then begin
            key.key_passphrase <- None;
            config.modified <- true
          end;
          None
      | _ -> passphrase
    in

    let keyfile = match keyfile with
      | Some "" | Some "none" ->
          if key.key_passphrase <> None then
            Error.raise
              "Cannot clear keyfile without clearing first passphrase";
          if key.key_pair <> None then begin
            key.key_pair <- None;
            config.modified <- true;
          end;
          None
      | _ -> keyfile
    in

    let contract = match contract with
      | Some ""  | Some "none" ->
          begin
            match key.key_account with
            | None -> ()
            | Some acc ->
                acc.acc_contract <- None ;
                if key.key_pair <> None then
                  key.key_account <- None ;
                config.modified <- true
          end;
          None
      | _ ->
          Option.map Misc.fully_qualified_contract contract
    in

    let address, contract = match address with
      | Some "" | Some "none" ->
          begin
            match key.key_account with
            | None -> None, contract
            | Some acc ->
                key.key_account <- None;
                config.modified <- true;
                match contract with
                | None -> None, acc.acc_contract
                | Some _ -> None, contract
          end
      | _ -> address, contract
    in

    begin
      match address, passphrase, keyfile with
      | Some _, Some _, _ ->
          Error.raise "--address and --passphrase are incomptible"
      | Some _, None, Some _ ->
          Error.raise "--address and --keyfile are incomptible"
      | _ -> ()
    end;

    match passphrase, keyfile with

    | Some _, Some _ ->
        Error.raise "--passphrase and --keyfile are incomptible"

    | Some passphrase, None ->
        if key.key_passphrase <> None then
          Error.raise
            "You cannot change the passphrase of an account. You must delete it and recreate it.";

        if keyfile <> None then
          Error.raise "--passphrase and --keyfile are incompatible";

        let key_pair = gen_keypair config passphrase in

        begin
          match key.key_pair with
          | None -> ()
          | Some { public ; _ } ->
              if public <> key_pair.public then
                Error.raise
                  "Public key %s with new passphrase does not match former one %s." key_pair.public public;
        end;

        let acc_contract = match contract, key.key_account with
          | Some contract, _ -> Some contract
          | None, None -> None
          | None, Some { acc_contract ; _ } -> acc_contract
        in

        let wc = match wc with
          | Some _ -> wc
          | None ->
              match key.key_account with
              | None -> None
              | Some { acc_workchain ; _ } -> acc_workchain
        in

        let key_account =
          match acc_contract with
          | None -> None
          | Some contract ->
              let acc_address = gen_address config key_pair contract ~wc in
              Some { acc_address ;
                     acc_contract = Some contract ;
                     acc_workchain = wc
                   }
        in

        begin
          match key_account, key.key_account with
          | Some _, None -> ()
          | None, None -> ()
          | None, Some _ ->
              Error.raise "Since account address was known, you must specify the contract to use or delete the address first.";
          | Some { acc_address = new_address ; _ },
            Some { acc_address = former_address ; _ }
            ->
              if new_address <> former_address then
                Error.raise "New address %s is different from former address %s. You must delete it first."
                  new_address former_address;
        end;

        key.key_passphrase <- Some passphrase;
        key.key_pair <- Some key_pair;
        key.key_account <- key_account;
        config.modified <- true;

    | None, Some keyfile ->

        let key_pair = Misc.read_json_file Encoding.keypair keyfile in

        if key.key_pair <> None then
          Error.raise
            "You cannot change the keyfile of an account whose keyfile is already set. You must delete it and recreate it.";

        let acc_contract = match contract, key.key_account with
          | Some contract, _ -> Some contract
          | None, None -> None
          | None, Some { acc_contract ; _ } -> acc_contract
        in

        let wc = match wc with
          | Some _ -> wc
          | None ->
              match key.key_account with
              | None -> None
              | Some { acc_workchain ; _ } -> acc_workchain
        in

        let key_account =
          match acc_contract with
          | None -> None
          | Some contract ->
              let acc_address = gen_address config key_pair contract ~wc in
              Some { acc_address ;
                     acc_contract = Some contract ;
                     acc_workchain = wc }
        in

        begin
          match key_account, key.key_account with
          | Some _, None -> ()
          | None, None -> ()
          | None, Some _ ->
              Error.raise "Since account address was known, you must specify the contract to use or delete the address first.";
          | Some { acc_address = new_address ; _ },
            Some { acc_address = former_address ; _ }
            ->
              if new_address <> former_address then
                Error.raise "New address %s is different from former address %s. You must delete it first."
                  new_address former_address;
        end;

        key.key_pair <- Some key_pair;
        key.key_account <- key_account;
        config.modified <- true;

    | None, None ->

        let acc_contract = match contract, key.key_account with
          | Some contract, _ -> Some contract
          | None, None -> None
          | None, Some { acc_contract ; _ } -> acc_contract
        in

        let wc = match wc with
          | Some _ -> wc
          | None ->
              match key.key_account with
              | None -> None
              | Some { acc_workchain ; _ } -> acc_workchain
        in

        match address, acc_contract, key.key_pair with
        | Some _,  _, Some _ ->
            Error.raise
              "--address is incompatible with known keyfile and contract";
        | Some acc_address, acc_contract, None ->

            key.key_account <- Some { acc_address ;
                                      acc_contract ;
                                      acc_workchain = wc };
            config.modified <- true

        | None, Some _, None ->
            begin match key.key_account with
              | None ->
                  Error.raise
                    "You cannot set the contract without keys or address"
              | Some acc ->
                  acc.acc_contract <- acc_contract;
                  config.modified <- true
            end
        | None, None, _ -> ()
        | None, Some contract, Some key_pair ->
            if key.key_account <> None then
              Error.raise "You must clear address before changing contract";

            let acc_address = gen_address config key_pair contract ~wc in
            key.key_account <-
              Some { acc_address ;
                     acc_contract = Some contract ;
                     acc_workchain = wc };
            config.modified <- true

  end;

  if config.modified then begin
    Printf.printf "Account %S modified.\n%!" key.key_name;
    get_key_info config key ~info:true
  end

let get_live accounts =
  let config = Config.config () in
  let net = Config.current_network config in
  let host = match net.net_name with
    | "mainnet" -> "ton.live"
    | "testnet" -> "net.ton.live"
    | _ -> assert false
  in
  List.iter (fun account ->
      let address = Utils.address_of_account config account in
      let addr = Misc.raw_address address in
      let url = Printf.sprintf
          "https://%s/accounts/accountDetails?id=%s" host addr in
      Misc.call [ "xdg-open" ; url ]
    ) accounts


let genkey ?name ?contract config =
  let net = Config.current_network config in
  begin
    match name with
    | None -> ()
    | Some name ->
        Misc.check_new_key_exn net name
  end;
  let seed_phrase = gen_passphrase config in

  (*
  let stdout = Misc.call_stdout_lines
    @@ Misc.tonoscli config [ "genpubkey" ; seed_phrase  ] in
  let pubkey =
    match stdout with
    | _ :: "Succeeded." :: pubkey :: _ ->
        begin match EzString.split pubkey ' ' with
          | [ "Public"; "key:" ; pubkey ] -> pubkey
          | stdout ->
              Error.raise "Could not parse pubkey of tonos-cli genpubkey: [%s]"
                (String.concat "|" stdout)
        end
    | _ -> Error.raise "Could not parse output of tonos-cli genpubkey: [%s]"
             (String.concat "|" stdout)
  in
*)
  let keypair = gen_keypair config seed_phrase in
  Printf.eprintf "Passphrase: %S\n%!" seed_phrase;
  Printf.eprintf "{ \"public\": \"%s\",\n%!" keypair.public;
  Printf.eprintf "  \"secret\": \"%s\" }\n%!"
    (match keypair.secret with None -> assert false | Some s -> s);
  match name with
  | None -> ()
  | Some name ->
      let net = Config.current_network config in
      net.net_keys <- {
        key_name = name ;
        key_pair = Some keypair;
        key_passphrase = Some seed_phrase;
        key_account = None ;
      } :: net.net_keys;
      config.modified <- true;
      Printf.eprintf "Key for user %S generated\n%!" name;

      match contract with
      | None -> ()
      | Some contract ->
          change_account config ~name ~contract ()

let action accounts ~list ~info
    ~create ~delete ~passphrase ~address ~contract ~keyfile ~live ~wc =
  let config = Config.config () in
  match passphrase, address, contract, keyfile, wc with
  | None, None, _, None, None when
    ( contract = None || create ) ->
      if create then
        match accounts with
          [] -> genkey config
        | _ ->
            let contract = Option.map Misc.fully_qualified_contract contract in
            List.iter (fun name ->
                genkey ~name config ?contract
              ) accounts;
      else
      if delete then
        let net = Config.current_network config in
        List.iter (fun name ->
            Misc.delete_account config net name
          ) accounts;
        Printf.eprintf "All provided accounts deleted.\n%!"
      else
      if live then
        get_live accounts
      else
        get_account_info accounts ~list ~info
  | _ ->
      match accounts with
      | _ :: _ :: _ ->
          Error.raise
            "Only one account can be created/specified with advanced options"
      | [] -> Error.raise "A new key name must be provided"
      | [ name ] ->
          if create then
            add_account config
              ~name ~passphrase ~address ~contract ~keyfile ~wc
          else
            change_account config
              ~name ?passphrase ?address ?contract ?keyfile ?wc ()

let cmd =
  let passphrase = ref None in
  let address = ref None in
  let contract = ref None in
  let keyfile = ref None in
  let accounts = ref [] in
  let list = ref false in
  let info = ref false in
  let create = ref false in
  let delete = ref false in
  let live = ref false in
  let wc = ref None in
  EZCMD.sub
    "account"
    (fun () -> action
        !accounts
        ~list:!list
        ~info:!info
        ~create:!create
        ~delete:!delete
        ~passphrase:!passphrase
        ~address:!address
        ~contract:!contract
        ~keyfile:!keyfile
        ~live:!live
        ~wc:!wc
    )
    ~args:
      [ [],
        Arg.Anons (fun args -> accounts := args),
        EZCMD.info "Name of account" ;

        [ "list" ] ,
        Arg.Set list,
        EZCMD.info "List all accounts" ;

        [ "info" ] ,
        Arg.Set info,
        EZCMD.info "Display account parameters" ;

        [ "create" ] ,
        Arg.Set create,
        EZCMD.info "Create new account" ;

        [ "delete" ; "remove" ] ,
        Arg.Set delete,
        EZCMD.info "Delete old accounts" ;

        [ "passphrase"],
        Arg.String (fun s -> passphrase := Some s),
        EZCMD.info ~docv:"PASSPHRASE" "BIP39 Passphrase for account";

        [ "address"],
        Arg.String (fun s ->
            let s = match EzString.chop_prefix s ~prefix:"0-1:" with
                None -> s
              | Some s -> "-1:" ^ s
            in
            address := Some s),
        EZCMD.info ~docv:"ADDRESS" "Address for account";

        [ "contract"],
        Arg.String (fun s -> contract := Some s),
        EZCMD.info ~docv:"CONTRACT" "Contract for account";

        [ "surf" ],
        Arg.Unit (fun () -> contract := Some "SetcodeMultisigWallet2"),
        EZCMD.info "Contract should be TON Surf contract" ;

        [ "multisig" ],
        Arg.Unit (fun () -> contract := Some "SafeMultisigWallet"),
        EZCMD.info "Contract should be multisig" ;

        [ "keyfile"],
        Arg.String (fun s -> keyfile := Some s),
        EZCMD.info ~docv:"KEYFILE" "Key file for account";

        [ "live" ],
        Arg.Set live,
        EZCMD.info "Open block explorer on address";

        [ "wc" ], Arg.Int (fun s -> wc := Some s),
        EZCMD.info ~docv:"WORKCHAIN" "The workchain (default is 0)";

        [ "whois" ], Arg.String whois,
        EZCMD.info ~docv:"ADDRESS" "Returns corresponding key name";

      ]
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command can perform the following actions:";
        `I ("1.", "Display information on given accounts, either locally or from the blockchain");
        `I ("2.", "Create new accounts");
        `I ("3.", "Add information to existing accounts");
        `I ("4.", "Delete existing accounts");
      ];
      `S "DISPLAY LOCAL INFORMATION";
      `Blocks [
        `P "Examples:";
        `Pre {|ft account --list|};
        `Pre {|ft account my-account --info|}
      ];
      `S "DISPLAY BLOCKCHAIN INFORMATION";
      `Blocks [
        `P "Accounts must have an address on the blockchain.";
        `P "Examples:";
        `Pre {|ft account my-account|};
        `Pre {|ft account|};
      ];
      `S "CREATE NEW ACCOUNTS";
      `Blocks [
        `P "Examples:";
        `Pre {|ft account --create account1 account2 account3|};
        `Pre {|ft account --create new-account --passphrase "some known passphrase"|};
        `Pre {|ft account --create new-account --contract SafeMultisigWallet|};
        `Pre {|ft account --create new-address --address 0:1234...|};
        `P "Only the last one will compute an address on the blockchain, since the contract must be known.";
      ];
      `S "COMPLETE EXISTING ACCOUNTS";
      `Blocks [
        `P "Examples:";
        `Pre {|ft account old-account --contract SafeMultisigWallet|};
      ];
      `S "DELETE EXISTING ACCOUNTS";
      `Blocks [
        `P "Examples:";
        `Pre {|ft account --delete account1 account2|};
      ];

    ]
    ~doc:
      "Get account info (local or from blockchain), or create/modify/delete accounts."
