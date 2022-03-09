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

open Ez_file.V1
open Ezcmd.V2
open EZCMD.TYPES
open Types

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
    | 0L, 0L, _ -> Printf.sprintf "%03Ld" millitons
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

let gen_address config keypair contract
    ?initial_data ?initial_pubkey ?wc () =
  Misc.with_contract contract
    (fun ~contract_tvc ~contract_abi ->

       if Globals.use_ton_sdk then
         let abi = EzFile.read_file contract_abi in
         Ton_sdk.CRYPTO.generate_address
           ~tvc_file:contract_tvc
           ~abi ~keypair ?initial_data ?initial_pubkey
           ()
       else
         match initial_data, initial_pubkey with
         | None, None ->
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
               )
         | _ -> Error.raise "Cannot use FT_USE_TONOS with initial data/pubkey"
    )

let add_account config ~initial_data ~initial_pubkey ~pubkey
    ~name ~passphrase ~address ~contract ~wc ~keyfile ~force =
  let net = Config.current_network config in
  let key_name = name in
  (try
     Misc.check_new_key_exn net name
   with exn ->
     if force then
       net.net_keys <-
         List.filter (fun k -> k.key_name <> name) net.net_keys
     else
       raise exn
  );

  let contract = Option.map Misc.fully_qualified_contract contract in

  let key_passphrase =
    match passphrase, keyfile, pubkey, address with
    | None, None, None, None -> Some ( gen_passphrase config )
    | _ -> passphrase
  in

  let key_pair =
    match keyfile, key_passphrase with
    | Some _, Some _ ->
        Error.raise "--passphrase and --keyfile are incompatible"
    | None, None -> None
    | None, Some passphrase ->
        Some ( gen_keypair config passphrase )
    | Some file, None -> Some ( Misc.read_json_file Encoding.keypair file )
  in

  let key_pair =
    match key_pair, pubkey with
    | None, None -> None
    | Some _, Some _ ->
        Error.raise "--pubkey is incompatible with --passphrase or --keyfile"
    | None, Some public ->
        if String.length public <> 64 then
          Error.raise "Pubkey should be 64 bytes hexa (no 0x)";
        Some { Sdk_types.public ; secret = None }
    | key_pair, None -> key_pair
  in

  let key_account = match address, contract, key_pair with
    | Some _, Some _, Some _ ->
        Error.raise "--address is incompatible with combining --contract and --keyfile/--passphrase"
    | Some acc_address, _ , _ ->
        Some { acc_address ;
               acc_contract = contract ;
               acc_workchain = wc ;
               acc_static_vars = initial_data ;
             }
    | None, Some contract, Some keypair ->
        let acc_address = gen_address config keypair contract
            ?initial_data ?initial_pubkey ?wc () in
        Some { acc_address ;
               acc_contract = Some contract ;
               acc_workchain = wc ;
               acc_static_vars = initial_data ;
             }
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
    ~name ?passphrase ?address ?contract
    ?initial_data ?initial_pubkey ?keyfile ?wc () =
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
      | Some file ->
          Some ( Misc.read_json_file Encoding.keypair file )
      | None -> None
    in

    let keyfile = match initial_pubkey with
      | None -> keyfile
      | Some public ->
          begin
            match keyfile with
            | None -> ()
            | Some keyfile ->
                if keyfile.Ton_sdk.TYPES.public <> public then
                  Error.raise "Keyfile and initial_pubkey differ";
                match passphrase with
                | None -> ()
                | Some _ ->
                    Error.raise "passphrase and initial_pubkey are incompatible"
          end;
          Some { public ; secret = None }
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

        let initial_data = match initial_data with
          | Some _ -> initial_data
          | None ->
              match key.key_account with
              | None -> None
              | Some { acc_static_vars ; _ } -> acc_static_vars
        in

        let key_account =
          match acc_contract with
          | None -> None
          | Some contract ->
              let acc_address = gen_address config key_pair contract
                  ?initial_data ?initial_pubkey  ?wc () in
              Some { acc_address ;
                     acc_contract = Some contract ;
                     acc_workchain = wc ;
                     acc_static_vars = initial_data ;
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

    | None, Some key_pair ->

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

        let initial_data = match initial_data with
          | Some _ -> initial_data
          | None ->
              match key.key_account with
              | None -> None
              | Some { acc_static_vars ; _ } -> acc_static_vars
        in

        let key_account =
          match acc_contract with
          | None -> None
          | Some contract ->
              let acc_address = gen_address config key_pair contract
                  ?initial_data ?initial_pubkey ?wc () in
              Some { acc_address ;
                     acc_contract = Some contract ;
                     acc_workchain = wc ;
                     acc_static_vars = initial_data ;
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

        let initial_data = match initial_data with
          | Some _ -> initial_data
          | None ->
              match key.key_account with
              | None -> None
              | Some { acc_static_vars ; _ } -> acc_static_vars
        in

        match address, acc_contract, key.key_pair with
        | Some _,  _, Some _ ->
            Error.raise
              "--address is incompatible with known keyfile and contract";
        | Some acc_address, acc_contract, None ->

            key.key_account <- Some { acc_address ;
                                      acc_contract ;
                                      acc_workchain = wc ;
                                      acc_static_vars = initial_data ;
                                    };
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

            let acc_address = gen_address config key_pair contract
                ?initial_data ?initial_pubkey ?wc () in
            key.key_account <-
              Some { acc_address ;
                     acc_contract = Some contract ;
                     acc_workchain = wc ;
                     acc_static_vars = initial_data ;
                   };
            config.modified <- true

  end;

  if config.modified then begin
    Printf.printf "Account %S modified.\n%!" key.key_name;
    get_key_info config key ~info:true
  end


let genkey ?name ?contract ?initial_data ?initial_pubkey config ~force =
  let net = Config.current_network config in
  begin
    match name with
    | None -> ()
    | Some name ->
        try
          Misc.check_new_key_exn net name
        with exn ->
          if force then
            net.net_keys <-
              List.filter (fun k -> k.key_name <> name) net.net_keys
          else
            raise exn
  end;
  let seed_phrase, keypair =
    match initial_pubkey with
    | None ->
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
        Some seed_phrase, keypair
    | Some public ->
        None, { public ; secret = None }
  in
  match name with
  | None -> ()
  | Some name ->
      let net = Config.current_network config in
      net.net_keys <- {
        key_name = name ;
        key_pair = Some keypair;
        key_passphrase = seed_phrase;
        key_account = None ;
      } :: net.net_keys;
      config.modified <- true;
      Printf.eprintf "Key for user %S generated\n%!" name;

      match contract with
      | None -> ()
      | Some contract ->
          change_account config ~name ~contract
            ?initial_data ?initial_pubkey ()

let action accounts ~passphrase ~address ~contract ~keyfile ~wc ~pubkey
    ~force ~initial_data =
  let config = Config.config () in

  let subst, _ = Subst.subst_string config in
  let passphrase = Option.map subst passphrase in
  let initial_data = Option.map subst initial_data in
  let keyfile = Option.map subst keyfile in

  match passphrase, address, contract, keyfile, wc, pubkey with
  | None, None, _, None, None, None when
      ( contract = None ) ->
      begin
        match accounts with
          [] -> genkey config ~force
        | _ ->
            let contract = Option.map Misc.fully_qualified_contract contract in
            List.iter (fun name ->
                genkey ~name config ?contract ?initial_data ~force
              ) accounts;
      end
  | _ ->
      match accounts with
      | _ :: _ :: _ ->
          Error.raise
            "Only one account can be created/specified with advanced options"
      | [] -> Error.raise "A new key name must be provided"
      | [ name ] ->
          add_account config
            ~name ~passphrase ~address ~contract ~keyfile ~wc ~force ~pubkey
            ~initial_data ~initial_pubkey:None

let cmd =
  let accounts = ref [] in
  let passphrase = ref None in
  let address = ref None in
  let contract = ref None in
  let keyfile = ref None in
  let wc = ref None in
  let force = ref false in
  let static_vars = ref None in
  let pubkey = ref None in
  EZCMD.sub
    "account create"
    (fun () ->
       action
        !accounts
        ~passphrase:!passphrase
        ~address:!address
        ~contract:!contract
        ~keyfile:!keyfile
        ~wc:!wc
        ~force:!force
        ~initial_data:!static_vars
        ~pubkey:!pubkey
    )
    ~args:
      [ [],
        Arg.Anons (fun args -> accounts := args),
        EZCMD.info "Name of account" ;

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

        [ "static-vars"],
        Arg.String (fun s -> static_vars := Some s),
        EZCMD.info ~docv:"JSON" "Set static vars for account";

        [ "surf" ],
        Arg.Unit (fun () -> contract := Some "SetcodeMultisigWallet2"),
        EZCMD.info "Contract should be TON Surf contract" ;

        [ "multisig" ],
        Arg.Unit (fun () -> contract := Some "SafeMultisigWallet"),
        EZCMD.info "Contract should be multisig" ;

        [ "keyfile"],
        Arg.String (fun s -> keyfile := Some s),
        EZCMD.info ~docv:"KEYFILE" "Key file for account";

        [ "pubkey"],
        Arg.String (fun s -> pubkey := Some s),
        EZCMD.info ~docv:"0xPUBKEY" "Public Key for account";

        [ "wc" ], Arg.Int (fun s -> wc := Some s),
        EZCMD.info ~docv:"WORKCHAIN" "The workchain (default is 0)";

        [ "force" ; "f" ], Arg.Set force,
        EZCMD.info "Override existing contracts with --create";

      ]
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command creates new accounts in the wallet" ;
        `P "Examples:";
        `Pre {|ft account create account1 account2 account3|};
        `Pre {|ft account create new-account --passphrase "some known passphrase" --contract SetcodeMultisigWallet2|};
        `Pre {|ft account create new-address --address 0:1234... --surf|};
        `P "The accounts are created in the wallet, not in the blockchain. To create accounts on the blockchain, you need to transfer funds to the account address and deploy a contract (for example, with 'ft multisig create')"
      ];

    ]
    ~doc: "Create new accounts in the wallet"

let () =
  Subst.add_dyn_subst "create-account"
    (fun _iter rem ->
       let config = Config.config () in
       match rem with
       | name :: rem ->
           let address = String.concat ":" rem in
           add_account config
             ~name ~address:(Some address)
             ~passphrase:None
             ~contract:None
             ~keyfile:None
             ~wc:None
             ~force:false
             ~initial_data:None
             ~initial_pubkey:None
             ~pubkey:None
           ;
           address
       | _ -> Error.raise "Wrong arity for create-account:NAME:ADDRESS"
    ) ;
  Subst.add_dyn_subst "create-contract"
    (fun _iter rem ->
       let config = Config.config () in
       match rem with
       | name :: contract :: rem ->
           let address = String.concat ":" rem in
           add_account config
             ~name
             ~address:(Some address)
             ~contract:(Some contract)
             ~passphrase:None
             ~keyfile:None
             ~wc:None
             ~force:false
             ~initial_data:None
             ~initial_pubkey:None
             ~pubkey:None;
           address
       | _ -> Error.raise "Wrong arity for create-contract:NAME:CONTRACT:ADDRESS"
    ) ;

  ()
