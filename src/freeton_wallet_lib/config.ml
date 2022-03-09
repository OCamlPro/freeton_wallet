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
open EzCompat
open EzFile.OP

open Types

let set_current_network = ref None
let set_current_node = ref None
let set_current_account = ref None

let set_config = ref
    (try
       Some ( Sys.getenv "FT_SWITCH")
     with _ -> None)

let set_temporary_switch net = set_config := Some net

let remote_account ?key_pair ?key_passphrase key_name ?contract:acc_contract acc_address =
  { key_name ;
    key_passphrase ;
    key_pair ;
    key_account = Some
        { acc_address ;
          acc_contract ;
          acc_workchain = None ;
          acc_static_vars = None ;
        } ;
  }

let mainnet_keys = [

  remote_account "debot-multisig"
    "0:9ce35b55a00da91cfc70f649b2a2a58414d3e21ee8d1eb80dab834d442f33606" ;

  remote_account "broxus-wton-vault"
    "0:d0936a9fc29b5175487208b1d07ab8042ce7ddbc2de7e271c4087ca833b865cc" ;

  remote_account "broxus-dex-root"
    "0:943bad2e74894aa28ae8ddbe673be09a0f3818fd170d12b4ea8ef1ea8051e940" ;

]

let surf_account acc_address =

  Some  { acc_address ;
          acc_contract = Some "SetcodeMultisigWallet2" ;
          acc_workchain = None ;
          acc_static_vars = None ;
        }

let tokens_manifest =
  "https://raw.githubusercontent.com/broxus/ton-assets/master/manifest.json"

let testnet_keys = [
  remote_account "giver"
    "0:653b9a6452c7a982c6dc92b2da9eba832ade1c467699ebb3b43dca6d77b780dd"
    ~contract:"TestnetGiver" ;
]

let sandbox_keys = [
  remote_account "giver"
    "0:841288ed3b55d9cdafa806807f02a0ae0c169aa5edfe88a789a6482429756a94"
    ~contract:"Giver" ;
  remote_account "giver2"
    "0:b5e9240fc2d2f1ff8cbb1d1dee7fb7cae155e5f6320e585fcc685698994a19a5"
    ~contract:"GiverV2" ;
  remote_account "msig"
    ~key_pair: {
      public =
        "99c84f920c299b5d80e4fcce2d2054b05466ec9df19532a688c10eb6dd8d6b33";
      secret =
        Some "73b60dc6a5b1d30a56a81ea85e0e453f6957dbfbeefb57325ca9f7be96d3fe1a";
    }
    ~key_passphrase:
      "fan harsh baby section father problem person void depth already powder chicken"
    "0:d5f5cfc4b52d2eb1bd9d3a8e51707872c7ce0c174facddd0e06ae5ffd17d2fcd"
    ~contract:"SafeMultisigWallet";

  { key_name = "user9" ;
    key_passphrase = Some
        "nut slice spawn badge online term asset door pill version add sugar";
    key_pair = Some
        { public =
            "a327b480cb40bdd0593b23ca85777ac9582ad250d67990b0dee778a1e2766382";
          secret = Some
              "32582b1650409f5025454d57329a7bc05ecbd0303ff0080154f0f965d12437d8" };
    key_account =
      surf_account
        "0:d0eeb6beda8c7ef8e190cfc859fdb1d642f59c08ad9d52b117262d2fcc910f17" ;
  };
  { key_name = "user8" ;
    key_passphrase = Some
        "twelve used bean frog like blood gap type erase enter emerge security";
    key_pair = Some
        { public =
            "d207f8fb2fccf047245e7b86866b39ac655c5f28baae3a02eafbae6d6427221d" ;
          secret = Some
              "8973bbcd27e6e8eced2c734e326498a57b906bf4a8d61659a2ef58fb2c453afb" };
    key_account =
      surf_account
        "0:dd343a443b3cb7542d6f0d6fafae3bbd36329c32d33b0ca9113f399150acd3bc" ;
  };
  { key_name = "user7" ;
    key_passphrase = Some
        "frost balance pyramid parrot easy credit sister trip shrug special trophy speed";
    key_pair = Some
        { public =
            "675a6987fbf9d6e5ed26936795492dc319e1c39fe4145745d73896c277542d52" ;
          secret = Some
              "14c354b9c554d68129cff59c1b9cbbe2950c0c8391eb9273001e1cbb45194a33" };
    key_account =
      surf_account
        "0:beb854d8de3a9845fa8139563af47cad4dcefbda7991f4e5f577743f73994d71" ;
  };
  { key_name = "user6" ;
    key_passphrase = Some
        "alter oven hard acoustic century call despair common light palm neck saddle";
    key_pair = Some
        { public =
            "a1155f4dad62d2936e2ee1da26f7e352a6574625cb0d9c4431f5bc4e4879fcc2" ;
          secret = Some
              "3a741642e6c01014d7c359a6548c216c118d9612d9d734dac11ed6c4217e43c6" };
    key_account =
      surf_account
        "0:649ac71c032f04894d19fd0467deee43ac84db5fc5ebfc298974850502b1fdb5" ;
  };
  { key_name = "user5" ;
    key_passphrase = Some
        "frown point legend unlock damage hockey gentle march easily relief person brief";
    key_pair = Some
        { public =
            "43c9873928299e674c37f41b11b8fbf92b4131fcf5102fcce19e59469a8ab747" ;
          secret = Some
              "30b74dd563917bafe3269f298350cdfc5d10eb44efee4d7623a9635536d2af54" };
    key_account =
      surf_account
            "0:215879e44133ad2dbf62816fdaefeccd3ced279ab4324966736d9a6cb5532c47" ;
  };
  { key_name = "user4" ;
    key_passphrase = Some
        "dismiss episode cabin comic income code avoid wave frost false rocket abandon";
    key_pair = Some
        { public =
            "c5f199fd3a88385901eab994fddbf90ff9899495e9fe3fb1ccbc076d134abf35" ;
          secret = Some
              "06090e0a7c3b9dacc64da8047096f81046736f28c49629f2f26ab8ae8e506bf0" };
    key_account = surf_account
        "0:b52e00b619de1c2b002aef084cbf9d66efa0b58f94897d72ad586f7b2ed9a2f3" ;
  };
  { key_name = "user3" ;
    key_passphrase = Some
        "elite notable can skull polar anxiety basket defense acid absurd photo scheme";
    key_pair = Some
        { public =
            "e950bca31326f661144c918020a10bfd6e1f3a6bd16dba504856c5983ae1363f" ;
          secret = Some
              "0138839a4d388ed882aaf4bed56841c5e3bd701884e791067e33e428f19a11e0" };
    key_account = surf_account
        "0:e4fe3af7e0afc23fb1209dd956dd74cfaf7b805b16d6809047de7aa9ad65c354" ;
  };
  { key_name = "user2" ;
    key_passphrase = Some
        "lizard all pear connect gentle state hero enrich law aerobic cook eternal";
    key_pair = Some
        { public =
            "d183edabfa08d900dd1b177948351817771cdd57ac9ff8576925ad76856689eb" ;
          secret = Some
              "6b51fc5b9419696f18252e2cf067e4a604bb8c06554718e289aa7724f8625753" };
    key_account =
      surf_account
        "0:c0788ae1d2f2e698ef95cfe9c2221e1f334d69779bfdbd5c1bf22aa95eb91f4f" ;
  };
  { key_name = "user1" ;
    key_passphrase = Some
        "describe pencil flip state bench aware power fashion age drift peanut advance";
    key_pair = Some
        { public =
            "0857aea8cc2c96b955095ee2ceaf71f6708db009caa720ca8cd962ca1aecb4f2" ;
          secret = Some
              "df0620047586fae3be0dc9f8c6b0191f5e70c6774642317ef452c835d36b3b37" };
    key_account =
      surf_account
        "0:f89872394a383dc289f27ded48f02a6269e19d02be5821ba8081c67a1070588a" ;
  };
  { key_name = "user0" ;
    key_passphrase = Some
        "calm price ecology burger notice urge small artefact afford seed best album";
    key_pair = Some
        { public =
            "fd9ee2babfa35b65917f732316dbb3d31935ccacd2a0aa92e043f8c762e0da28" ;
          secret = Some
              "7883b5b2962f1a2d4891f52a16f125b855b6be61f84b162a4476a2f240c9e2c9" } ;
    key_account =
      surf_account
        "0:108f6113fb0cad8c98b70e8ea3cfd12b52710ec20441d05ceb78cacb4f5566b7" ;
  }
]

let mainnet_node = {
  node_name = "tonlabs" ;
  node_url = "https://main.ton.dev";
  node_local = None ;
}

let mainnet_network = {
  net_name = "mainnet" ;
  current_node = "tonlabs" ;
  current_account = None ;
  net_nodes = [ mainnet_node ] ;
  net_keys = [] ;
  net_deployer = "deployer" ;
  net_toolchain = "";
}

let testnet_node = {
  node_name = "tonlabs" ;
  node_url = "https://net.ton.dev";
  node_local = None ;
}

let testnet_network = {
  net_name = "testnet" ;
  current_node = "tonlabs" ;
  current_account = None ;
  net_nodes = [ testnet_node ] ;
  net_keys = [ ];
  net_deployer = "deployer" ;
  net_toolchain = "";
}

let rustnet_node = {
  node_name = "tonlabs" ;
  node_url = "https://rustnet.ton.dev";
  node_local = None ;
}

let rustnet_network = {
  net_name = "rustnet" ;
  current_node = "tonlabs" ;
  current_account = None ;
  net_nodes = [ rustnet_node ] ;
  net_keys = [ ];
  net_deployer = "deployer" ;
  net_toolchain = "";
}

let fldnet_node = {
  node_name = "tonlabs" ;
  node_url = "https://fld.ton.dev";
  node_local = None ;
}

let fldnet_network = {
  net_name = "fldnet" ;
  current_node = "tonlabs" ;
  current_account = None ;
  net_nodes = [ fldnet_node ] ;
  net_keys = [ ];
  net_deployer = "deployer" ;
  net_toolchain = "";
}

let nil_node = {
  node_name = "NilFoundation" ;
  node_url = "https://net.freeton.nil.foundation/" ;
  node_local = None ;
}

let nil_network = {
  net_name = "nil" ;
  current_node = "NilFoundation" ;
  current_account = None ;
  net_nodes = [ nil_node ] ;
  net_keys = [ ];
  net_deployer = "deployer" ;
  net_toolchain = "nil";
}

let known_networks = [
  mainnet_network ;
  testnet_network ;
  fldnet_network ;
  rustnet_network ;
  nil_network ;
]

let repo_tonos_cli = "https://github.com/tonlabs/tonos-cli.git"
let repo_solc = "https://github.com/tonlabs/TON-Solidity-Compiler.git"
let repo_tvm_linker = "https://github.com/tonlabs/TVM-linker.git"

let default_repos = {
  repo_toolchain = "";
  repo_tonos_cli ;
  repo_solc ;
  repo_tvm_linker ;
}

let nil_repos = {
  repo_toolchain = "nil";
  repo_tonos_cli = "https://github.com/NilFoundation/tonos-cli.git" ;
  repo_solc = "https://github.com/NilFoundation/tvm-solidity.git" ;
  repo_tvm_linker = "https://github.com/NilFoundation/tvm-lld.git" ;
}

let default_multisigs = [
  "SafeMultisigWallet" ;
  "SetcodeMultisigWallet" ;
  "SafeMultisigWallet24" ;
  "SetcodeMultisigWallet24" ;
  "SetcodeMultisigWallet2" ;
]

let default_config = {
  modified = true ;
  version = 0;
  current_network = "testnet" ;
  networks = [ testnet_network ;
               mainnet_network ;
             ] ;
  repos = None ;
  toolchains = [ default_repos ; nil_repos ];
  multisigs = default_multisigs ;
}

let save config =
  EzFile.make_dir ~p:true Globals.ft_dir;
  if Sys.file_exists Globals.config_file then begin
    Sys.rename Globals.config_file (Globals.config_file ^ "~")
  end;
  begin
    let config = { config with
                   networks = List.map (fun net ->
                       begin
                         match net.net_keys with
                         | [] -> ()
                         | keys ->
                             let wallet_dir = Globals.ft_dir // net.net_name in
                             let wallet_file = wallet_dir // "wallet.json" in
                             EzFile.make_dir ~p:true wallet_dir ;
                             Misc.write_json_file Encoding.wallet wallet_file keys;
                             if !Globals.verbosity > 0 then
                               Printf.eprintf "Saving wallet file %s\n%!" wallet_file ;
                       end ;
                       { net with net_keys = [] }
                     ) config.networks ;
                   repos = None;
                 }
    in
    if !Globals.verbosity > 0 then
      Printf.eprintf "Saving config file %s\n%!" Globals.config_file ;
    Misc.write_json_file Encoding.config Globals.config_file config ;
  end;
  config.modified <- false ;
  ()

let maybe_add_keys config net keys =
  let set = ref StringSet.empty in
  List.iter (fun k ->
      set := StringSet.add k.key_name !set
    ) net.net_keys;
  let set = !set in
  net.net_keys <-
    net.net_keys @
    List.filter (fun key ->
        let new_key = not (StringSet.mem key.key_name set) in
        if new_key then config.modified <- true;
        new_key
      ) keys

let load_wallet config net =
  match net.net_keys with
  | [] ->
      let wallet_file = Globals.ft_dir // net.net_name // "wallet.json" in
      if Sys.file_exists wallet_file then begin
        if !Globals.verbosity > 0 then
          Printf.eprintf "Loading wallet file %s\n%!" wallet_file ;
        net.net_keys <- Misc.read_json_file Encoding.wallet wallet_file ;

        if net.net_name = "mainnet" then
          maybe_add_keys config net mainnet_keys
        else
        if net.net_name = "testnet" then
          maybe_add_keys config net testnet_keys

      end
  | _ -> ()

let set_switch config switch =
  let list = EzString.split switch '.' in
  let rec set_node net list =
    match list with
    | [] -> ()
    | name :: tail ->
        begin
          match Misc.find_node net name, Misc.find_key net name with
          | Some _, Some _ -> assert false
          | Some _node, _ ->
              net.current_node <- name;
              config.modified <- true
          | None, Some _key ->
              net.current_account <- Some name;
              config.modified <- true
          | None, None ->
              Error.raise "Unknown node or account %S" name
        end;
        set_node net tail
  in
  let set_network list =
    match list with
    | [] -> ()
    | name :: tail ->
        match Misc.find_network config name with
        | Some net ->
            config.current_network <- name;
            config.modified <- true;
            set_node net tail
        | None ->
            let net =
              Misc.find_network_exn config config.current_network in
            set_node net list
  in
  set_network list ;
  ()

let current_network config =
  match !set_current_network with
  | None -> assert false
  | Some network ->
      Misc.find_network_exn config network

let current_node config =
  let net = current_network config in
  match !set_current_node with
  | None -> assert false
  | Some node ->
      match Misc.find_node net node with
      | None ->
          Error.raise "Unknown node %S in network %S"
            net.current_node net.net_name
      | Some node -> node

let load () =
  let config =
    if Sys.file_exists Globals.config_file then begin
      let config = Misc.read_json_file Encoding.config Globals.config_file in
      if !Globals.verbosity > 0 then
        Printf.eprintf "Config loaded from %s\n%!" Globals.config_file;
      config.modified <- false ;
      config
    end else begin
      if !Globals.verbosity > 0 then
        Printf.eprintf "File %s does not exist\n%!" Globals.config_file ;
      default_config
    end
  in


  let set_account net list =
    match list with
    | [] ->
        set_current_account := net.current_account
    |  name :: tail ->
        begin
          match Misc.find_key net name with
          | Some _key ->
              set_current_account := Some name
          | None ->
              Error.raise "Unknown node or account %S" name
        end;
        match tail with
        | [] -> ()
        | s :: _ ->
            Error.raise "Don't know what to do with switch tail %S" s
  in
  let set_node net list =
    match list with
    | [] ->
        set_current_node := Some net.current_node;
        set_account net []
    | name :: tail ->
        match Misc.find_node net name with
        | Some _node ->
            set_current_node := Some name;
            set_account net tail
        | None ->
            set_current_node := Some net.current_node;
            set_account net list
  in
  let set_network list =
    match list with
    | [] ->
        set_current_network := Some config.current_network;
        let net = Misc.find_network_exn config config.current_network in
        set_node net []
    | name :: tail ->
        match Misc.find_network config name with
        | Some net ->
            set_current_network := Some name;
            set_node net tail
        | None ->
            set_current_network := Some config.current_network;
            let net =
              Misc.find_network_exn config config.current_network in
            set_node net list
  in
  begin
    match !set_config with
    | None -> set_network []
    | Some switch ->
        let list = EzString.split switch '.' in
        set_network list ;
  end;
  List.iter (fun net ->
      match net.net_keys with
      | [] -> ()
      | _keys ->
          Printf.eprintf "Keys present. Need saving\n%!";
          config.modified <- true (* force save to save keys in wallet *)
    ) config.networks;

  let net = current_network config in
  if !Globals.verbosity > 0 then
    Printf.eprintf "Network: %s\n%!" net.net_name;
  load_wallet config net ;

  if config.version < 1 then begin
    config.version <- 1 ;
    config.modified <- true ;
    List.iter (fun net ->
        load_wallet config net ;
        if net.net_name = "mainnet" then
          net.net_keys <-
            net.net_keys @
            List.filter (fun key ->
                List.for_all
                  (fun k -> k.key_name <> key.key_name ) net.net_keys
              ) mainnet_keys
      ) config.networks
  end;

  begin
    match config.repos with
    | None -> ()
    | Some _ ->
        config.repos <- None ;
        config.modified <- true ;
  end;
  begin
    match config.toolchains with
    | [] ->
        config.toolchains <- [ default_repos ; nil_repos ];
        config.modified <- true ;
    | _ -> ()
  end;
  if config.multisigs = [] then begin
    config.multisigs <- default_multisigs ;
    config.modified <- true
  end;


  EzFile.make_dir ~p:true Misc.temp_dir;
  if config.modified then begin
    save config;
    config.modified <- false;
  end;
  config

let loaded = ref false
let config = lazy (load ())
let config () =
  loaded := true;
  Lazy.force config
let loaded () = !loaded

let print () =
  let config = config () in
  List.iter (fun net ->
      let current = net.net_name = config.current_network in
      Printf.eprintf "* %s%s\n%!" net.net_name
        (if current then " (current)" else "");
      List.iter (fun node ->
          let current_node = node.node_name = net.current_node in
          Printf.eprintf "  - %s%s\n%!" node.node_name
            (if current_node then
               if current then " (current)"
               else " (current if network was selected)"
             else "");
          Printf.eprintf "    url: %s\n%!" node.node_url
        ) net.net_nodes
    ) config.networks

let find_toolchain config toolchain =
  let rec iter toolchains =
    match toolchains with
    | [] ->
        Error.raise "Broken config: no toolchain %S\n%!" toolchain
    | repos :: toolchains ->
        if repos.repo_toolchain = toolchain then
          repos
        else
          iter toolchains
  in
  iter config.toolchains

let toolchain config =
  let net = current_network config in
  find_toolchain config net.net_toolchain
