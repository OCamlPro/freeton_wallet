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

open EzCompat (* for StringSet *)
open Ezcmd.V2
open EZCMD.TYPES
open EzFile.OP
open Types

type create =
  | UseAccount of string
  | CreateAccount of string

type todo =
    ListContracts
  | BuildContract of string
  | DeployContract of string
  | ImportContract of string
  | ShowABI of string
  | SolABI of string

let remove_files dirname files =
  List.iter (fun file ->
      if Sys.file_exists file then
        Sys.remove file
    ) ( files @ List.map (fun file -> dirname // file) files)

let check_exists dirname file =
  if Sys.file_exists file then
    file
  else
    let file = dirname // file in
    if Sys.file_exists file then
      file
    else
      Error.raise "File %s was not generated" file

let sol_abi contract =
  let _config = Config.config () in
  let contract_abi = Misc.get_contract_abifile contract in
  let abi = Ton_sdk.ABI.read contract_abi in
  let filename = contract ^ "_impl.sol" in
  Printf.eprintf "Generating %S\n%!" filename;
  let oc = open_out filename  in
  let ppf = Format.formatter_of_out_channel oc in

  Format.fprintf ppf "pragma ton-solidity >= 0.32.0;@.";

  let open Ton_sdk.TYPES.ABI in

  begin
    match abi.header with
    | [] -> () | headers ->
        Format.fprintf ppf "@.// Headers:@.";
        List.iter (fun header ->
            Format.fprintf ppf "pragma AbiHeader %s;@." header
          ) headers ;
  end;
  Format.fprintf ppf "@.contract %s {@[<1>@.@." contract ;

  begin
    match abi.data with
    | [] -> ()
    | data ->
        List.iter (fun d ->
            Format.fprintf ppf "  %s static %s;@." d.data_type d.data_name
          ) data;
  end;

  let constructors, functions = List.partition (fun f ->
      f.fun_name = "constructor"
    ) abi.functions
  in
  let variables, functions = List.partition (fun f ->
      match EzString.chop_prefix ~prefix:"g_" f.fun_name with
      | Some _ -> true
      | None -> false
    ) functions
  in

  let fprintf_params params =
    match params with
    | [] -> Format.fprintf ppf "()"
    | _ ->
        Format.fprintf ppf "(@[<1>@ ";
        List.iteri (fun i p ->
            if i > 0 then Format.fprintf ppf ",@ ";
            Format.fprintf ppf "%s@ %s" p.param_type p.param_name
          ) params ;
        Format.fprintf ppf "@ @])"
  in

  let fprintf_functions list =
    match list with
    | [] -> ()
    | _ ->
        List.iter (fun f ->
            let is_viewer =
              match EzString.chop_prefix f.fun_name ~prefix:"get" with
              | Some "" -> true
              | None -> false
              | Some x -> Char.uppercase x.[0] = x.[0]
            in
            Format.fprintf ppf "@.  %s @[<1>@ "
              ( match f.fun_name with
               | "constructor" -> "constructor"
               | name -> Printf.sprintf "function %s" name );

            fprintf_params f.fun_inputs ;
            Format.fprintf ppf "@ public";
            if is_viewer then
              Format.fprintf ppf "@ view";
            begin
              match f.fun_outputs with
              | [] -> ()
              | outputs ->
                   Format.fprintf ppf "@ returns@ ";
                   fprintf_params outputs;
            end;
            Format.fprintf ppf "@.  {@.";
            if not is_viewer then begin
              Format.fprintf ppf
                "    // require( tvm.pubkey() == msg.pubkey, EXN_AUTH_FAILED)@.";
              Format.fprintf ppf "    // tvm.accept()@.";
            end;
            Format.fprintf ppf "    // TOOO@.";

            Format.fprintf ppf "  }@.";
            Format.fprintf ppf "@]@."
          ) list
  in


  begin
    match abi.events with
    | [] -> ()
    | events ->
        Format.fprintf ppf "@.";
        List.iter (fun ev ->
            Format.fprintf ppf "  event %s " ev.ev_name ;
            fprintf_params ev.ev_inputs ;
            Format.fprintf ppf "@."
          ) events
  end;

  begin
    match variables with
    | [] -> ()
    | _ ->
        Format.fprintf ppf "@.";
        List.iter (fun f ->
            Format.fprintf ppf "  %s public %s;@."
              ( match f.fun_outputs with
                  [ p ] -> p.param_type
                | _ -> "// UNKNOWN " ) f.fun_name
          ) variables ;
  end;
  fprintf_functions constructors ;
  fprintf_functions functions ;

  Format.fprintf ppf "@]}@.";
  close_out oc;
  ()

let create_new_version contract =
  let contract_dir = Globals.contracts_dir // contract in
  let version_file = contract_dir // "CURRENT" in
  let num =
    if Sys.file_exists contract_dir then
      let num = EzFile.read_file version_file in
      let num = int_of_string ( String.trim num ) in
      let num = num + 1 in
      string_of_int num
    else
      let () = EzFile.make_dir ~p:true contract_dir in
      "1"
  in
  EzFile.write_file version_file num;
  contract_dir // num

let action ~todo ~force ~params ~wc ?create ?sign ~deployer () =
  match todo with
  | ListContracts -> CommandList.list_contracts ()
  | ShowABI contract ->
      let s = Utils.show_abi ~contract in
      Printf.printf "%s\n%!" s
  | SolABI contract -> sol_abi contract
  | BuildContract filename ->
      (* TODO: check that no account is using this contract,
         otherwise, these accounts will become unreachable, i.e. we
         lose the tvc file and so how to regen their address. *)
      let dirname = Filename.dirname filename in
      let basename = Filename.basename filename in
      let contract, ext = EzString.cut_at basename '.' in
      if ext <> "sol" then
        Error.raise "File %s must end with .sol extension" basename;
      let known = CommandList.known_contracts () in
      if not force && StringMap.mem contract known then
        Error.raise "Contract %s already exists (use -f to override)"
          contract;
      let solc = Misc.binary_file "solc" in
      (* maybe use argument --tvm-optimize *)
      let tvm_linker = Misc.binary_file "tvm_linker" in
      let stdlib = Misc.binary_file "stdlib_sol.tvm" in

      let abi_file = contract ^ ".abi.json" in
      let code_file = contract ^ ".code" in
      let tvm_file = contract ^ ".tvm" in
      remove_files dirname [ abi_file ; code_file ; tvm_file ];
      Misc.call [ solc ; filename ];
      let abi_file = check_exists dirname abi_file in
      let code_file = check_exists dirname code_file in
      Misc.call [ tvm_linker ; "compile" ; "-o" ; tvm_file ;
                  code_file ;
                  "--abi-json" ; abi_file ;
                  "--lib" ; stdlib
                ];
      let tvm_file = check_exists dirname tvm_file in

      let contract_prefix = create_new_version contract in

      Misc.call [ "cp" ; "-f" ; abi_file ;
                  contract_prefix ^ ".abi.json" ];
      let tvc_file = contract_prefix ^ ".tvc" in
      Misc.call [ "cp" ; "-f" ; tvm_file ; tvc_file ];
      Misc.register_tvc_file ~tvc_file
        ~contract: ( Misc.fully_qualified_contract contract ) ;
      Misc.call [ "cp" ; "-f" ; filename ;
                  contract_prefix ^ ".sol" ];
      ()

  | ImportContract filename ->
      let dirname = Filename.dirname filename in
      let basename = Filename.basename filename in
      let contract, _ext = EzString.cut_at basename '.' in
      let abi = ref [] in
      let tvc = ref [] in
      let src = ref [] in
      let files = [
        abi, "abi.json";
        abi, "abi";
        tvc, "tvm";
        tvc, "tvc";
        src, "sol";
        src, "cpp";
        src, "hpp";
      ] in
      List.iter (fun (kind, ext) ->
          let filename = Filename.concat dirname ( contract ^ "." ^ ext) in
          if Sys.file_exists filename then
            kind := filename :: !kind
        ) files;
      begin
        match !abi, !tvc with
        | [ abi ], [ tvc ] ->
            let contract_prefix = create_new_version contract in
            Misc.call [ "cp"; "-f"; abi ; contract_prefix ^ ".abi.json" ];
            let tvc_file = contract_prefix ^ ".tvc" in
            Misc.call [ "cp"; "-f"; tvc ; tvc_file ];
            Misc.register_tvc_file ~tvc_file
              ~contract: ( Misc.fully_qualified_contract contract ) ;
            begin match !src with
              | [] -> ()
              | list ->
                  EzFile.make_dir ~p:true contract_prefix;
                  List.iter (fun src ->
                      Misc.call [ "cp"; "-f"; src ;
                                  contract_prefix // Filename.basename src ];
                    ) list
            end
        | [], _ -> Error.raise "Missing abi file"
        | _, [] -> Error.raise "Missing tvc file"
        | _, [_] -> Error.raise "Ambiguity with abi files (.abi.json/.abi)"
        | _, _ -> Error.raise "Ambiguity with tvc files (.tvc/.tvm)"

      end

  | DeployContract contract ->

      let config = Config.config () in
      let net = Config.current_network config in
      let create =
        match create, sign with
        | None, _ -> CreateAccount contract
        | Some ( UseAccount _ ), Some _ ->
            Error.raise "--dst and --sign cannot be used together"
        | Some ( UseAccount dst ), None -> UseAccount dst
        | Some ( CreateAccount dst ), _ -> CreateAccount dst
      in
      let contract = Misc.fully_qualified_contract contract in
      let dst, sign =
        match create with
        | CreateAccount dst ->

            if Misc.key_exists net dst then begin
              if force then
                Misc.delete_account config net dst
              else
                Error.raise "Key %s already exists. Use -f to override" dst
            end;

            Printf.eprintf "Generating new key %S\n%!" dst;
            let sign =
              match sign with
              | None ->
                  CommandAccount.genkey ~name:dst ~contract config;
                  None
              | Some sign ->
                  let sign = Misc.find_key_exn net sign in
                  let key_pair = match sign.key_pair with
                    | None ->
                        Error.raise "--sign KEY where KEY has no key pair"
                    | Some key_pair -> key_pair
                  in
                  let acc_address =
                    CommandAccount.gen_address config key_pair contract ~wc:None
                  in
                  let key_account = Some {
                      acc_address ;
                      acc_contract = Some contract ;
                      acc_workchain = None;
                    } in
                  let key = { key_name = dst ;
                              key_account = key_account ;
                              key_passphrase = None ;
                              key_pair = None } in
                  net.net_keys <- key :: net.net_keys ;
                  Some sign
            in
            let deployer = match deployer with
              | None -> net.net_deployer
              | Some deployer -> deployer
            in
            Printf.eprintf "Sending 1 TON from deployer %S\n%!" deployer;
            CommandMultisig.send_transfer
              ~account:deployer
              ~dst
              ~amount:"1" ();
            Config.save config;
            dst, sign
        | UseAccount dst -> dst, None
      in
      let key = Misc.find_key_exn net dst in
      begin
        match key.key_account with
        | Some { acc_contract = Some acc_contract ; _ } ->
            if acc_contract <> contract then
              Error.raise "Wrong contract %S for dest %S" acc_contract dst
        | _ -> ()
      end;
      let sign = match sign with
        | None -> key
        | Some sign -> sign
      in
      Subst.with_substituted config params (fun params ->
          Printf.eprintf "Deploying contract %S to %s\n%!" contract dst;
          Utils.deploy_contract config ~key ~sign ~contract ~params ~wc ())

let tab = '\t'

let create_interface name =
  let filename = name ^ ".sol" in
  if Sys.file_exists filename then
    Printf.eprintf "Skipping generation of %S: already exists\n%!" filename
  else
    let content =
      Printf.sprintf
        {|/* Interface %s */

pragma ton-solidity >= 0.32.0;

interface %s {

  /* some functions like:
  function do_something( uint256 arg ) external ;
  */
}
|} name name
    in
    EzFile.write_file filename content;
    Printf.eprintf "Interface %S generated\n%!" filename

let create_contract name =

  create_interface ("I" ^ name);

  let filename = name ^ ".sol" in
  if Sys.file_exists filename then
    Printf.eprintf "Skipping generation of %S: already exists\n%!" filename
  else
    let content =
      Printf.sprintf {|/*
  Implementation of contract %s
 */

pragma ton-solidity >= 0.32.0;

pragma AbiHeader expire;
pragma AbiHeader pubkey;

import "./I%s.sol";

contract %s is I%s {

  // 100 - message sender is not a custodian;
  uint64 constant EXN_AUTH_FAILED = 100 ;

  uint8 g_nvals ;                      // required number of ...
  mapping(uint256 => uint8) g_vals ;   // pubkey -> value_index

  constructor( ) public {
    require( msg.pubkey() == tvm.pubkey(), EXN_AUTH_FAILED );
    tvm.accept();
    // TODO
    g_vals[ 0 ] = 1;
  }

}

|}
        name name name name in
    EzFile.write_file filename content;
    Printf.eprintf "Contract %S generated\n%!" filename;

    let content =
      if Sys.file_exists "Makefile" then
        EzFile.read_file "Makefile"
      else
        Printf.sprintf
          {|# Auto-generated by ft contract --new

all: contracts
INTERFACES=I*.sol

clean:
%crm -f *~ *.code *.tvm
|} tab
    in
    let content =
      Printf.sprintf {|%s

contracts::%s.code

%s.code: %s.sol $(INTERFACES)
%cft contract --build %s.sol -f
|} content
        name name name tab name
    in
    EzFile.write_file "Makefile" content;
    Printf.eprintf "File Makefile updated\n%!";

    if not ( Sys.file_exists ".gitignore" ) then begin
      EzFile.write_file ".gitignore"
        {|*~
*.code

# Remove these lines when you want to keep the tvm files
*.tvm
*.abi.json
|};
      Printf.eprintf "File .gitignore created\n%!";
    end


let cmd =
  let set_todo, with_todo = Misc.todo_arg () in
  let has_todo = ref false in
  let set_todo todo =
    has_todo := true;
    set_todo todo
  in
  let can_skip_todo = ref false in
  let force = ref false in
  let params = ref "{}" in
  let wc = ref None in
  let create = ref None in
  let deployer = ref None in
  let sign = ref None in
  EZCMD.sub
    "contract"
    (fun () ->
       if !has_todo || not !can_skip_todo then
         with_todo (fun todo ->
             action
               ~todo ~force:!force
               ~params:!params
               ~wc:!wc
               ?create:!create
               ?sign:!sign
               ~deployer:!deployer
               ()
           )
    )
    ~args:
      [

        [ "new" ] , Arg.String (fun name ->
            can_skip_todo := true;
            create_contract name),
        EZCMD.info ~docv:"NAME" "Create template file for contract NAME";

        [ "newi" ] , Arg.String (fun name ->
            can_skip_todo := true ;
            create_interface name),
        EZCMD.info ~docv:"NAME" "Create template file for interface NAME";

        [ "list" ], Arg.Unit (fun () ->
            set_todo "--list" ListContracts ),
        EZCMD.info "List known contracts";

        [ "force" ; "f" ], Arg.Set force,
        EZCMD.info "Override existing contracts";

        [ "build"], Arg.String (fun filename ->
            set_todo "--build" (BuildContract filename)),
        EZCMD.info ~docv:"FILENAME" "Build a contract and remember it";

        [ "deploy" ], Arg.String (fun contract ->
            set_todo "--deploy" (DeployContract contract)
          ),
        EZCMD.info ~docv:"CONTRACT" "Deploy contract CONTRACT";

        [ "import" ], Arg.String (fun contract ->
            set_todo "--import" (ImportContract contract)
          ),
        EZCMD.info ~docv:"CONTRACT" "Deploy contract CONTRACT";

        [ "show-abi" ], Arg.String (fun contract ->
            set_todo "--show-abi" (ShowABI contract)
          ),
        EZCMD.info ~docv:"CONTRACT" "Show ABI of contract CONTRACT";

        [ "sol-abi" ], Arg.String (fun contract ->
            Globals.verbosity := 0;
            set_todo "--sol-abi" (SolABI contract)
          ),
        EZCMD.info ~docv:"CONTRACT"
          "Output ABI of contract CONTRACT as Solidity ";

        [ "dst" ], Arg.String (fun s -> create := Some (UseAccount s) ),
        EZCMD.info ~docv:"ACCOUNT"
          "Deploy to this account, using the existing keypair";

        [ "sign" ], Arg.String (fun s -> sign := Some s),
        EZCMD.info ~docv:"ACCOUNT" "Deploy using this keypair";

        [ "deployer" ], Arg.String (fun s -> deployer := Some s),
        EZCMD.info ~docv:"ACCOUNT"
          "Deployer is this account (pays creation fees)";

        [ "params" ], Arg.String (fun s ->
            params := s),
        EZCMD.info ~docv:"PARAMS" "Constructor/call Arguments ({} by default)";

        [ "create" ], Arg.String (fun s -> create := Some (CreateAccount s) ),
        EZCMD.info ~docv:"ACCOUNT"
          "Create ACCOUNT by deploying contract (with --deploy)";

        [ "replace" ], Arg.String (fun s ->
            create := Some (CreateAccount s) ; force := true ),
        EZCMD.info ~docv:"ACCOUNT"
          "Replace ACCOUNT when deploying contract (with --deploy)";

      ]
    ~doc: "Manage contracts"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command can perform the following actions:";
        `I ("1.", "Build a Solidity contract and store it in the contract database");
        `I ("2.", "List known contracts in the contract database");
        `I ("3.", "Import a contract into the contract database");
        `I ("4.", "Deploy a known contract to the blockchain");
      ];
      `S "BUILD A CONTRACT";
      `Blocks [
        `P "Example:";
        `Pre {|ft contract --build Foobar.sol|};
        `P "After this command, the contract will be known as 'Foobar' in the contract database";
      ];
      `S "LIST KNOWN CONTRACTS";
      `Blocks [
        `P "Example:";
        `Pre {|ft contract --list|};
        `P "List all known contracts: embedded contracts are contracts that are natively known by 'ft', other contracts are stored in $HOME/.ft/contracts, and were either built or imported by 'ft'.";
      ];
      `S "IMPORT A CONTRACT";
      `Blocks [
        `P "Example:";
        `Pre {|ft contract --import src/Foo.tvm|};
        `P "Import the given contract into the contract database. Two files are mandatory: the ABI file and the TVM file. They should be stored in the same directory. The ABI file must use either a '.abi' or '.abi.json' extension, whereas the TVM file must use either '.tvc' or '.tvm. If a source file (.sol, .cpp, .hpp) is also present, it is copied in the database.";
      ];
      `S "DEPLOY A CONTRACT";
      `Blocks [
        `P "Examples:";
        `Pre {|ft contract --deploy Forbar|};
        `P "Create an account 'Foorbar', deploy a contract 'Foobar' to it.";
        `Pre {|ft contract --deploy Forbar --create foo|};
        `P "Create an account 'foo', deploy a contract 'Foobar' to it.";
        `Pre {|ft contract --deploy Forbar --replace foo|};
        `P "Delete account 'foo', recreate it and deploy a contract 'Foobar' to it.";
        `Pre {|ft contract --deploy Forbar --create foo --sign admin|};
        `P "Create an empty account 'foo', deploy a contract 'Foobar' to it, using the keypair from 'admin'.";
        `Pre {|ft contract --deploy Forbar --dst foo|};
        `P "Deploy a contract 'Foobar' an existing account 'foo' using its keypair.";
        `P "";
        `P "With --create and --replace, 1 TON is transferred to the initial account using a 'deployer' multisig account. The deployer account can either be set switch wide (ft config --deployer 'account') or in the deploy command (using the --deployer 'account' argument)";
      ];
    ]
