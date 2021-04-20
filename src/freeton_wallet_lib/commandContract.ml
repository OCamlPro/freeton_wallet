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
  | UseAccount
  | CreateAccount of string
  | ReplaceAccount of string

type todo =
    ListContracts
  | BuildContract of string
  | DeployContract of string
  | ImportContract of string

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

let action ~todo ~force ~sign ~params ~wc ~create =
  match todo with
  | ListContracts ->
      CommandList.list_contracts ()
  | BuildContract filename ->
      (* TODO: check that no account is using this contract,
         otherwise, these accounts will become unreachable, i.e. we
         lose the tvc file and so how to regen their address. *)
      let dirname = Filename.dirname filename in
      let basename = Filename.basename filename in
      let name, ext = EzString.cut_at basename '.' in
      if ext <> "sol" then
        Error.raise "File %s must end with .sol extension" basename;
      let known = CommandList.known_contracts () in
      if not force && StringMap.mem name known then
        Error.raise "Contract %s already exists (use -f to override)" name;
      let solc = Misc.binary_file "solc" in
      (* maybe use argument --tvm-optimize *)
      let tvm_linker = Misc.binary_file "tvm_linker" in
      let stdlib = Misc.binary_file "stdlib_sol.tvm" in

      let abi_file = name ^ ".abi.json" in
      let code_file = name ^ ".code" in
      let tvm_file = name ^ ".tvm" in
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

      Misc.call [ "cp" ; "-f" ; filename ; abi_file ; Globals.contracts_dir ];
      let tvc_file = Globals.contracts_dir // name ^ ".tvc" in
      Misc.call [ "cp" ; "-f" ; tvm_file ; tvc_file ];
      ()

  | ImportContract filename ->
      let dirname = Filename.dirname filename in
      let basename = Filename.basename filename in
      let name, _ext = EzString.cut_at basename '.' in
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
          let filename = Filename.concat dirname (name ^ "." ^ ext) in
          if Sys.file_exists filename then
            kind := filename :: !kind
        ) files;
      begin
        match !abi, !tvc with
        | [ abi ], [ tvc ] ->
            Misc.call [ "cp"; "-f"; abi ;
                        Globals.contracts_dir // name ^ ".abi.json" ];
            Misc.call [ "cp"; "-f"; tvc ;
                        Globals.contracts_dir // name ^ ".tvc" ];
            List.iter (fun src ->
                Misc.call [ "cp"; "-f"; src ;
                            Globals.contracts_dir // Filename.basename src ];
              ) !src
        | [], _ -> Error.raise "Missing abi file"
        | _, [] -> Error.raise "Missing tvc file"
        | _, [_] -> Error.raise "Ambiguity with abi files (.abi.json/.abi)"
        | _, _ -> Error.raise "Ambiguity with tvc files (.tvc/.tvm)"

      end

  | DeployContract contract ->

      let config = Config.config () in
      let net = Config.current_network config in
      let create =
        match create with
        | ReplaceAccount sign ->
            Misc.delete_account config net sign;
            CreateAccount sign
        | UseAccount | CreateAccount _ -> create
      in
      let sign =
        match create with
        | CreateAccount sign ->
            Printf.eprintf "Generating new key\n%!";
            CommandAccount.genkey ~name:sign ~contract:contract config;
            Printf.eprintf "Sending 1 TON from deployer %S\n%!"
              net.net_deployer;
            CommandMultisig.send_transfer
              ~src:net.net_deployer
              ~dst:sign
              ~amount:"1" ();
            Config.save config;
            sign
        | ReplaceAccount _ -> assert false
        | UseAccount ->
            match sign with
            | None -> Error.raise "--deploy CONTRACT requires --sign SIGNER"
            | Some sign -> sign
      in
      let key = Misc.find_key_exn net sign in
      begin
        match key.key_account with
        | Some { acc_contract = Some acc_contract ; _ } ->
            if acc_contract <> contract then
              Error.raise "Wrong contract %S for signer %S" acc_contract sign
        | _ -> ()
      end;
      Subst.with_substituted config params (fun params ->
          Printf.eprintf "Deploying contract %S to %s\n%!" contract sign;
          Utils.deploy_contract config ~key ~contract ~params ~wc ())


let tab = '\t'

let create_interface name =
  let filename = name ^ ".sol" in
  if Sys.file_exists filename then
    Printf.eprintf "Skipping generation of %S: already exists\n%!" filename
  else
    let content =
      Printf.sprintf
        {|/* Interface %s */

pragma ton-solidity ^0.37.0;

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

pragma ton-solidity ^0.37.0;

pragma AbiHeader expire;
pragma AbiHeader pubkey;

import "./I%s.sol";

/*
  Exception codes:
  100 - message sender is not a custodian;
*/
contract %s is I%s {

  uint64 constant EXPIRATION_TIME = 86400; // lifetime is 24 hours

  uint8 g_nvals ;                      // required number of ...
  mapping(uint256 => uint8) g_vals ;   // pubkey -> value_index

  constructor( uint256[] values ) public {
    require( msg.pubkey() == tvm.pubkey(), 100 );
    tvm.accept();
    // TODO
    g_vals[ values[0] ] = 1;
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
%cft contract --build %s.sol
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
  let signer = ref None in
  let params = ref "{}" in
  let wc = ref None in
  let create = ref UseAccount in
  EZCMD.sub
    "contract"
    (fun () ->
       if !has_todo || not !can_skip_todo then
         with_todo (fun todo ->
             action
               ~todo ~force:!force
               ~sign:!signer
               ~params:!params
               ~wc:!wc
               ~create:!create
           )
    )
    ~args:
      [

        [ "new" ] , Arg.String (fun name ->
            can_skip_todo := true;
            create_contract name),
        EZCMD.info "NAME Create template file for contract NAME";

        [ "newi" ] , Arg.String (fun name ->
            can_skip_todo := true ;
            create_interface name),
        EZCMD.info "NAME Create template file for interface NAME";

        [ "list" ], Arg.Unit (fun () -> set_todo "--list" ListContracts ),
        EZCMD.info "List known contracts";

        [ "force" ], Arg.Set force,
        EZCMD.info "Override existing contracts";

        [ "build"], Arg.String (fun filename ->
            set_todo "--build" (BuildContract filename)),
        EZCMD.info "Build a contract and remember it";

        [ "deploy" ], Arg.String (fun contract ->
            set_todo "--deploy" (DeployContract contract)
          ),
        EZCMD.info "CONTRACT Deploy contract CONTRACT";

        [ "import" ], Arg.String (fun contract ->
            set_todo "--import" (ImportContract contract)
          ),
        EZCMD.info "CONTRACT Deploy contract CONTRACT";

        [ "sign" ], Arg.String (fun s ->
            signer := Some s),
        EZCMD.info "ACCOUNT Sign with account ACCOUNT";

        [ "params" ], Arg.String (fun s ->
            params := s),
        EZCMD.info "PARAMS Constructor/call Arguments ({} by default)";

        [ "create" ], Arg.String (fun s -> create := CreateAccount s),
        EZCMD.info "ACCOUNT Create ACCOUNT by deploying contract (with --deploy)";

        [ "replace" ], Arg.String (fun s -> create := ReplaceAccount s),
        EZCMD.info "ACCOUNT Replace ACCOUNT when deploying contract (with --deploy)";

      ]
    ~doc: "Manage contracts"
