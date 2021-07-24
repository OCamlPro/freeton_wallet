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


let known_contracts () =
  let contracts = ref StringMap.empty in
  List.iter (fun file ->
      if Filename.dirname file = "contracts" then
        match EzString.split (Filename.basename file) '.' with
        | [ name ; "abi" ; "json" ] ->
            contracts := StringMap.add name "(embedded)" !contracts
        | _ -> ()
    ) Files.file_list;

  Array.iter (fun file ->
      match EzString.split (Filename.basename file) '.' with
      | [ name ; "tvc" ] ->
          contracts := StringMap.add name
              ( Globals.contracts_dir // file ) !contracts
      | [ name ] ->
          let dir = Globals.contracts_dir // name in
          if Sys.is_directory dir then
            let current = EzFile.read_file ( dir // "CURRENT" )
                          |> String.trim in
            let filename = dir // current ^ ".tvc" in
            if not ( Sys.file_exists filename ) then
              Printf.eprintf "Warning: %s does not exist\n%!" filename
            else
              contracts := StringMap.add name
                  ( Printf.sprintf "%s (version %s)" dir current )
                  !contracts
      | _ -> ()
    ) (try Sys.readdir Globals.contracts_dir with _ -> [||]);
  !contracts

let list_contracts () =
  let set = known_contracts () in
  Printf.printf "Known contracts:\n";
  StringMap.iter  (fun name s ->
      Printf.printf "* %s %s\n" name s) set;
  Printf.printf "%!"


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



let same_file f1 f2 =
  EzFile.read_file f1 = EzFile.read_file f2

let get_current_version contract =
  let contract_dir = Globals.contracts_dir // contract in
  let version_file = contract_dir // "CURRENT" in
  if Sys.file_exists contract_dir then
    let num = EzFile.read_file version_file in
    Some ( int_of_string ( String.trim num ) )
  else
    None

let create_new_version contract num =
  let num = string_of_int num in
  let contract_dir = Globals.contracts_dir // contract in
  let version_file = contract_dir // "CURRENT" in
  EzFile.make_dir ~p:true contract_dir ;
  EzFile.write_file version_file num;
  contract_dir // num


let preprocess_solidity ~from_ ~to_ =
  let files = ref [] in
  Subst.with_subst (fun preprocess ->
      match FreetonSolidity.handle_exception (fun file ->
          (* Solidity_lexer.recursive_comments := true ; *)
          let ast = FreetonSolidity.parse_file ~preprocess file in
          files := List.map (fun m ->
              m.Solidity_ast.module_file) ast.program_modules;
          let tast = FreetonSolidity.typecheck_ast ast in
          let s = FreetonSolidity.string_of_ast tast in
          Printf.sprintf
            {|// This file was generated from file %S. DO NOT EDIT !
pragma ton-solidity >= 0.32.0;

pragma AbiHeader expire;
pragma AbiHeader pubkey;
%s
|} file s
        ) from_
      with
      | Ok content ->
          EzFile.write_file to_ content
      | Error s ->
          Printf.eprintf "Error: %s\n%!" s;
          exit 2
    );
  !files

let action ~filename ~force ?contract () =
  (* TODO: check that no account is using this contract,
     otherwise, these accounts will become unreachable, i.e. we
     lose the tvc file and so how to regen their address. *)
  let contract_name = contract in
  let dirname = Filename.dirname filename in
  let basename = Filename.basename filename in
  let contract, ext = EzString.cut_at basename '.' in
  let filename, contract_name =
    match String.lowercase_ascii ext with
    | "sol" -> filename, contract_name
    | "spp" | "solpp" ->
        let new_filename = contract ^ ".sol" in
        let _files: string list =
          preprocess_solidity ~from_:filename ~to_:new_filename
        in
        new_filename,
        ( match contract_name with
          | None -> Some contract
          | _ -> contract_name )
    | _ ->
        Error.raise "File %s must end with .sol extension" basename
  in
  let known = known_contracts () in
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
  Misc.call ( match contract_name with
      | Some contract -> [ solc ; "--contract"; contract; filename ]
      | None -> [ solc ; filename ] );
  let abi_file = check_exists dirname abi_file in
  let code_file = check_exists dirname code_file in
  Misc.call [ tvm_linker ; "compile" ; "-o" ; tvm_file ;
              code_file ;
              "--abi-json" ; abi_file ;
              "--lib" ; stdlib
            ];
  let tvm_file = check_exists dirname tvm_file in

  let next_version =
    match get_current_version contract with
    | None -> Some 1
    | Some version ->
        let contract_dir = Globals.contracts_dir // contract in
        let contract_prefix = contract_dir // string_of_int version in

        if
          same_file (contract_prefix ^ ".tvc")  tvm_file &&
          same_file (contract_prefix ^ ".abi.json") abi_file then begin
          Printf.eprintf "Contract already known as %s/%d\n%!" contract version;
          None
        end else
          Some ( version + 1 )
  in
  begin
    match next_version with
    | None -> ()
    | Some next_version ->
        let contract_prefix = create_new_version contract next_version in
        Misc.call [ "cp" ; "-f" ; abi_file ;
                    contract_prefix ^ ".abi.json" ];
        let tvc_file = contract_prefix ^ ".tvc" in
        Misc.call [ "cp" ; "-f" ; tvm_file ; tvc_file ];
        Misc.register_tvc_file ~tvc_file
          ~contract: ( Misc.fully_qualified_contract contract ) ;
        Misc.call [ "cp" ; "-f" ; filename ;
                    contract_prefix ^ ".sol" ];
        ()
  end


let cmd =
  let force = ref false in
  let contract = ref None in
  let filename = ref None in
  EZCMD.sub
    "contract build"
    (fun () ->
       match !filename with
       | None ->
           Error.raise "You must provide the contract file to build";
       | Some filename ->
             action
               ~filename
               ~force:!force
               ?contract:!contract
               ()

    )
    ~args:
      [

        [ "force" ; "f" ], Arg.Set force,
        EZCMD.info "Override existing contracts";

        [], Arg.Anon (0, fun file -> filename := Some file),
        EZCMD.info ~docv:"FILENAME" "Build this contract and remember it";

        [ "contract"], Arg.String (fun s -> contract := Some s),
        EZCMD.info ~docv:"CONTRACT" "Name of contract to build";

      ]
    ~doc: "Build a contract"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command builds a Solidity contract and store it in the contract database";
        `P "Example:";
        `Pre {|ft contract build Foobar.sol|};
        `P "After this command, the contract will be known as 'Foobar' in the contract database";
      ];
    ]
