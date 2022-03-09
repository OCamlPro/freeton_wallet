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
open Ezcmd.V2
open EZCMD.TYPES
open EzFile.OP

let import ~contract ~tvc ~abi ~force ~src =
  let next_version =
    match CommandContractBuild.get_current_version contract with
    | None -> Some 1
    | Some version ->
        let contract = contract // string_of_int version in
        let contract_prefix = Globals.contracts_dir // contract in
        if
          CommandContractBuild.same_file
            (contract_prefix ^ ".tvc")  tvc &&
          CommandContractBuild.same_file
            (contract_prefix ^ ".abi.json") abi then begin
          Printf.eprintf "Contract already known as %s\n%!"
            contract ;
          None
        end else
          Some ( version + 1 )
  in
  begin
    match next_version with
    | None -> ()
    | Some next_version ->
        let known = CommandContractBuild.known_contracts () in
        if not force && StringMap.mem contract known then
          Error.raise "Contract %s already exists (use -f to override)"
            contract;
        let contract_prefix =
          CommandContractBuild.create_new_version contract next_version
        in
        Misc.call [ "cp"; "-f"; abi ; contract_prefix ^ ".abi.json" ];
        let tvc_file = contract_prefix ^ ".tvc" in
        Misc.call [ "cp"; "-f"; tvc ; tvc_file ];
        Misc.register_tvc_file ~tvc_file
          ~contract: ( Misc.fully_qualified_contract contract ) ;
        match src with
        | [] -> ()
        | list ->
            EzFile.make_dir ~p:true contract_prefix;
            List.iter (fun src ->
                Misc.call [ "cp"; "-f"; src ;
                            contract_prefix // Filename.basename src ];
              ) list
  end


let rec action ~force ~filename ~make ?contract ?solidity_version
    ?(prefix="") () =

  if Sys.is_directory filename then
    let files = Sys.readdir filename in
    if make then Error.raise "Cannot --make on a directory";
    Array.iter (fun file ->
        if Filename.check_suffix file ".tvc" then
          let filename = Filename.concat filename file in
          action ~force ~filename ~make ~prefix ()
      ) files
  else
    let dirname = if make then "" else Filename.dirname filename in
    let basename = Filename.basename filename in
    let contract_name, ext = EzString.cut_at basename '.' in
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
        let filename = Filename.concat dirname ( contract_name ^ "." ^ ext) in
        if Sys.file_exists filename then
          kind := filename :: !kind
      ) files;
    let contract = match contract with
      | Some contract -> contract
      | None -> prefix ^ contract_name
    in
    begin
      match !abi, !tvc with
      | [ abi ], [ tvc ] ->
          begin
            let build_file =
              if make then
                if ext = "spp" then
                  let new_filename = contract ^ ".sol" in
                  let tmp_filename = Filename.temp_file contract ".sol" in
                  let files, need_build = CommandContractBuild.preprocess_solidity
                      ~old_file:new_filename
                      ~from_:filename
                      ~to_: tmp_filename
                      ()
                  in
                  let tvc_time = (Unix.lstat tvc).Unix.st_mtime in
                  let need_build = ref need_build in
                  if not !need_build then
                    List.iter (fun file ->
                        let time = try
                            (Unix.lstat file).Unix.st_mtime
                          with _ -> 0.
                        in
                        (* Printf.eprintf "Check %S\n%!" file; *)
                        need_build := !need_build
                                      ||  ( time > tvc_time );
                        (* Printf.eprintf "need_build: %b\n%!" !need_build; *)
                      ) (filename :: files);
                  if !need_build then begin
                    Printf.eprintf "contract: %S\n%!" contract;
                    Sys.rename tmp_filename new_filename;
                    Some new_filename
                  end else begin
                    Sys.remove tmp_filename ;
                    None
                  end else begin
                  Printf.eprintf "Warning: --make on .sol file does not rebuild. Use .spp instead\n%!";
                  None
                end else
                None
            in
            match build_file with
            | None ->
                import ~contract ~abi ~tvc ~src:!src ~force
            | Some filename ->
                CommandContractBuild.action ~filename ~force ~contract
                  ?solidity_version ()
          end
      | _, []
      | [], _
        when make ->
          CommandContractBuild.action ~filename ~force ~contract
            ?solidity_version ()
      | [], _ -> Error.raise "Missing abi file"
      | _, [] -> Error.raise "Missing tvc file"
      | _, [_] -> Error.raise "Ambiguity with abi files (.abi.json/.abi)"
      | _, _ -> Error.raise "Ambiguity with tvc files (.tvc/.tvm)"

    end

let cmd =
  let force = ref false in
  let make = ref false in
  let filename = ref None in
  let contract = ref None in
  let solidity_version = ref "master" in
  let prefix = ref None in
  EZCMD.sub
    "contract import"
    (fun () ->
       match !filename with
       | None -> Error.raise "You must provide the filename of the contract"
       | Some filename ->
           action
             ~force:!force
             ~make:!make
             ~filename
             ?contract:!contract
             ~solidity_version:!solidity_version
             ?prefix:!prefix
             ()
    )
    ~args:
      [

        [], Arg.Anon (0, fun file -> filename := Some file),
        EZCMD.info ~docv:"FILENAME" "Import contract from FILENAME";

        [ "force" ; "f" ], Arg.Set force,
        EZCMD.info "Override existing contracts";

        [ "make" ], Arg.Set make,
        EZCMD.info "Build contract if needed (only for .spp)";

        [ "contract"], Arg.String (fun s -> contract := Some s),
        EZCMD.info ~docv:"CONTRACT" "Name of contract to build";

        [ "prefix"], Arg.String (fun s -> prefix := Some s),
        EZCMD.info ~docv:"STRING" "Prefix all contracts with this prefix";

        [ "solidity-version" ], Arg.String ( fun s -> solidity_version := s ),
        EZCMD.info ~docv:"VERSION" "Version of Solidity to use" ;

      ]
    ~doc: "Import a contract"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command imports a contract into the contract database";
        `P "Example:";
        `Pre {|ft contract import src/Foo.tvm|};
        `P "Import the given contract into the contract database. Two files are mandatory: the ABI file and the TVM file. They should be stored in the same directory. The ABI file must use either a '.abi' or '.abi.json' extension, whereas the TVM file must use either '.tvc' or '.tvm. If a source file (.sol, .cpp, .hpp) is also present, it is copied in the database.";
      ];
    ]
