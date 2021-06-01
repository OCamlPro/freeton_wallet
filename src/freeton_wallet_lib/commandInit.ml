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
open EzFile.OP
open EZCMD.TYPES

let git_dir = Globals.ft_dir // "GIT"
let bin_dir = Globals.ft_dir // "bin"

let bin_install file =
  let basename = Filename.basename file in
  Misc.call [ "cp" ; "-f" ; file ; bin_dir // basename ]

let install_tonos_cli () =
  Unix.chdir git_dir ;
  let dir = git_dir // "tonos-cli" in
  let exists = Sys.file_exists dir in
  if not exists then
    Misc.call [ "git" ; "clone"; "https://github.com/tonlabs/tonos-cli.git" ];
  Unix.chdir dir;
  if exists then begin
    Misc.call [ "git" ; "pull" ];
    Misc.call [ "cargo" ; "update" ];
  end;
  Misc.call [ "cargo"; "build" ];

  bin_install "target/debug/tonos-cli" ;
  ()

let install_solc () =
  Unix.chdir git_dir ;
  let dir = git_dir // "TON-Solidity-Compiler" in
  let exists = Sys.file_exists dir in
  if not exists then
    Misc.call [ "git" ; "clone"; "https://github.com/tonlabs/TON-Solidity-Compiler.git" ];
  Unix.chdir dir;
  if exists then
    Misc.call [ "git" ; "pull" ];
  Misc.call [ "sh" ; "./compiler/scripts/install_deps.sh" ];
  EzFile.make_dir ~p:true "build";
  Unix.chdir "build";
  Misc.call [ "cmake" ; "../compiler/" ; "-DCMAKE_BUILD_TYPE=Release" ];
  Misc.call [ "cmake" ; "--build" ; "." ; "--" ; "-j8" ];
  Unix.chdir "..";

  bin_install "build/solc/solc" ;
  bin_install "lib/stdlib_sol.tvm" ;
  ()

let install_tvm_linker () =
  Unix.chdir git_dir ;
  let dir = git_dir // "TVM-linker" in
  let exists = Sys.file_exists dir in
  if not exists then
    Misc.call [ "git" ; "clone"; "https://github.com/tonlabs/TVM-linker.git" ];
  Unix.chdir dir;
  if exists then
    Misc.call [ "git" ; "pull" ];
  Unix.chdir "tvm_linker";
  if exists then
    Misc.call [ "cargo" ; "update" ];
  Misc.call [ "cargo"; "build" ];

  bin_install "target/debug/tvm_linker" ;
  ()

let install_code_hashes () =
  List.iter (fun file ->
      if Filename.dirname file = "contracts" then
        match EzString.split (Filename.basename file) '.' with
        | [ name ; "tvc" ] ->
            ignore ( Misc.get_contract_file ".tvc" name )
        | _ -> ()
    ) Files.file_list;
  ()

let action ~clean ~client ~solc ~linker ~code_hashes =
  if clean then
    Misc.call [ "rm"; "-rf"; git_dir ];
  EzFile.make_dir ~p:true git_dir ;
  EzFile.make_dir ~p:true bin_dir ;

  if client then install_tonos_cli ();
  if solc then install_solc ();
  if linker then install_tvm_linker ();
  if code_hashes then install_code_hashes ();

 ()

let cmd =
  let clean = ref false in
  let client = ref false in
  let solc = ref false in
  let linker = ref false in
  let code_hashes = ref false in

  EZCMD.sub
    "init"
    (fun () ->
       let client, solc, linker, code_hashes =
         match !client, !solc, !linker, !code_hashes with
         | false, false, false, false -> true, true, true, true
         | client, solc, linker, code_hashes ->
             client, solc, linker, code_hashes
       in
       action ~clean:!clean ~client ~solc ~linker ~code_hashes
    )
    ~args: [
      [ "clean" ], Arg.Set clean,
      EZCMD.info "Clean before building";

      [ "client" ], Arg.Set client,
      EZCMD.info "Build and install 'tonos-cli' from sources";
      [ "solc" ], Arg.Set solc,
      EZCMD.info "Build and install 'solc' from sources";
      [ "linker" ], Arg.Set linker,
      EZCMD.info "Build and install 'tvm_linker' from sources";
      [ "code-hashes" ], Arg.Set code_hashes,
      EZCMD.info "Create a database of code hashes from predefined contracts";

    ]
    ~doc: "Initialize with TON Labs binary tools, compiled from sources."
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Initialize with TON Labs binary tools, downloading them \
            from their GIT repositories and compiling them (a recent \
            Rust compiler must be installed).";
        `P "Tools are installed in $HOME/.ft/bin/.";
        `P "The following tools can be installed:";
        `I ("1.", "The 'tonos-cli' client");
        `I ("2.", "The 'solc' client from the TON-Solidity-Compiler repository");
        `I ("3.", "The 'tvm_linker' encoder from the TVM-linker repository");
        `P "If no specific option is specified, all tools are \
            generated. If a tool has already been generated, calling \
            it again will try to upgrade to a more recent version."
      ];
    ]
