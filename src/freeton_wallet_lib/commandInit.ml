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
  if exists then
    Misc.call [ "git" ; "pull" ];
  Misc.call [ "cargo"; "build" ];

  bin_install "tonos-cli" ;
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
  Misc.call [ "cargo"; "build" ];

  bin_install "target/debug/tvm_linker" ;
  ()

let action ~clean ~client_only () =
  if clean then
    Misc.call [ "rm"; "-rf"; git_dir ];
  EzFile.make_dir ~p:true git_dir ;
  EzFile.make_dir ~p:true bin_dir ;

  install_tonos_cli ();
  if not client_only then begin
    install_solc ();
    install_tvm_linker ();
  end;

 ()

let cmd =
  let clean = ref false in
  let client_only = ref false in

  EZCMD.sub
    "init"
    (fun () -> action
        ~clean:!clean
        ~client_only:!client_only
        ())
    ~args: [
      [ "clean" ], Arg.Set clean,
        EZCMD.info "Clean before building";

      [ "client" ], Arg.Set client_only,
        EZCMD.info "Only build and install the client, not solc&linker";

    ]
    ~doc: "Initialize with TON Labs binary tools"
