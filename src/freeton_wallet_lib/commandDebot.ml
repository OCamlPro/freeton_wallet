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
open EzFile.OP

type todo =
  | New of string

let action todo =
  match todo with
  | New name ->
      let basename = Filename.chop_suffix name "Debot" in
      if Sys.file_exists name then
        Error.raise "Directory %S already exists" name ;
      let dir = Sys.getcwd () in
      let config = Config.config () in
      let toolchain = Config.toolchain config in
      let git_dir = Globals.git_dir ~toolchain in
      EzFile.make_dir ~p:true git_dir ;
      let srcdir = git_dir // "debots" in
      let exists = Sys.file_exists srcdir in
      if not exists then begin
        Unix.chdir git_dir ;
        Misc.call [ "git" ; "clone"; "https://github.com/tonlabs/debots.git" ];
      end ;
      Unix.chdir srcdir;
      if exists then
        Misc.call [ "git" ; "pull" ];
      let files = Sys.readdir srcdir in
      let dstdir = dir // name in
      let libdir = dstdir // "lib" in
      EzFile.make_dir ~p:true libdir ;
      Array.iter ( fun file ->
          if Filename.check_suffix file ".sol" then
            Misc.call [ "cp"; "-f"; srcdir // file ; libdir // file ]
        ) files ;
      Misc.call [ "cp"; "-f"; srcdir // "helloworld/hellodebot.png" ;
                  dstdir // ( name ^ ".png" ) ] ;

      let brace () s =
        match s with
          "name" -> basename
        | _ -> assert false
      in
      Unix.chdir dir;
      List.iter (fun ( file, dest ) ->
          let content = match Files.read ( "debot" // file ) with
            | None -> assert false
            | Some s -> s in
          let file = name // dest in
          EzFile.write_file (file ^ ".tmp") content ;
          let content =
            Ez_subst.V1.EZ_SUBST.string
              ~escape:(ref false) ~sep:'!' ~brace ~ctxt:() content
          in
          EzFile.write_file file content ;
          Printf.eprintf "File %S generated\n%!" file ;
        )
        [
          "Makefile", "Makefile" ;
          "Template.sol", name ^ ".spp" ;
          "StdMethods.sol", "lib/StdMethods.sol" ;
          "Utility.sol", "lib/Utility.sol" ;
          "cpp.sol", "lib/cpp.sol" ;
        ] ;

      ()

let cmd =
  let set_todo, with_todo = Misc.todo_arg () in
  EZCMD.sub
    "debot"
    (fun () ->
       with_todo (fun todo -> action todo)
    )
    ~args:
      [
        [ "new" ] , Arg.String (fun name ->
            match Filename.check_suffix name "Debot" with
            | true ->
                set_todo "--new" (New name)
            | false ->
                Error.raise "Argument of --new should end with 'Debot'"
          ),
        EZCMD.info ~docv:"NAME" "Create template files for debot NAME";

      ]
    ~doc: "Manage debots"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
      ];
    ]
