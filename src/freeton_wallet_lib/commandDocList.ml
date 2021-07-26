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

let cmd =
  EZCMD.sub
    "doc list"
    (fun () ->
       let files = try Sys.readdir Globals.doc_dir with _ -> [||] in
       Printf.printf "%d files in %s:\n%!"
         ( Array.length files ) Globals.doc_dir;
       Array.iter (fun file ->
           Printf.printf "%s\n%!" file) files;
    )
    ~args:
      [
      ]
    ~doc: "List useful documentation files stored in ~/.ft/doc/"
    ~man:[]
