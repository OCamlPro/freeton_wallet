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

let () =
  EzPGUpdater.main ( Freeton_crawler_db_versions.Main.database ())
    ~downgrades: Freeton_crawler_db_versions.Main.downgrades
    ~upgrades: Freeton_crawler_db_versions.Main.upgrades;

  if Array.length Sys.argv > 1 then
    let filename = Sys.argv.(1) in
    let oc = open_out filename in
    Printf.fprintf oc "let init () = ()\n%!";
    close_out oc
