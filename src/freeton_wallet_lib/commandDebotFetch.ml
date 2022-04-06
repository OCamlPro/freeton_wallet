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
open Types
open Ezcmd.V2
open EZCMD.TYPES
open EzFile.OP

let fetch config debot =
  Unix.putenv "DEBOT_LOAD_DIR" Globals.debots_dir;
  let cmd = Utils.tonoscli config [ "debot" ; "fetch" ; debot ] in
  Misc.call cmd;
  ()

let post net req =
  let node = Misc.find_node net net.current_node in
  match node with
  | None -> assert false
  | Some node ->
      let url = node.node_url in
      let open Ton_sdk in
      match REQUEST.post_run url req with
      | Ok r -> r
      | Error exn -> raise exn

let action ?debot ?boc ?(use_cached=false) () =
  match debot with
  | None -> Error.raise "You must provide a debot name"
  | Some debot ->
      let config = Config.config () in
      let subst, _ = Subst.subst_string config in
      let debot = subst debot in

      EzFile.make_dir ~p:true Globals.debots_dir;
      let debot =
        match boc with
        | None ->
            let net = Config.current_network config in
            let debot = Utils.address_of_account net debot in
            Misc.raw_address debot
        | Some net_name ->
            match Misc.find_network config net_name with
            | None -> Error.raise "Could not find network %S" net_name
            | Some net ->
                Config.load_wallet config net ;
                let debot = Utils.address_of_account net debot in
                let debot = Misc.raw_address debot in
                let _, addr = EzString.cut_at debot ':' in
                let debot_file =
                  Globals.debots_dir //
                  Printf.sprintf "DEBOT_%s.boc" addr
                in
                if use_cached && Sys.file_exists debot_file then
                  debot
                else
                  match post net
                          (Ton_sdk.REQUEST.account ~level:2 debot) with
                  | [ acc ] ->
                      begin
                        match acc.acc_boc with
                        | None ->
                            Error.raise "Debot not initialized"
                        | Some boc ->
                            Printf.eprintf "Saving debot boc in %s\n%!"
                              debot_file ;
                            EzFile.write_file debot_file boc
                      end;
                      debot
                  | [] -> Error.raise "No account at address %s" debot
                  | _ -> assert false
      in
      fetch config debot

let cmd =
  let debot = ref None in
  let use_cached = ref false in
  let boc_from = ref None in
  EZCMD.sub
    "debot fetch"
    (fun () ->
       action
         ?debot:!debot
         ?boc:!boc_from
         ~use_cached:!use_cached
         ()
    )
    ~args:
      [
        [ "boc-from" ], Arg.String (fun s -> boc_from := Some s),
        EZCMD.info ~docv:"NETWORK"
          "Download debot from NETWORK (patched tonos-cli)";

        [ "cached" ], Arg.Set use_cached,
        EZCMD.info "Use cached version of boc (patched tonos-cli)";

        [],
        Arg.Anon (0, fun name -> debot := Some name),
        EZCMD.info ~docv:"DEBOT" "Address of debot" ;
      ]
    ~doc: "Call tonos-cli fetch DEBOT"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]
