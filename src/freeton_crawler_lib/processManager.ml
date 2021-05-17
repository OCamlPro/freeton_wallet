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

open Db_utils

let kill pid signal =
  try Unix.kill pid signal with _ -> ()

let get_status ~kind =
  (* Printf.eprintf "get_status %S\n%!" kind; *)
  let> pid = Db.PIDS.get kind in
  match pid with
  | None -> Lwt.return_none
  | Some pid ->
      match Unix.kill pid 0 with
      | exception _ ->
          Lwt.return_some (pid, false)
      | _ ->
          Lwt.return_some (pid, true)

let status ~name ~kind =
  let> res = get_status ~kind in
  begin
    match res with
    | None ->
        Printf.eprintf "Process %s %s not yet started\n%!" name kind
    | Some (pid, running) ->
        if running then
          Printf.eprintf "Process %s %s is running with pid %d\n%!"
            name kind pid
        else
          Printf.eprintf "Process %s %s is stopped ( former pid %d )\n%!"
            name kind pid
  end;
  Lwt.return_unit

let stop ~name ~kind =
  let> pid = Db.PIDS.get kind in
  begin
    match pid with
    | None -> ()
    | Some pid ->
        Printf.eprintf "Stopping worker for %s (pid:%d): %s\n%!"
          name pid kind ;
        kill pid Sys.sigint;
  end;
  Lwt.return_unit

let status name =
  let> () = status ~name ~kind:"monitor" in
  let> () = status ~name ~kind:"worker" in
  Lwt.return_unit

let stop name =

  let> () = stop ~name ~kind:"monitor" in
  let> () = stop ~name ~kind:"worker" in

  status name

let start name cmd =
  let> res = get_status ~kind:"monitor" in
  match res with
  | Some ( pid, true ) ->
      Printf.eprintf
        "Manager process for %s is already running with pid %d\n%!"
        name pid;
      exit 2
  | None
  | Some _ ->
      let> res = get_status ~kind:"worker" in
      match res with
      | Some ( pid, true ) ->
          Printf.eprintf
            "Worker process for %s is already running with pid %d\n%!"
            name pid;
          exit 2
      | None
      | Some _ ->
          let> () = Db.PIDS.set "monitor" ( Unix.getpid() ) in
          let worker = ref ( Unix.getpid () ) in

          Sys.set_signal Sys.sigint (Sys.Signal_handle (fun signal ->
              kill !worker signal;
              exit 1
            ));

          let string_of_status = function
            | Unix.WEXITED n -> Printf.sprintf "exited %d" n
            | Unix.WSIGNALED n -> Printf.sprintf "signaled %d" n
            | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n
          in
          let rec iter () =

            let> res = get_status ~kind:"worker" in
            match res with
            | Some (pid, true) ->
                let> res = Lwt_unix.waitpid [] pid in
                begin
                  match res with
                  | exception exn ->
                      Printf.eprintf
                        "Exception: %s\n%!" (Printexc.to_string exn);
                      exit 2
                  | ( pid, status ) ->
                      Printf.eprintf
                        "monitor with pid %d died with status %s\n%!" pid
                        (string_of_status status);
                      iter ()
                end
            | _ ->
                let log = Printf.sprintf "%s.log" name in
                let oc = open_out_gen
                    [ Open_wronly ; Open_append ; Open_creat ] 0o644 log
                in
                let fd = Unix.descr_of_out_channel oc in
                Printf.eprintf "Starting process %s\n%!" name;
                let pid = Unix.create_process
                    cmd.(0) cmd
                    Unix.stdin fd fd
                in
                close_out oc;
                worker := pid;
                let> () = Lwt_unix.sleep 30.0 in
                iter ()
          in
          iter ()

let set_worker name =
  let> res = get_status ~kind:"worker" in
  match res with
  | Some ( pid, true ) ->
      Printf.eprintf "Worker process for %s is already running as pid %d\n%!"
        name pid ;
      exit 2
  | _ ->
      Db.PIDS.set "worker" ( Unix.getpid() )
