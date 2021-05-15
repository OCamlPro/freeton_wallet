
let database = ref ( Freeton_crawler_db_versions.Main.database () )

module PGOCaml = PGOCaml_lwt
module M = PGOCaml_lwt.M

type database_handle = (string, bool) Hashtbl.t PGOCaml.t

let connect () =
  PGOCaml.connect ~database:!database ()

let create () =
  EzPG.createdb !database

let (>>=) = M.(>>=)
(* let return = M.return *)

let connected = ref false
let dbh_pool : database_handle Lwt_pool.t Lazy.t =
  let validate conn =
    PGOCaml.alive conn >>= fun is_alive ->
    PGOCaml.debug "[Reader] Validate connection : [%b]\n%!" is_alive ;
    M.return is_alive in
  let check _conn is_ok =
    PGOCaml.debug "[Reader] Check connection.\n%!" ;
    is_ok false in
  let dispose conn =
    PGOCaml.debug "[Reader] Dispose connection.\n%!" ;
    PGOCaml.close conn in
  lazy begin
    M.pool_create ~check ~validate ~dispose 20 (fun () ->
        connected := true;
        connect ()
      )
  end

let (let>>>) _ f =
  M.pool_use ( Lazy.force dbh_pool )
    (fun dbh -> Lwt.bind (Lwt.return dbh) f)

open Lwt_utils

let try_transaction(* _no_res *) f =
  let>>> dbh = () in
  let> () = [%pgsql dbh "BEGIN"] in
  Lwt.catch (fun () ->
      let> r = f dbh in
      let> () = [%pgsql dbh "COMMIT"] in
      Lwt.return (Ok r)
    )
    (fun exn ->
       let> () = [%pgsql dbh "ROLLBACK"] in
       Lwt.return (Error exn)
    )

let transaction f =
  let> res = try_transaction f in
  match res with
  | Ok r -> Lwt.return r
  | Error exn -> raise exn

(* let transaction f =
 *   let>>> dbh = () in
 *   let> () = [%pgsql dbh "BEGIN"] in
 *   Lwt.catch (fun () ->
 *       let> r = f dbh in
 *       let> () = match r with
 *         | Ok _ -> [%pgsql dbh "COMMIT"]
 *         | Error _ -> [%pgsql dbh "ROLLBACK"] in
 *       Lwt.return r
 *     )
 *     (fun exn ->
 *        let> () = [%pgsql dbh "ROLLBACK"] in
 *        Format.eprintf "Error in DB transaction: @[%s@]@."
 *          (Printexc.to_string exn);
 *        Error.server_error exn "Error in DB transaction, was rolled back"
 *     ) *)

(*
type pg_error =
  | UniqueViolation of string
  | OtherPGError of string
  | OtherError of exn

let pg_error_of_exn = function
  | PGOCaml.PostgreSQL_Error (_, fields) ->
    let msg = match List.assoc_opt 'M' fields with
      | None -> "Unknown Postgres Error"
      | Some msg -> msg in
    (match List.assoc_opt 'C' fields with
     | Some "23505" (* unique_violation *) ->
       UniqueViolation msg
     | _ -> OtherPGError msg
    )
  | e -> OtherError e
*)

let ( let> ) = ( let> )
