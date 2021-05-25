let database = ref (Freeton_crawler_db_versions.Main.database ())
module PGOCaml = PGOCaml_lwt
module M = PGOCaml_lwt.M
type database_handle = (string, bool) Hashtbl.t PGOCaml.t
let connect () = PGOCaml.connect ~database:(!database) ()
let create () = EzPG.createdb (!database)
let (>>=) = M.(>>=)
let connected = ref false
let dbh_pool : database_handle Lwt_pool.t Lazy.t =
  let validate conn =
    (PGOCaml.alive conn) >>=
      (fun is_alive ->
         PGOCaml.debug "[Reader] Validate connection : [%b]\n%!" is_alive;
         M.return is_alive) in
  let check _conn is_ok =
    PGOCaml.debug "[Reader] Check connection.\n%!"; is_ok false in
  let dispose conn =
    PGOCaml.debug "[Reader] Dispose connection.\n%!"; PGOCaml.close conn in
  lazy
    (M.pool_create ~check ~validate ~dispose 20
       (fun () -> connected := true; connect ()))
let (let>>>) _ f =
  M.pool_use (Lazy.force dbh_pool) (fun dbh -> Lwt.bind (Lwt.return dbh) f)
open Lwt_utils
let try_transaction f =
  let>>> dbh = ()
   in
  let> () =
    PGOCaml.bind
      (let dbh = dbh in
       let params : string option list list = [] in
       let split = [`Text "BEGIN"] in
       let i = ref 0 in
       let j = ref 0 in
       let query =
         String.concat ""
           (List.map
              (function
               | `Text text -> text
               | `Var (_varname, false, _) ->
                   let () = incr i in
                   let () = incr j in "$" ^ (string_of_int j.contents)
               | `Var (_varname, true, _) ->
                   let param = List.nth params i.contents in
                   let () = incr i in
                   "(" ^
                     ((String.concat ","
                         (List.map
                            (fun _ ->
                               let () = incr j in
                               "$" ^ (string_of_int j.contents)) param))
                        ^ ")")) split) in
       let params = List.flatten params in
       let name = "ppx_pgsql." ^ (Digest.to_hex (Digest.string query)) in
       let hash =
         try PGOCaml.private_data dbh
         with
         | Not_found ->
             let hash = Hashtbl.create 17 in
             (PGOCaml.set_private_data dbh hash; hash) in
       let is_prepared = Hashtbl.mem hash name in
       PGOCaml.bind
         (if not is_prepared
          then
            PGOCaml.bind (PGOCaml.prepare dbh ~name ~query ())
              (fun () -> Hashtbl.add hash name true; PGOCaml.return ())
          else PGOCaml.return ())
         (fun () -> PGOCaml.execute_rev dbh ~name ~params ()))
      (fun _rows -> PGOCaml.return ())
   in
  Lwt.catch
    (fun () ->
       let> r = f dbh
        in
       let> () =
         PGOCaml.bind
           (let dbh = dbh in
            let params : string option list list = [] in
            let split = [`Text "COMMIT"] in
            let i = ref 0 in
            let j = ref 0 in
            let query =
              String.concat ""
                (List.map
                   (function
                    | `Text text -> text
                    | `Var (_varname, false, _) ->
                        let () = incr i in
                        let () = incr j in "$" ^ (string_of_int j.contents)
                    | `Var (_varname, true, _) ->
                        let param = List.nth params i.contents in
                        let () = incr i in
                        "(" ^
                          ((String.concat ","
                              (List.map
                                 (fun _ ->
                                    let () = incr j in
                                    "$" ^ (string_of_int j.contents)) param))
                             ^ ")")) split) in
            let params = List.flatten params in
            let name = "ppx_pgsql." ^ (Digest.to_hex (Digest.string query)) in
            let hash =
              try PGOCaml.private_data dbh
              with
              | Not_found ->
                  let hash = Hashtbl.create 17 in
                  (PGOCaml.set_private_data dbh hash; hash) in
            let is_prepared = Hashtbl.mem hash name in
            PGOCaml.bind
              (if not is_prepared
               then
                 PGOCaml.bind (PGOCaml.prepare dbh ~name ~query ())
                   (fun () -> Hashtbl.add hash name true; PGOCaml.return ())
               else PGOCaml.return ())
              (fun () -> PGOCaml.execute_rev dbh ~name ~params ()))
           (fun _rows -> PGOCaml.return ())
        in Lwt.return (Ok r))
    (fun exn ->
       let> () =
         PGOCaml.bind
           (let dbh = dbh in
            let params : string option list list = [] in
            let split = [`Text "ROLLBACK"] in
            let i = ref 0 in
            let j = ref 0 in
            let query =
              String.concat ""
                (List.map
                   (function
                    | `Text text -> text
                    | `Var (_varname, false, _) ->
                        let () = incr i in
                        let () = incr j in "$" ^ (string_of_int j.contents)
                    | `Var (_varname, true, _) ->
                        let param = List.nth params i.contents in
                        let () = incr i in
                        "(" ^
                          ((String.concat ","
                              (List.map
                                 (fun _ ->
                                    let () = incr j in
                                    "$" ^ (string_of_int j.contents)) param))
                             ^ ")")) split) in
            let params = List.flatten params in
            let name = "ppx_pgsql." ^ (Digest.to_hex (Digest.string query)) in
            let hash =
              try PGOCaml.private_data dbh
              with
              | Not_found ->
                  let hash = Hashtbl.create 17 in
                  (PGOCaml.set_private_data dbh hash; hash) in
            let is_prepared = Hashtbl.mem hash name in
            PGOCaml.bind
              (if not is_prepared
               then
                 PGOCaml.bind (PGOCaml.prepare dbh ~name ~query ())
                   (fun () -> Hashtbl.add hash name true; PGOCaml.return ())
               else PGOCaml.return ())
              (fun () -> PGOCaml.execute_rev dbh ~name ~params ()))
           (fun _rows -> PGOCaml.return ())
        in Lwt.return (Error exn))
let transaction f =
  let> res = try_transaction f
   in match res with | Ok r -> Lwt.return r | Error exn -> raise exn
let (let>) = (let>)
