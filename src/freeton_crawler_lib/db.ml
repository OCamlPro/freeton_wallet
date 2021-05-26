open Db_utils
let int32_of_string = Int32.of_string
let string_of_string s = s
let int64_of_string = Int64.of_string
let init = Updated.db_versions_hash
let with_dbh f x = let>>> dbh = ()
                    in f dbh x
module EVENTS :
  sig
    type t = {
      name: string ;
      args: string ;
      time: int64 ;
      tr_lt: int64 }
    val list : ?serial:int32 -> unit -> (int32 * string * t) list Lwt.t
    val add : msg_id:string -> t -> unit Lwt.t
    val mem : msg_id:string -> bool Lwt.t
  end =
  struct
    type t = {
      name: string ;
      args: string ;
      time: int64 ;
      tr_lt: int64 }
    let list ?serial  () =
      let>>> dbh = ()
       in
      let> list =
        match serial with
        | None ->
            PGOCaml.bind
              (let dbh = dbh in
               let params : string option list list = [] in
               let split =
                 [`Text
                    "SELECT * FROM freeton_events ORDER BY serial DESC LIMIT 100"] in
               let i = ref 0 in
               let j = ref 0 in
               let query =
                 String.concat ""
                   (List.map
                      (function
                       | `Text text -> text
                       | `Var (_varname, false, _) ->
                           let () = incr i in
                           let () = incr j in
                           "$" ^ (string_of_int j.contents)
                       | `Var (_varname, true, _) ->
                           let param = List.nth params i.contents in
                           let () = incr i in
                           "(" ^
                             ((String.concat ","
                                 (List.map
                                    (fun _ ->
                                       let () = incr j in
                                       "$" ^ (string_of_int j.contents))
                                    param))
                                ^ ")")) split) in
               let params = List.flatten params in
               let name =
                 "ppx_pgsql." ^ (Digest.to_hex (Digest.string query)) in
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
                      (fun () ->
                         Hashtbl.add hash name true; PGOCaml.return ())
                  else PGOCaml.return ())
                 (fun () -> PGOCaml.execute_rev dbh ~name ~params ()))
              (fun _rows ->
                 PGOCaml.return
                   (let original_query =
                      "SELECT * FROM freeton_events ORDER BY serial DESC LIMIT 100" in
                    List.rev_map
                      (fun row ->
                         match row with
                         | c0::c1::c2::c3::c4::c5::[] ->
                             (((let open PGOCaml in int32_of_string)
                                 (try PGOCaml_aux.Option.get c0
                                  with
                                  | _ ->
                                      failwith
                                        "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                               ((let open PGOCaml in string_of_string)
                                  (try PGOCaml_aux.Option.get c1
                                   with
                                   | _ ->
                                       failwith
                                         "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                               ((let open PGOCaml in string_of_string)
                                  (try PGOCaml_aux.Option.get c2
                                   with
                                   | _ ->
                                       failwith
                                         "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                               ((let open PGOCaml in string_of_string)
                                  (try PGOCaml_aux.Option.get c3
                                   with
                                   | _ ->
                                       failwith
                                         "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                               ((let open PGOCaml in int64_of_string)
                                  (try PGOCaml_aux.Option.get c4
                                   with
                                   | _ ->
                                       failwith
                                         "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                               ((let open PGOCaml in int64_of_string)
                                  (try PGOCaml_aux.Option.get c5
                                   with
                                   | _ ->
                                       failwith
                                         "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")))
                         | _ ->
                             let msg =
                               "ppx_pgsql: internal error: " ^
                                 ("Incorrect number of columns returned from query: "
                                    ^
                                    (original_query ^
                                       (".  Columns are: " ^
                                          (String.concat "; "
                                             (List.map
                                                (function
                                                 | Some str ->
                                                     Printf.sprintf "%S" str
                                                 | None -> "NULL") row))))) in
                             raise (PGOCaml.Error msg)) _rows))
        | Some serial ->
            PGOCaml.bind
              (let dbh = dbh in
               let params : string option list list =
                 [[Some (((let open PGOCaml in string_of_int32)) serial)]] in
               let split =
                 [`Text "SELECT * FROM freeton_events WHERE serial < ";
                 `Var ("serial", false, false);
                 `Text " ORDER BY serial DESC LIMIT 100"] in
               let i = ref 0 in
               let j = ref 0 in
               let query =
                 String.concat ""
                   (List.map
                      (function
                       | `Text text -> text
                       | `Var (_varname, false, _) ->
                           let () = incr i in
                           let () = incr j in
                           "$" ^ (string_of_int j.contents)
                       | `Var (_varname, true, _) ->
                           let param = List.nth params i.contents in
                           let () = incr i in
                           "(" ^
                             ((String.concat ","
                                 (List.map
                                    (fun _ ->
                                       let () = incr j in
                                       "$" ^ (string_of_int j.contents))
                                    param))
                                ^ ")")) split) in
               let params = List.flatten params in
               let name =
                 "ppx_pgsql." ^ (Digest.to_hex (Digest.string query)) in
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
                      (fun () ->
                         Hashtbl.add hash name true; PGOCaml.return ())
                  else PGOCaml.return ())
                 (fun () -> PGOCaml.execute_rev dbh ~name ~params ()))
              (fun _rows ->
                 PGOCaml.return
                   (let original_query =
                      "SELECT * FROM freeton_events WHERE serial < $serial ORDER BY serial DESC LIMIT 100" in
                    List.rev_map
                      (fun row ->
                         match row with
                         | c0::c1::c2::c3::c4::c5::[] ->
                             (((let open PGOCaml in int32_of_string)
                                 (try PGOCaml_aux.Option.get c0
                                  with
                                  | _ ->
                                      failwith
                                        "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                               ((let open PGOCaml in string_of_string)
                                  (try PGOCaml_aux.Option.get c1
                                   with
                                   | _ ->
                                       failwith
                                         "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                               ((let open PGOCaml in string_of_string)
                                  (try PGOCaml_aux.Option.get c2
                                   with
                                   | _ ->
                                       failwith
                                         "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                               ((let open PGOCaml in string_of_string)
                                  (try PGOCaml_aux.Option.get c3
                                   with
                                   | _ ->
                                       failwith
                                         "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                               ((let open PGOCaml in int64_of_string)
                                  (try PGOCaml_aux.Option.get c4
                                   with
                                   | _ ->
                                       failwith
                                         "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                               ((let open PGOCaml in int64_of_string)
                                  (try PGOCaml_aux.Option.get c5
                                   with
                                   | _ ->
                                       failwith
                                         "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")))
                         | _ ->
                             let msg =
                               "ppx_pgsql: internal error: " ^
                                 ("Incorrect number of columns returned from query: "
                                    ^
                                    (original_query ^
                                       (".  Columns are: " ^
                                          (String.concat "; "
                                             (List.map
                                                (function
                                                 | Some str ->
                                                     Printf.sprintf "%S" str
                                                 | None -> "NULL") row))))) in
                             raise (PGOCaml.Error msg)) _rows))
       in
      let list = List.rev list in
      let list =
        List.rev_map
          (fun (serial, msg_id, name, args, time, tr_lt) ->
             (serial, msg_id, { name; args; time; tr_lt })) list in
      Lwt.return list
    let add ~msg_id  event =
      let>>> dbh = ()
       in
      PGOCaml.bind
        (let dbh = dbh in
         let params : string option list list =
           [[Some (((let open PGOCaml in string_of_string)) msg_id)];
           [Some (((let open PGOCaml in string_of_string)) event.name)];
           [Some (((let open PGOCaml in string_of_string)) event.args)];
           [Some (((let open PGOCaml in string_of_int64)) event.time)];
           [Some (((let open PGOCaml in string_of_int64)) event.tr_lt)]] in
         let split =
           [`Text
              "insert into freeton_events (msg_id, event_name, event_args, time, tr_lt ) values (";
           `Var ("{msg_id}", false, false);
           `Text ", ";
           `Var ("{event.name}", false, false);
           `Text ", ";
           `Var ("{event.args}", false, false);
           `Text ", ";
           `Var ("{event.time}", false, false);
           `Text ", ";
           `Var ("{event.tr_lt}", false, false);
           `Text " )"] in
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
    let mem ~msg_id  =
      let>>> dbh = ()
       in
      let> res =
        PGOCaml.bind
          (let dbh = dbh in
           let params : string option list list =
             [[Some (((let open PGOCaml in string_of_string)) msg_id)]] in
           let split =
             [`Text "select msg_id from freeton_events where msg_id = ";
             `Var ("{msg_id}", false, false)] in
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
          (fun _rows ->
             PGOCaml.return
               (let original_query =
                  "select msg_id from freeton_events where msg_id = ${msg_id}" in
                List.rev_map
                  (fun row ->
                     match row with
                     | c0::[] ->
                         (let open PGOCaml in string_of_string)
                           (try PGOCaml_aux.Option.get c0
                            with
                            | _ ->
                                failwith
                                  "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")
                     | _ ->
                         let msg =
                           "ppx_pgsql: internal error: " ^
                             ("Incorrect number of columns returned from query: "
                                ^
                                (original_query ^
                                   (".  Columns are: " ^
                                      (String.concat "; "
                                         (List.map
                                            (function
                                             | Some str ->
                                                 Printf.sprintf "%S" str
                                             | None -> "NULL") row))))) in
                         raise (PGOCaml.Error msg)) _rows))
       in match res with | [] -> Lwt.return false | _ -> Lwt.return true
  end 
module TRANSACTIONS :
  sig
    type t =
      {
      tr_lt: int64 ;
      tr_id: string ;
      block_id: string ;
      json: string option }
    val add :
      tr_lt:int64 ->
        tr_id:string -> block_id:string -> json:string -> unit Lwt.t
    val list : ?before_lt:int64 -> ?json:bool -> unit -> t list Lwt.t
  end =
  struct
    type t =
      {
      tr_lt: int64 ;
      tr_id: string ;
      block_id: string ;
      json: string option }
    let add ~tr_lt  ~tr_id  ~block_id  ~json  =
      let>>> dbh = ()
       in
      PGOCaml.bind
        (let dbh = dbh in
         let params : string option list list =
           [[Some (((let open PGOCaml in string_of_int64)) tr_lt)];
           [Some (((let open PGOCaml in string_of_string)) tr_id)];
           [Some (((let open PGOCaml in string_of_string)) block_id)];
           [Some (((let open PGOCaml in string_of_string)) json)]] in
         let split =
           [`Text
              "insert into freeton_transactions (tr_lt, tr_id, block_id, json) values (";
           `Var ("tr_lt", false, false);
           `Text ", ";
           `Var ("tr_id", false, false);
           `Text ", ";
           `Var ("block_id", false, false);
           `Text ", ";
           `Var ("json", false, false);
           `Text ")"] in
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
    let list ?before_lt  ?(json= false)  () =
      let>>> dbh = ()
       in
      match json with
      | true ->
          let> list =
            (match before_lt with
             | Some tr_lt ->
                 PGOCaml.bind
                   (let dbh = dbh in
                    let params : string option list list =
                      [[Some (((let open PGOCaml in string_of_int64)) tr_lt)]] in
                    let split =
                      [`Text
                         "SELECT * FROM freeton_transactions WHERE tr_lt < ";
                      `Var ("tr_lt", false, false);
                      `Text " ORDER BY tr_lt DESC LIMIT 100"] in
                    let i = ref 0 in
                    let j = ref 0 in
                    let query =
                      String.concat ""
                        (List.map
                           (function
                            | `Text text -> text
                            | `Var (_varname, false, _) ->
                                let () = incr i in
                                let () = incr j in
                                "$" ^ (string_of_int j.contents)
                            | `Var (_varname, true, _) ->
                                let param = List.nth params i.contents in
                                let () = incr i in
                                "(" ^
                                  ((String.concat ","
                                      (List.map
                                         (fun _ ->
                                            let () = incr j in
                                            "$" ^ (string_of_int j.contents))
                                         param))
                                     ^ ")")) split) in
                    let params = List.flatten params in
                    let name =
                      "ppx_pgsql." ^ (Digest.to_hex (Digest.string query)) in
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
                           (fun () ->
                              Hashtbl.add hash name true; PGOCaml.return ())
                       else PGOCaml.return ())
                      (fun () -> PGOCaml.execute_rev dbh ~name ~params ()))
                   (fun _rows ->
                      PGOCaml.return
                        (let original_query =
                           "SELECT * FROM freeton_transactions WHERE tr_lt < $tr_lt ORDER BY tr_lt DESC LIMIT 100" in
                         List.rev_map
                           (fun row ->
                              match row with
                              | c0::c1::c2::c3::[] ->
                                  (((let open PGOCaml in int64_of_string)
                                      (try PGOCaml_aux.Option.get c0
                                       with
                                       | _ ->
                                           failwith
                                             "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                                    ((let open PGOCaml in string_of_string)
                                       (try PGOCaml_aux.Option.get c1
                                        with
                                        | _ ->
                                            failwith
                                              "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                                    ((let open PGOCaml in string_of_string)
                                       (try PGOCaml_aux.Option.get c2
                                        with
                                        | _ ->
                                            failwith
                                              "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                                    ((let open PGOCaml in string_of_string)
                                       (try PGOCaml_aux.Option.get c3
                                        with
                                        | _ ->
                                            failwith
                                              "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")))
                              | _ ->
                                  let msg =
                                    "ppx_pgsql: internal error: " ^
                                      ("Incorrect number of columns returned from query: "
                                         ^
                                         (original_query ^
                                            (".  Columns are: " ^
                                               (String.concat "; "
                                                  (List.map
                                                     (function
                                                      | Some str ->
                                                          Printf.sprintf "%S"
                                                            str
                                                      | None -> "NULL") row))))) in
                                  raise (PGOCaml.Error msg)) _rows))
             | None ->
                 PGOCaml.bind
                   (let dbh = dbh in
                    let params : string option list list = [] in
                    let split =
                      [`Text
                         "SELECT * FROM freeton_transactions ORDER BY tr_lt DESC LIMIT 100"] in
                    let i = ref 0 in
                    let j = ref 0 in
                    let query =
                      String.concat ""
                        (List.map
                           (function
                            | `Text text -> text
                            | `Var (_varname, false, _) ->
                                let () = incr i in
                                let () = incr j in
                                "$" ^ (string_of_int j.contents)
                            | `Var (_varname, true, _) ->
                                let param = List.nth params i.contents in
                                let () = incr i in
                                "(" ^
                                  ((String.concat ","
                                      (List.map
                                         (fun _ ->
                                            let () = incr j in
                                            "$" ^ (string_of_int j.contents))
                                         param))
                                     ^ ")")) split) in
                    let params = List.flatten params in
                    let name =
                      "ppx_pgsql." ^ (Digest.to_hex (Digest.string query)) in
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
                           (fun () ->
                              Hashtbl.add hash name true; PGOCaml.return ())
                       else PGOCaml.return ())
                      (fun () -> PGOCaml.execute_rev dbh ~name ~params ()))
                   (fun _rows ->
                      PGOCaml.return
                        (let original_query =
                           "SELECT * FROM freeton_transactions ORDER BY tr_lt DESC LIMIT 100" in
                         List.rev_map
                           (fun row ->
                              match row with
                              | c0::c1::c2::c3::[] ->
                                  (((let open PGOCaml in int64_of_string)
                                      (try PGOCaml_aux.Option.get c0
                                       with
                                       | _ ->
                                           failwith
                                             "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                                    ((let open PGOCaml in string_of_string)
                                       (try PGOCaml_aux.Option.get c1
                                        with
                                        | _ ->
                                            failwith
                                              "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                                    ((let open PGOCaml in string_of_string)
                                       (try PGOCaml_aux.Option.get c2
                                        with
                                        | _ ->
                                            failwith
                                              "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                                    ((let open PGOCaml in string_of_string)
                                       (try PGOCaml_aux.Option.get c3
                                        with
                                        | _ ->
                                            failwith
                                              "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")))
                              | _ ->
                                  let msg =
                                    "ppx_pgsql: internal error: " ^
                                      ("Incorrect number of columns returned from query: "
                                         ^
                                         (original_query ^
                                            (".  Columns are: " ^
                                               (String.concat "; "
                                                  (List.map
                                                     (function
                                                      | Some str ->
                                                          Printf.sprintf "%S"
                                                            str
                                                      | None -> "NULL") row))))) in
                                  raise (PGOCaml.Error msg)) _rows)))
           in
          let list =
            List.rev_map
              (fun (tr_lt, tr_id, block_id, json) ->
                 { tr_lt; tr_id; block_id; json = (Some json) })
              (List.rev list) in
          Lwt.return list
      | false ->
          let> list =
            (match before_lt with
             | Some tr_lt ->
                 PGOCaml.bind
                   (let dbh = dbh in
                    let params : string option list list =
                      [[Some (((let open PGOCaml in string_of_int64)) tr_lt)]] in
                    let split =
                      [`Text
                         "SELECT tr_lt, tr_id, block_id FROM freeton_transactions WHERE tr_lt < ";
                      `Var ("tr_lt", false, false);
                      `Text " ORDER BY tr_lt DESC LIMIT 100"] in
                    let i = ref 0 in
                    let j = ref 0 in
                    let query =
                      String.concat ""
                        (List.map
                           (function
                            | `Text text -> text
                            | `Var (_varname, false, _) ->
                                let () = incr i in
                                let () = incr j in
                                "$" ^ (string_of_int j.contents)
                            | `Var (_varname, true, _) ->
                                let param = List.nth params i.contents in
                                let () = incr i in
                                "(" ^
                                  ((String.concat ","
                                      (List.map
                                         (fun _ ->
                                            let () = incr j in
                                            "$" ^ (string_of_int j.contents))
                                         param))
                                     ^ ")")) split) in
                    let params = List.flatten params in
                    let name =
                      "ppx_pgsql." ^ (Digest.to_hex (Digest.string query)) in
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
                           (fun () ->
                              Hashtbl.add hash name true; PGOCaml.return ())
                       else PGOCaml.return ())
                      (fun () -> PGOCaml.execute_rev dbh ~name ~params ()))
                   (fun _rows ->
                      PGOCaml.return
                        (let original_query =
                           "SELECT tr_lt, tr_id, block_id FROM freeton_transactions WHERE tr_lt < $tr_lt ORDER BY tr_lt DESC LIMIT 100" in
                         List.rev_map
                           (fun row ->
                              match row with
                              | c0::c1::c2::[] ->
                                  (((let open PGOCaml in int64_of_string)
                                      (try PGOCaml_aux.Option.get c0
                                       with
                                       | _ ->
                                           failwith
                                             "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                                    ((let open PGOCaml in string_of_string)
                                       (try PGOCaml_aux.Option.get c1
                                        with
                                        | _ ->
                                            failwith
                                              "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                                    ((let open PGOCaml in string_of_string)
                                       (try PGOCaml_aux.Option.get c2
                                        with
                                        | _ ->
                                            failwith
                                              "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")))
                              | _ ->
                                  let msg =
                                    "ppx_pgsql: internal error: " ^
                                      ("Incorrect number of columns returned from query: "
                                         ^
                                         (original_query ^
                                            (".  Columns are: " ^
                                               (String.concat "; "
                                                  (List.map
                                                     (function
                                                      | Some str ->
                                                          Printf.sprintf "%S"
                                                            str
                                                      | None -> "NULL") row))))) in
                                  raise (PGOCaml.Error msg)) _rows))
             | None ->
                 PGOCaml.bind
                   (let dbh = dbh in
                    let params : string option list list = [] in
                    let split =
                      [`Text
                         "SELECT tr_lt, tr_id, block_id FROM freeton_transactions ORDER BY tr_lt DESC LIMIT 100"] in
                    let i = ref 0 in
                    let j = ref 0 in
                    let query =
                      String.concat ""
                        (List.map
                           (function
                            | `Text text -> text
                            | `Var (_varname, false, _) ->
                                let () = incr i in
                                let () = incr j in
                                "$" ^ (string_of_int j.contents)
                            | `Var (_varname, true, _) ->
                                let param = List.nth params i.contents in
                                let () = incr i in
                                "(" ^
                                  ((String.concat ","
                                      (List.map
                                         (fun _ ->
                                            let () = incr j in
                                            "$" ^ (string_of_int j.contents))
                                         param))
                                     ^ ")")) split) in
                    let params = List.flatten params in
                    let name =
                      "ppx_pgsql." ^ (Digest.to_hex (Digest.string query)) in
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
                           (fun () ->
                              Hashtbl.add hash name true; PGOCaml.return ())
                       else PGOCaml.return ())
                      (fun () -> PGOCaml.execute_rev dbh ~name ~params ()))
                   (fun _rows ->
                      PGOCaml.return
                        (let original_query =
                           "SELECT tr_lt, tr_id, block_id FROM freeton_transactions ORDER BY tr_lt DESC LIMIT 100" in
                         List.rev_map
                           (fun row ->
                              match row with
                              | c0::c1::c2::[] ->
                                  (((let open PGOCaml in int64_of_string)
                                      (try PGOCaml_aux.Option.get c0
                                       with
                                       | _ ->
                                           failwith
                                             "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                                    ((let open PGOCaml in string_of_string)
                                       (try PGOCaml_aux.Option.get c1
                                        with
                                        | _ ->
                                            failwith
                                              "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")),
                                    ((let open PGOCaml in string_of_string)
                                       (try PGOCaml_aux.Option.get c2
                                        with
                                        | _ ->
                                            failwith
                                              "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")))
                              | _ ->
                                  let msg =
                                    "ppx_pgsql: internal error: " ^
                                      ("Incorrect number of columns returned from query: "
                                         ^
                                         (original_query ^
                                            (".  Columns are: " ^
                                               (String.concat "; "
                                                  (List.map
                                                     (function
                                                      | Some str ->
                                                          Printf.sprintf "%S"
                                                            str
                                                      | None -> "NULL") row))))) in
                                  raise (PGOCaml.Error msg)) _rows)))
           in
          let list =
            List.rev_map
              (fun (tr_lt, tr_id, block_id) ->
                 { tr_lt; tr_id; block_id; json = None }) (List.rev list) in
          Lwt.return list
  end 
module PIDS :
  sig
    val get : string -> int option Lwt.t
    val set : string -> int -> unit Lwt.t
  end =
  struct
    let get name =
      let>>> dbh = ()
       in
      let> res =
        PGOCaml.bind
          (let dbh = dbh in
           let params : string option list list =
             [[Some (((let open PGOCaml in string_of_string)) name)]] in
           let split =
             [`Text "SELECT pid FROM pids WHERE name = ";
             `Var ("name", false, false)] in
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
          (fun _rows ->
             PGOCaml.return
               (let original_query =
                  "SELECT pid FROM pids WHERE name = $name" in
                List.rev_map
                  (fun row ->
                     match row with
                     | c0::[] ->
                         (let open PGOCaml in int32_of_string)
                           (try PGOCaml_aux.Option.get c0
                            with
                            | _ ->
                                failwith
                                  "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\"")
                     | _ ->
                         let msg =
                           "ppx_pgsql: internal error: " ^
                             ("Incorrect number of columns returned from query: "
                                ^
                                (original_query ^
                                   (".  Columns are: " ^
                                      (String.concat "; "
                                         (List.map
                                            (function
                                             | Some str ->
                                                 Printf.sprintf "%S" str
                                             | None -> "NULL") row))))) in
                         raise (PGOCaml.Error msg)) _rows))
       in
      match res with
      | [] -> Lwt.return_none
      | pid::[] -> Lwt.return_some (Int32.to_int pid)
      | _ -> assert false
    let set name pid =
      let pid = Int32.of_int pid in
      let> old_pid = get name
       in
      match old_pid with
      | None ->
          let>>> dbh = ()
           in
          PGOCaml.bind
            (let dbh = dbh in
             let params : string option list list =
               [[Some (((let open PGOCaml in string_of_string)) name)];
               [Some (((let open PGOCaml in string_of_int32)) pid)]] in
             let split =
               [`Text "INSERT INTO pids (name, pid) VALUES (";
               `Var ("name", false, false);
               `Text ", ";
               `Var ("pid", false, false);
               `Text ")"] in
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
      | Some _pid ->
          let>>> dbh = ()
           in
          PGOCaml.bind
            (let dbh = dbh in
             let params : string option list list =
               [[Some (((let open PGOCaml in string_of_int32)) pid)];
               [Some (((let open PGOCaml in string_of_string)) name)]] in
             let split =
               [`Text "UPDATE pids SET pid = ";
               `Var ("pid", false, false);
               `Text " WHERE name = ";
               `Var ("name", false, false)] in
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
  end 
