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

(*
include MLwt
*)

let (let>) p f = Lwt.bind p f
    (*
let (let>!) p f = Lwt.map f p
let (let@) p f = Lwt.bind p (function Error _ as e -> return e | Ok x -> f x)
let (let@!) p f = Lwt.map (function Error _ as e -> e | Ok x -> Ok (f x)) p
let (let>+) p f = Lwt.bind p (fun x -> return @@ Ok (f x))
let (let|>) r f = Lwt.bind (return r) (function Error _ as e -> return e | Ok x -> f x)
let (let|>!) r f = Lwt.map (function Error _ as e -> e | Ok x -> Ok (f x)) (return r)

let (let|) r f = match r with Error _ as e -> e | Ok x -> f x
let (let|!) r f = match r with Error _ as e -> e | Ok x -> Ok (f x)

let return_u = return_unit

let filter_map_res_p f l =
  Lwt_list.filter_map_p (fun x ->
      f x >|= function
      | Ok (Some r) -> Some (Ok r)
      | Ok None -> None
      | Error e -> Some (Error e)
    ) l
  >|= fun l_res ->
  List.fold_left (fun acc op -> match acc, op with
      | Error _, _ -> acc
      | _, Error e -> Error e
      | Ok acc, Ok op -> Ok (op :: acc)
    ) (Ok []) l_res
  |> function
  | Ok l -> Ok (List.rev l)
  | Error _ as e -> e

module Res = struct

  module Infix = struct

    let ( >>? ) v f = match v with Error _ as err -> err | Ok v -> f v

    let ( >>=? ) v f =
      v >>= function Error _ as err -> Lwt.return err | Ok v -> f v

    let ( >>|? ) v f = v >>=? fun v -> Lwt.return_ok (f v)

    let ( >|= ) = Lwt.( >|= )

    let ( >|? ) v f = v >>? fun v -> Ok (f v)

  end

  include Infix

  let return v = Lwt.return_ok v

  let return_unit = Lwt.return (Ok ())

  let return_none = Lwt.return (Ok None)

  let return_some x = Lwt.return_ok (Some x)

  let return_nil = Lwt.return (Ok [])

  let return_true = Lwt.return (Ok true)

  let return_false = Lwt.return (Ok false)

  let error s = Error s

  let combine_errors e1 _e2 = e1

  let rec map_s f l =
    match l with
    | [] ->
      return_nil
    | h :: t ->
      f h >>=? fun rh -> map_s f t >>=? fun rt -> return (rh :: rt)

  let mapi_s f l =
    let rec mapi_s f i l =
      match l with
      | [] ->
        return_nil
      | h :: t ->
        f i h
        >>=? fun rh -> mapi_s f (i + 1) t >>=? fun rt -> return (rh :: rt)
    in
    mapi_s f 0 l

  let rec rev_map_append_s acc f = function
    | [] ->
      return acc
    | hd :: tl ->
      f hd >>=? fun v -> rev_map_append_s (v :: acc) f tl

  let rev_map_s f l = rev_map_append_s [] f l

  let rec map_p f l =
    match l with
    | [] ->
      return_nil
    | x :: l -> (
        let tx = f x and tl = map_p f l in
        tx
        >>= fun x ->
        tl
        >>= fun l ->
        match (x, l) with
        | (Ok x, Ok l) ->
          Lwt.return_ok (x :: l)
        | (Error exn1, Error exn2) ->
          Lwt.return_error (combine_errors exn1 exn2)
        | (Ok _, Error exn) | (Error exn, Ok _) ->
          Lwt.return_error exn )

  let mapi_p f l =
    let rec mapi_p f i l =
      match l with
      | [] ->
        return_nil
      | x :: l -> (
          let tx = f i x and tl = mapi_p f (i + 1) l in
          tx
          >>= fun x ->
          tl
          >>= fun l ->
          match (x, l) with
          | (Ok x, Ok l) ->
            Lwt.return_ok (x :: l)
          | (Error exn1, Error exn2) ->
            Lwt.return_error (combine_errors exn1 exn2)
          | (Ok _, Error exn) | (Error exn, Ok _) ->
            Lwt.return_error exn )
    in
    mapi_p f 0 l

  let rec map2_s f l1 l2 =
    match (l1, l2) with
    | ([], []) ->
      return_nil
    | (_ :: _, []) | ([], _ :: _) ->
      invalid_arg "Error_monad.map2_s"
    | (h1 :: t1, h2 :: t2) ->
      f h1 h2 >>=? fun rh -> map2_s f t1 t2 >>=? fun rt -> return (rh :: rt)

  let mapi2_s f l1 l2 =
    let rec mapi2_s i f l1 l2 =
      match (l1, l2) with
      | ([], []) ->
        return_nil
      | (_ :: _, []) | ([], _ :: _) ->
        invalid_arg "Error_monad.mapi2_s"
      | (h1 :: t1, h2 :: t2) ->
        f i h1 h2
        >>=? fun rh ->
        mapi2_s (i + 1) f t1 t2 >>=? fun rt -> return (rh :: rt)
    in
    mapi2_s 0 f l1 l2

  let rec map2 f l1 l2 =
    match (l1, l2) with
    | ([], []) ->
      Ok []
    | (_ :: _, []) | ([], _ :: _) ->
      invalid_arg "Error_monad.map2"
    | (h1 :: t1, h2 :: t2) ->
      f h1 h2 >>? fun rh -> map2 f t1 t2 >>? fun rt -> Ok (rh :: rt)

  let rec filter_map_s f l =
    match l with
    | [] ->
      return_nil
    | h :: t -> (
        f h
        >>=? function
        | None ->
          filter_map_s f t
        | Some rh ->
          filter_map_s f t >>=? fun rt -> return (rh :: rt) )

  let rec filter_map_p f l =
    match l with
    | [] ->
      return_nil
    | h :: t -> (
        let th = f h and tt = filter_map_p f t in
        th
        >>=? function
        | None -> tt | Some rh -> tt >>=? fun rt -> return (rh :: rt) )

  let rec filter_s f l =
    match l with
    | [] ->
      return_nil
    | h :: t -> (
        f h
        >>=? function
        | false ->
          filter_s f t
        | true ->
          filter_s f t >>=? fun t -> return (h :: t) )

  let rec filter_p f l =
    match l with
    | [] ->
      return_nil
    | h :: t -> (
        let jh = f h and t = filter_p f t in
        jh >>=? function false -> t | true -> t >>=? fun t -> return (h :: t) )

  let rec iter_s f l =
    match l with [] -> return_unit | h :: t -> f h >>=? fun () -> iter_s f t

  let iteri_s f l =
    let rec iteri_s i f l =
      match l with
      | [] -> return_unit
      | h :: t -> f i h >>=? fun () -> iteri_s (i + 1) f t in
    iteri_s 0 f l

  let rec iter_p f l =
    match l with
    | [] ->
      return_unit
    | x :: l -> (
        let tx = f x and tl = iter_p f l in
        tx
        >>= fun tx_res ->
        tl
        >>= fun tl_res ->
        match (tx_res, tl_res) with
        | (Ok (), Ok ()) ->
          Lwt.return_ok ()
        | (Error exn1, Error exn2) ->
          Lwt.return_error (combine_errors exn1 exn2)
        | (Ok (), Error exn) | (Error exn, Ok ()) ->
          Lwt.return_error exn )

  let iteri_p f l =
    let rec iteri_p i f l =
      match l with
      | [] ->
        return_unit
      | x :: l -> (
          let tx = f i x and tl = iteri_p (i + 1) f l in
          tx
          >>= fun tx_res ->
          tl
          >>= fun tl_res ->
          match (tx_res, tl_res) with
          | (Ok (), Ok ()) ->
            Lwt.return (Ok ())
          | (Error exn1, Error exn2) ->
            Lwt.return_error (combine_errors exn1 exn2)
          | (Ok (), Error exn) | (Error exn, Ok ()) ->
            Lwt.return (Error exn) )
    in
    iteri_p 0 f l

  let rec iter2_p f l1 l2 =
    match (l1, l2) with
    | ([], []) ->
      return_unit
    | ([], _) | (_, []) ->
      invalid_arg "Error_monad.iter2_p"
    | (x1 :: l1, x2 :: l2) -> (
        let tx = f x1 x2 and tl = iter2_p f l1 l2 in
        tx
        >>= fun tx_res ->
        tl
        >>= fun tl_res ->
        match (tx_res, tl_res) with
        | (Ok (), Ok ()) ->
          Lwt.return_ok ()
        | (Error exn1, Error exn2) ->
          Lwt.return_error (combine_errors exn1 exn2)
        | (Ok (), Error exn) | (Error exn, Ok ()) ->
          Lwt.return_error exn )

  let iteri2_p f l1 l2 =
    let rec iteri2_p i f l1 l2 =
      match (l1, l2) with
      | ([], []) ->
        return_unit
      | ([], _) | (_, []) ->
        invalid_arg "Error_monad.iteri2_p"
      | (x1 :: l1, x2 :: l2) -> (
          let tx = f i x1 x2 and tl = iteri2_p (i + 1) f l1 l2 in
          tx
          >>= fun tx_res ->
          tl
          >>= fun tl_res ->
          match (tx_res, tl_res) with
          | (Ok (), Ok ()) ->
            Lwt.return_ok ()
          | (Error exn1, Error exn2) ->
            Lwt.return_error (combine_errors exn1 exn2)
          | (Ok (), Error exn) | (Error exn, Ok ()) ->
            Lwt.return_error exn )
    in
    iteri2_p 0 f l1 l2

  let rec fold_left_s f init l =
    match l with
    | [] ->
      return init
    | h :: t ->
      f init h >>=? fun acc -> fold_left_s f acc t

  let rec fold_right_s f l init =
    match l with
    | [] ->
      return init
    | h :: t ->
      fold_right_s f t init >>=? fun acc -> f h acc

  let rec join = function
    | [] ->
      return_unit
    | t :: ts -> (
        t
        >>= function
        | Error _ as err ->
          join ts >>=? fun () -> Lwt.return err
        | Ok () ->
          join ts )
end

let ( let* ) o f = match o with
  | None -> None
  | Some x -> f x

let (let*!) o f = match o with
  | None -> None
  | Some x -> Some (f x)

let some x = Some x
*)

let debug flag fmt =
  if flag then Format.eprintf fmt
  else Format.ifprintf Format.err_formatter fmt

(*
let () =
  Random.self_init ()
  *)
