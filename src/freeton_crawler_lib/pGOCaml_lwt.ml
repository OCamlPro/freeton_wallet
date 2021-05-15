
let verbose_mode = ref false
let verbose_counter = ref 0

let debug fmt = Lwt_utils.debug !verbose_mode fmt

module Monad_lwt = struct
  include Lwt

  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel

  let output_char = Lwt_io.write_char
  let output_string = Lwt_io.write
  let flush = Lwt_io.flush
  let input_char = Lwt_io.read_char
  let really_input = Lwt_io.read_into_exactly
  let close_in = Lwt_io.close

  (* type 'a pool = 'a Lwt_pool.t *)
  let pool_create = Lwt_pool.create
  let pool_use = Lwt_pool.use

  let open_connection sockaddr =
    let sock = Lwt_unix.socket
        (Unix.domain_of_sockaddr sockaddr) Lwt_unix.SOCK_STREAM 0 in
    Lwt.catch
    (fun () ->
      Lwt_unix.connect sock sockaddr >>=
      (fun () ->
         Lwt_unix.set_close_on_exec sock;
         return (Lwt_io.of_fd ~mode:Lwt_io.input sock,
                 Lwt_io.of_fd ~mode:Lwt_io.output sock)
      )
    )
    (fun exn -> Lwt_unix.close sock >>= fun () -> fail exn)

  let output_binary_int oc n =
    output_char oc (Char.chr (n lsr 24)) >>= fun () ->
    output_char oc (Char.chr ((n lsr 16) land 255)) >>= fun () ->
    output_char oc (Char.chr ((n lsr 8) land 255)) >>= fun () ->
    output_char oc (Char.chr (n land 255))

  let input_binary_int ic =
    input_char ic >>= fun a ->
    input_char ic >>= fun b ->
    input_char ic >>= fun c ->
    input_char ic >>= fun d ->
    return ((Char.code a lsl 24)
      lor (Char.code b lsl 16)
      lor (Char.code c lsl 8)
      lor (Char.code d))

end
module M = Monad_lwt

  include PGOCaml_generic.Make(M)

  let prepare dbh ~name ~query () =
    if !verbose_mode then
      Printf.eprintf "DB %S PREPARE %s\n%!" name query;
    prepare dbh ~name ~query ()

  let execute_rev dbh ~name ~params () =
    if !verbose_mode then begin
      incr verbose_counter;
      let counter = !verbose_counter in
      Printf.eprintf "DB x%dx begin %s\n%!" counter name;
      bind (execute_rev dbh ~name ~params ())
        (fun rows ->
           Printf.eprintf "DB x%dx end %s\n%!" counter name;
           return rows)
    end else
      execute_rev dbh ~name ~params ()

  (*
  let string_of_jsonb e = Ezjsonm.value_to_string e
  let jsonb_of_string s = Ezjsonm.value_from_string s
  let string_of_bytea b = string_of_bytea (Bytes.to_string b)
  let bytea_of_string s = bytea_of_string s |> Bytes.of_string
*)
