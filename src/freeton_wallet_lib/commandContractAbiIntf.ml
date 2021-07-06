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
open EZCMD.TYPES

let intf_abi contract =
  let _config = Config.config () in
  let contract, contract_abi =
    if Sys.file_exists contract then
      let abifile = contract in
      let basename = Filename.basename abifile in
      let contract, _ = EzString.cut_at basename '.' in
      contract, abifile
    else
      contract, Misc.get_contract_abifile contract
  in
  let filename = contract ^ "_intf.sol" in
  let abi = Ton_sdk.ABI.read contract_abi in
  Printf.eprintf "Generating %S\n%!" filename;
  let oc = open_out filename  in
  let ppf = Format.formatter_of_out_channel oc in

  Format.fprintf ppf "pragma ton-solidity >= 0.32.0;@.";

  let open Ton_client.ABI.AbiContract in

  begin
    match abi.header with
    | [] -> () | headers ->
        List.iter (fun header ->
            Format.fprintf ppf "pragma AbiHeader %s;@." header
          ) headers ;
  end;
  Format.fprintf ppf "@.abstract contract %s {@[<1>@.@." contract ;

  begin
    match abi.data with
    | [] -> ()
    | data ->
        List.iter (fun d ->
            Format.fprintf ppf "  %s static %s;@." d.data_type d.data_name
          ) data;
  end;

  let constructors, functions = List.partition (fun f ->
      f.fun_name = "constructor"
    ) abi.functions
  in
  let _variables, functions = List.partition (fun f ->
      match EzString.chop_prefix ~prefix:"g_" f.fun_name with
      | Some _ -> true
      | None -> false
    ) functions
  in

  let fprintf_params params =
    match params with
    | [] -> Format.fprintf ppf "()"
    | _ ->
        Format.fprintf ppf "(@[<1>@ ";
        List.iteri (fun i p ->
            if i > 0 then Format.fprintf ppf ",@ ";
            Format.fprintf ppf "%s@ %s" p.param_type p.param_name
          ) params ;
        Format.fprintf ppf "@ @])"
  in

  let fprintf_functions list =
    match list with
    | [] -> ()
    | _ ->
        List.iter (fun f ->
            Format.fprintf ppf "  %s @[<1>@ "
              ( match f.fun_name with
               | "constructor" -> "constructor"
               | name -> Printf.sprintf "function %s" name );

            fprintf_params f.fun_inputs ;
            Format.fprintf ppf "@ public";
            begin
              match f.fun_outputs with
              | [] -> ()
              | outputs ->
                   Format.fprintf ppf "@ returns@ ";
                   fprintf_params outputs;
            end;
            Format.fprintf ppf " {}";
            Format.fprintf ppf "@]@."
          ) list
  in


  begin
    match abi.events with
    | [] -> ()
    | events ->
        Format.fprintf ppf "@.";
        List.iter (fun ev ->
            Format.fprintf ppf "  event %s " ev.ev_name ;
            fprintf_params ev.ev_inputs ;
            Format.fprintf ppf "@."
          ) events
  end;
  fprintf_functions constructors ;
  fprintf_functions functions ;

  Format.fprintf ppf "@]}@.";
  close_out oc;
  ()



let action ~contract () =
  intf_abi contract

let cmd =
  let contract = ref None in
  EZCMD.sub
    "contract abi intf"
    (fun () ->
       match !contract with
       | None -> Error.raise "You must provide a contract name"
       | Some contract ->
           action ~contract ()
    )
    ~args:
      [
        [], Arg.Anon (0, fun s -> contract := Some s),
        EZCMD.info ~docv:"CONTRACT" "Name of contract to build";

      ]
    ~doc: "Generate an interface from a contract ABI"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command generates an interface from a contract ABI" ;
      ];
    ]
