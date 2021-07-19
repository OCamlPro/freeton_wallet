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

let handle_exception f x =
  try
    Ok ( f x )
  with
  | (
    Solidity_common.GenericError _
  | Solidity_exceptions.SyntaxError _
  | Solidity_exceptions.TypecheckError _
  )  as exn ->
      Error ( Solidity_exceptions.string_of_exn exn )

let parse_file = Solidity_parser.parse_file ~freeton:true
let typecheck_ast = Solidity_typechecker.type_program
let string_of_ast = Solidity_printer.string_of_program ~freeton:true
