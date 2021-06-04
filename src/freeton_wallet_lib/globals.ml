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

open EzFile.OP

let verbosity = ref 1
let command = "ft"
let about = "ft COMMAND COMMAND-OPTIONS"


let homedir = Sys.getenv "HOME"
let use_ton_sdk = match Sys.getenv "FT_USE_TONOS" with
  | exception Not_found -> true
  | "no" -> true
  | _ -> false

let ft_dir = homedir // ".ft"
let config_file = ft_dir // "config.json"

let contracts_dir = ft_dir // "contracts"
let code_hash_dir = ft_dir // "code_hash"

let git_dir = ft_dir // "GIT"
let bin_dir = ft_dir // "bin"
