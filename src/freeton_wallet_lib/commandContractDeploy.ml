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
open Types

type create =
  | UseAccount of string
  | CreateAccount of string


let action ~contract ~force ~params ~wc ?create ?sign ~deployer
    ?initial_data ~credit () =
  let config = Config.config () in
  let net = Config.current_network config in
  let create =
    match create, sign with
    | None, _ -> CreateAccount contract
    | Some ( UseAccount _ ), Some _ ->
        Error.raise "--dst and --sign cannot be used together"
    | Some ( UseAccount dst ), None -> UseAccount dst
    | Some ( CreateAccount dst ), _ -> CreateAccount dst
  in
  let contract = Misc.fully_qualified_contract contract in
  let dst, sign =
    match create with
    | CreateAccount dst ->

        if Misc.key_exists net dst then begin
          if force then
            Misc.delete_account config net dst
          else
            Error.raise "Key %s already exists. Use -f to override" dst
        end;

        Printf.eprintf "Generating new key %S\n%!" dst;
        let sign =
          match sign with
          | None ->
              CommandAccountCreate.genkey ~name:dst
                ~contract ?initial_data config ~force:false;
              None
          | Some sign ->
              let sign = Misc.find_key_exn net sign in
              let key_pair = match sign.key_pair with
                | None ->
                    Error.raise "--sign KEY where KEY has no key pair"
                | Some key_pair -> key_pair
              in
              let acc_address =
                CommandAccountCreate.gen_address config key_pair contract
                  ~initial_data ~wc:None
              in
              let key_account = Some {
                  acc_address ;
                  acc_contract = Some contract ;
                  acc_workchain = None;
                  acc_static_vars = initial_data ;
                } in
              let key = { key_name = dst ;
                          key_account = key_account ;
                          key_passphrase = None ;
                          key_pair = None } in
              net.net_keys <- key :: net.net_keys ;
              Some sign
        in
        let deployer = match deployer with
          | None -> net.net_deployer
          | Some deployer -> deployer
        in
        if credit > 0 then begin
          Printf.eprintf "Sending %d TON from deployer %S\n%!" credit deployer;
          CommandMultisigTransfer.send_transfer
            ~account:deployer
            ~dst
            ~amount:(string_of_int credit) ();
        end;
        Config.save config;
        dst, sign
    | UseAccount dst ->
        begin
          match initial_data with
          | None -> ()
          | Some _ ->
              Error.raise "--initial_data can only be used with --create"
        end;
        dst, None
  in
  let key = Misc.find_key_exn net dst in
  begin
    match key.key_account with
    | Some { acc_contract = Some acc_contract ; _ } when
        acc_contract <> contract ->
        Error.raise "Wrong contract %S for dest %S" acc_contract dst;
    | _ -> ()
  end;
  let initial_data = match key.key_account with
    | Some { acc_static_vars ; _ } -> acc_static_vars
    | None -> Error.raise "Destination %S has no address" dst
  in
  let sign = match sign with
    | None -> key
    | Some sign -> sign
  in
  Subst.with_substituted config params (fun params ->
      Printf.eprintf "Deploying contract %S to %s\n%!" contract dst;
      Utils.deploy_contract config ~key ~sign ~contract ~params
        ?initial_data ~wc ())


let cmd =
  let force = ref false in
  let params = ref "{}" in
  let wc = ref None in
  let static_vars = ref None in
  let create = ref None in
  let deployer = ref None in
  let sign = ref None in
  let contract = ref None in
  let credit = ref 1 in
  EZCMD.sub
    "contract deploy"
    (fun () ->
       match !contract with
       | None ->
           Error.raise "You must provide the name of the contract to deploy"
       | Some contract ->
             action
               ~contract
               ~force:!force
               ~params:!params
               ~wc:!wc
               ?create:!create
               ?sign:!sign
               ~deployer:!deployer
               ?initial_data:!static_vars
               ~credit:!credit
               ()
    )
    ~args:
      [

        [ "force" ; "f" ], Arg.Set force,
        EZCMD.info "Override existing contracts";

        [], Arg.Anon (0, fun name ->
            contract := Some name
          ),
        EZCMD.info ~docv:"CONTRACT" "Deploy contract CONTRACT";

        [ "dst" ], Arg.String (fun s -> create := Some (UseAccount s) ),
        EZCMD.info ~docv:"ACCOUNT"
          "Deploy to this account, using the existing keypair";

        [ "sign" ], Arg.String (fun s -> sign := Some s),
        EZCMD.info ~docv:"ACCOUNT" "Deploy using this keypair";

        [ "static-vars"],
        Arg.String (fun s -> static_vars := Some s),
        EZCMD.info ~docv:"JSON" "Set static vars for account";

        [ "deployer" ], Arg.String (fun s -> deployer := Some s),
        EZCMD.info ~docv:"ACCOUNT"
          "Deployer is this account (pays creation fees)";

        [ "params" ], Arg.String (fun s ->
            params := s),
        EZCMD.info ~docv:"PARAMS" "Constructor/call Arguments ({} by default)";

        [], Arg.Anon (1, fun s -> params := s),
        EZCMD.info ~docv:"PARAMS" "Constructor/call Arguments ({} by default)";

        [ "create" ], Arg.String (fun s -> create := Some (CreateAccount s) ),
        EZCMD.info ~docv:"ACCOUNT"
          "Create ACCOUNT by deploying contract (with --deploy)";

        [ "replace" ], Arg.String (fun s ->
            create := Some (CreateAccount s) ; force := true ),
        EZCMD.info ~docv:"ACCOUNT"
          "Replace ACCOUNT when deploying contract (with --deploy)";

        [ "credit" ], Arg.Int (fun s -> credit := s),
        EZCMD.info ~docv:"TONS" "Initial credit of the account by the deployer";

      ]
    ~doc: "Deploy contracts"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command deploys a known contract to the blockchain";
        `P "Examples:";
        `Pre {|ft contract deploy Forbar|};
        `P "Create an account 'Foorbar', deploy a contract 'Foobar' to it.";
        `Pre {|ft contract deploy Forbar --create foo|};
        `P "Create an account 'foo', deploy a contract 'Foobar' to it.";
        `Pre {|ft contract deploy Forbar --replace foo|};
        `P "Delete account 'foo', recreate it and deploy a contract 'Foobar' to it.";
        `Pre {|ft contract deploy Forbar --create foo --sign admin|};
        `P "Create an empty account 'foo', deploy a contract 'Foobar' to it, using the keypair from 'admin'.";
        `Pre {|ft contract deploy Forbar --dst foo|};
        `P "Deploy a contract 'Foobar' an existing account 'foo' using its keypair.";
        `P "";
        `P "With --create and --replace, 1 TON is transferred to the initial account using a 'deployer' multisig account. The deployer account can either be set switch wide (ft config --deployer 'account') or in the deploy command (using the --deployer 'account' argument)";
      ];
    ]
