#!/usr/bin/env ocaml

let cmd fmt =
  Printf.kprintf (fun cmd ->
      let ret = Sys.command cmd in
      if ret <> 0 then begin
        Printf.eprintf "Fatal error: command returned non-zero status: %d\n%s\n%!" ret cmd;
        exit 2
      end
    ) fmt

let () =
  let len = Array.length Sys.argv in
  if len = 1 then begin
    Printf.eprintf "usage: ./scripts/import.ml .ft/contracts/*\n%!";
    Printf.eprintf " import contracts from .ft to sources\n%!";
    exit 0
  end;
  if not (Sys.file_exists "drom.toml") then begin
    Printf.eprintf "Fatal error: must be run in ft root dir\n%!";
    exit 2
  end;
  let destdir = "src/freeton_wallet_lib/files/contracts" in
  for i = 1 to len - 1 do
    let file = Sys.argv.(i) in
    let tvc_file = Filename.concat file "1.tvc" in
    let abi_file = Filename.concat file "1.abi.json" in
    if not ( Sys.file_exists tvc_file ) then begin
      Printf.eprintf "Fatal error: %s does not exists\n%!" tvc_file;
      exit 2
    end;
    if not ( Sys.file_exists abi_file ) then begin
      Printf.eprintf "Fatal error: %s does not exists\n%!" abi_file;
      exit 2
    end;
    let contract = Filename.basename file in
    cmd "cp -f %s %s/%s.tvc" tvc_file destdir contract;
    cmd "cp -f %s %s/%s.abi.json" abi_file destdir contract;

  done
