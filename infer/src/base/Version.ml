(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_yes = String.equal "yes"

let is_not_no = Fn.non (String.equal "no")

let major = 1

let minor = 1

let patch = 0

let commit = "82b4a324a"

let branch = "main"

type build_platform = Linux | Darwin | Windows

let build_platform = Linux

let is_release = is_yes "no"

let tag = Printf.sprintf "v%d.%d.%d" major minor patch

let versionString = if is_release then tag else Printf.sprintf "%s-%s" tag commit

let versionJson =
  String.concat ~sep:"\n"
    [ "{"
    ; ("\"major\": " ^ string_of_int major ^ ", ")
    ; ("\"minor\": " ^ string_of_int minor ^ ", ")
    ; ("\"patch\": " ^ string_of_int patch ^ ", ")
    ; ("\"commit\": \"" ^ commit ^ "\", ")
    ; ("\"branch\": \"" ^ branch ^ "\", ")
    ; ("\"tag\": \"" ^ tag ^ "\"")
    ; "}" ]

let clang_enabled = is_yes "no"

let erlang_enabled = is_yes "no"

let hack_enabled = is_yes "no"

let java_enabled = is_yes "yes"

let java_version = int_of_string_opt "11"

let xcode_enabled = is_not_no "no"

let man_pages_last_modify_date = "2024-03-08"

;;
