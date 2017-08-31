#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "proto" @@ fun c ->
  Ok [ Pkg.mllib "src/proto.mllib";
       Pkg.test "test/test"; ]
