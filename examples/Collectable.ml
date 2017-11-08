open Proto
open Proto.System

module Iter = Data.Iter
module File = IO.File

(* Produce a file with long words from dict. *)
let find_long_words len =
  File.read_lines (path "/usr/share/dict/words")
  |> Iter.map (fun word -> (String.length word, word))
  |> Iter.select (fun (n, word) -> n > 5)
  |> Iter.map (fun (n, word) -> format "%d\t%s" n word)
  |> File.write_lines (path "/tmp/long-words")

(* Construct the alphabet out of characters. *)
let alphabet =
  Iter.range 97 123
  |> Iter.map char
  |> Iter.intersparse ' '
  |> String.into "Alphabet: "

(* Construct a set from CSV column values read from STDIN. *)
let uniq_col n =
  Chan.read_lines IO.stdin
  |> Iter.map (String.split ~on:',')
  |> Iter.map (Iter.get n)
  |> Iter.filter_map int
  |> Set.collect

