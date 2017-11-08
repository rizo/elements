

(* https://dlang.org/phobos/std_stdio.html#.File.lockingBinaryWriter *)

(*
  auto file = File("file.txt"); // Open for reading
  const wordCount = file.byLine()            // Read lines
                        .map!split           // Split into words
                        .map!(a => a.length) // Count words per line
                        .sum();              // Total word count
  writeln(wordCount);


readFile :: FilePath -> Producer' String (SafeT IO) ()
readFile file = bracket
    (do h <- IO.openFile file IO.ReadMode
        putStrLn $ "{" ++ file ++ " open}"
        return h )
    (\h -> do
        IO.hClose h
        putStrLn $ "{" ++ file ++ " closed}" )
    P.fromHandle
*)

open Proto
open Proto.System

module Iter = Data.Iter
module Fold = Data.Fold
module File = IO.File


let long_words len =
  File.read_lines (path "/usr/share/dict/words")
  |> Iter.map (fun word -> (String.length word, word))
  |> Iter.select (fun (n, word) -> n > 5)
  |> Iter.map (fun (n, word) -> format "%d\t%s" n word)
  |> File.write_lines (path "/tmp/long-words")

