
open Elements
open Data

let get () =
  let v1 = vec ["red"; "green"; "blue"; "yellow"] in
  assert (Vec.get v1 3 = Some "yellow");
  assert (Vec.get v1 5 = None)

let group () =
  let l1 = [1; 1; 2; 2; 2; 2; 3; 4; 5; 5] in
  let r1 = [[1; 1]; [2; 2; 2; 2]; [3]; [4]; [5; 5]] in
  assert (Iter.group (List.iter l1) = r1)

let remove () =
  let l1 = [0; 1; 2; 3; 4; 5] in
  let r1 = [0; 2; 3; 4; 5] in
  assert (List.collect (Iter.remove 1 (List.iter l1)) = r1);
  assert (List.collect (Iter.remove 9 (List.iter l1)) = l1);

let group_on () =
  let l1 = [("a", 2); ("b", 2); ("c", 3); ("d", 4); ("e", 4)] in
  let r1 = [[("a", 2); ("b", 2)]; [("c", 3)]; [("d", 4); ("e", 4)]] in
  assert (Iter.group_on second (List.iter l1) = r1)

let group_by () =
  let l1 = [1; 2; 3; 20; 23; 25; 40; 47; 49; 52] in
  let r1 = [[1; 2; 3]; [20; 23; 25]; [40; 47; 49]; [52]] in
  assert (Iter.group_by (fun a b -> b - a <= 10) (List.iter l1) = r1)

let find () =
  let v1 = vec ["red"; "green"; "blue"; "pink"] in
  assert (Vec.find (fun color -> String.length color = 4) v1 = Some "blue");
  assert (Vec.find (fun color -> String.length color = 1) v1 = None)

let find_index () =
  let v1 = vec ["red"; "green"; "blue"; "pink"] in
  assert (Vec.find_index (fun color -> String.length color = 4) v1 = Some 2);
  assert (Vec.find_index (fun color -> String.length color = 1) v1 = None)

let find_indices () =
  let v1 = vec ["red"; "green"; "blue"; "pink"] in
  let r1 = List.iter [2; 3] in
  assert (Vec.find_index (fun color -> String.length color = 4) v1 = r1);

let cycle () =
  let v1 = vec [0; 1] in
  let r1 = vec [0; 1; 0; 1; 0; 1] in
  assert (Vec.collect (Iter.take 6 (Vec.cycle v1)) = r1)

let index () =
  let v1 = vec ["red"; "green"; "blue"; "yellow"] in
  assert (Vec.index v1 "blue"  = Some 2);
  assert (Vec.index v1 "black" = None)

let indices () =
  let v1 = vec ["a"; "a"; "b"; "c"; "d"; "a"; "b"] in
  assert (Vec.indices v1 "a"  = [0; 1; 5]);
  assert (Vec.indices v1 "c"  = [3]);
  assert (Vec.indices v1 "x"  = [])

let contains () =
  let v1 = vec ["red"; "green"; "blue"; "yellow"] in
  assert (Vec.contains v1 "red"   = true);
  assert (Vec.contains v1 "black" = false)

let enumerate () =
  let v1 = vec ["red"; "green"; "blue"; "yellow"] in
  let r1 = vec [(0, "red");   (1, "green");  (2, "blue");  (3, "yellow")] in
  let r2 = vec [(10, "red"); (11, "green"); (12, "blue"); (13, "yellow")] in
  assert (Vec.collect (Vec.enumerate v1) = r1);
  assert (Vec.collect (Vec.enumerate ~from:10 v1) = r2)

let collect () =
  let r1 = [0; 1; 2; 3; 4; 5; 6] in
  assert (Vec.collect (Iter.take 7 (Iter.count ())) = r1)

