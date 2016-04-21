
open Elements
open Data

let _ = {|

size 3
step 2

c   i   j   curr        next    i % size    i % step
0   a   0   []          []      0           0
1   b   0   [0]         []      1           1
2   c   0   [0;1]       []      2           0
3   d   0   [0;1;2]     [2]     0           1
4   e   0   [2;3]       []      1           0
5   f   0   [2;3;4]     [4]     2           1
6   g   0   [4;5]       []      0           0
7   h   0   [4;5;6]     [6]     1           1
8   i   0   [6;7]       []      2           0
9   j   0   [6;7;8]     [8]     0           1
9   j   0   [8;9]       []      1           0


|}

let test_window () =
  let input1  = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
  and output1 = [[0; 1; 2]; [2; 3; 4]; [4; 5; 6]; [6; 7; 8]; [8; 9]]
  and output2 = [[0; 1; 2; 3; 4]; [3; 4; 5; 6; 7]; [6; 7; 8; 9]]
  in begin
    assert (List.window ~size:3 ~step:2 input1 = output1);
    assert (List.window ~size:5 ~step:3 input1 = output2);
  end

let () =
  test_window ()

