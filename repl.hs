calc "val x = 6; val y = 3; fun test(x) { val x = x + 4; x + 2 }; y + test(test(x))"
calc "val x = 6; val y = 3; fun test(x) { val x = x + 4; x + 2 }; y + test(test(x))"
calc "if 2 > 3 then 2 else 3"
calc "test(3, 2, 1)"
calc "fun test(x,y) { if x == 0 then 0 else if y == 0 then 0 else (x+y)+test(x-1,y-1)}; test(2,2)"
