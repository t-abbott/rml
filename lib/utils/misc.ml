let proj2 f x y = (f x, f y)
let proj3 f x y z = (f x, f y, f z)
let rec repeat s = function 0 -> s | n -> s ^ repeat s (n - 1)
let indent s level = repeat "  " level ^ s
