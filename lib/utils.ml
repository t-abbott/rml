let rec inverse = function
  | (a, b) :: xs -> (b, a) :: (inverse xs)
  | [] -> []