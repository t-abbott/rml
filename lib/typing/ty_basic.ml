type t = TInt | TBool

let equal x y = x = y
let to_string = function TInt -> "int" | TBool -> "bool"
