module L = List;;

let flip f x y = f y x;;

let rec range begin_ end_ = if begin_ < end_
                              then begin_ :: range (begin_ + 1) end_
                              else [];;

let rec transpose = function
    [] -> []
  | xs :: xss -> let row1 = L.map L.hd xss in
                 let submatrix = transpose (L.map L.tl xss) in
                 let cons a b = a :: b in
                 L.map2 cons xs (row1 :: submatrix)
