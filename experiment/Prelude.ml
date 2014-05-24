let flip f x y = f y x;;

module L = struct

  include List;;

  let sum xs = fold_left (+) 0 xs;;

  let rec range begin_ end_ = if begin_ < end_
                                then begin_ :: range (begin_ + 1) end_
                                else [];;

  let maximum (x :: xs) =
    let rec max_rec acc = function
        [] -> acc
      | (y :: ys) -> max_rec (max y acc) ys
    in max_rec x xs;;

  let rec transpose = function
      [] -> []
    | xs :: xss -> let row1 = map hd xss in
                   let submatrix = transpose (map tl xss) in
                   let cons a b = a::b in
                   map2 cons xs (row1::submatrix)

end

module A = Array;;
