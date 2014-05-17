open Prelude;;
open Bitmap;;

let (dct1, dct_vecs, idct1, idct_vecs) =
  let sumf = List.fold_left (+.) 0.0 in
  let pi = acos (-1.0) in
  let normalize_factor = sqrt (2.0 /. 8.0) in
  let angle n k = cos (pi *. (float_of_int n +. 0.5) *. float_of_int k /. 8.0) in
  let cos_vecs f = flip List.map (range 0 8) (fun k ->
                     flip List.map (range 0 8) (fun n ->
                       f n k *. normalize_factor)) in
  let dct_vecs = cos_vecs angle in
  let idct_vecs =
    let fix_x0_coef (_::xs) = 0.5 *. normalize_factor::xs in
    List.map fix_x0_coef (cos_vecs (fun n k -> angle k n)) in
  ( (fun v -> List.map (fun u -> sumf (List.map2 ( *. ) v u)) dct_vecs)
  , dct_vecs
  , (fun v -> List.map (fun u -> sumf (List.map2 ( *. ) v u)) idct_vecs)
  , idct_vecs )

let input_file = "test.bmp";;
let output_file = "testout.bmp";;

let bmp_in =
  let fin = open_in_bin input_file in
  let bmp = input_bmp fin in
  close_in fin;
  bmp

let bmp_dct = let size = 8 * 8 * 4 + 7 in make_bmp size size;;

let () =
  flip List.iteri dct_vecs (fun i u ->
    flip List.iteri dct_vecs (fun j v -> (* maps [-1.0, 1.0] to [0.0,255.0] *)
    ( let char_of_float f = char_of_int (int_of_float ((f +. 1.0) *. 127.5)) in
      let (y0, x0) = (i*(4*8 + 1), j*(4*8 + 1)) in
      let set_color y x color =
        flip List.iter [0;1;2;3] (fun dy ->
          flip List.iter [0;1;2;3] (fun dx ->
            bmp_dct.bits.(y + dy).(x + dx) <- (color, color, color))) in
      flip List.iteri v (fun dy ui ->
        flip List.iteri u (fun dx vj ->
          set_color (y0 + dy * 4) (x0 + dx * 4) (char_of_float (ui *. vj)))))));

  let fout = open_out_bin "dct2.bmp" in
  output_bmp fout bmp_dct;
  close_out fout;;
