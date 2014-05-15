(* prelude *)

let rec range begin_ end_ = if begin_ < end_
                              then begin_ :: range (begin_ + 1) end_
                              else [];;

(* start *)

type bmp_file_hdr = {
  file_type: char * char;   (*  2 bytes *)
  bmp_filesize: int;        (*  4 bytes *)
  (* 4 bytes of reserved fields skipped *)
  offset: int               (*  4 bytes *)
};;

type bmp_info_hdr = {
  bmp_info_hdrsize: int;    (* 4 bytes *)
  width: int;               (* 4 bytes *)
  height: int;              (* 4 bytes *)
  (* 2 bytes of # of color planes skipped *)
  (* 2 bytes of # of bits per pixel skipped; assumed to be 24 *)
  (* 4 bytes of compression method skipped; assumed to be BI_RGB *)
  (* 4 bytes of image size skipped *)
  (* 4 bytes of horizontal resolution skipped *)
  (* 4 bytes of vertical resolution skipped *)
  (* 4 bytes of # of colors in the color palette skipped *)
  (* 4 bytes of # of important colors used skipped *)
};;

type bmp = {
  info: bmp_info_hdr;
  bits: (char * char * char) array array
};;

exception BitmapFormatNotSupported of char * char;;

let bmp_file_hdr_of_channel ci =
  let buffer = String.create 14 in
  let int_of_char4 idx =
    List.fold_left (+) 0
   (List.mapi (fun i base -> int_of_char buffer.[idx + i] * base)
    [1; 256; 256*256; 256*256*256]) in
  really_input ci buffer 0 14;
  match (buffer.[0], buffer.[1]) with
    ('B', 'M') -> { file_type = (buffer.[0], buffer.[1])
                  ; bmp_filesize = int_of_char4 2
                  ; offset = int_of_char4 10 }
  | (m0, m1) -> raise (BitmapFormatNotSupported (m0, m1))

let bmp_info_hdr_of_channel ci =
  let buffer = String.create 12 in
  let int_of_char4 idx =
    List.fold_left (+) 0
   (List.mapi (fun i base -> int_of_char buffer.[idx + i] * base)
    [1; 256; 256*256; 256*256*256]) in
  really_input ci buffer 0 12;
  { bmp_info_hdrsize = int_of_char4 0
  ; width = int_of_char4 4
  ; height = int_of_char4 8; }

let bmp_of_channel ci =
  let bf_hdr = bmp_file_hdr_of_channel ci in
  let bi_hdr = bmp_info_hdr_of_channel ci in
  let row_size = (bi_hdr.width * 24 + 31) / 32 * 4 in
  let bits = Array.make_matrix bi_hdr.width bi_hdr.height ('\x00','\x00','\x00') in
  Array.iteri (fun y arr ->
    seek_in ci (bf_hdr.offset + y * row_size);
    Array.iteri (fun x _ ->
      let r = input_char ci in
      let g = input_char ci in
      let b = input_char ci in
      arr.(x) <- (r, g, b)) arr) bits;
  { info = bi_hdr; bits = bits }

let input_file = "test.bmp";;
let output_file = "testout.bmp";;

let bmp_in =
  let fin = open_in_bin input_file in
  let bmp = bmp_of_channel fin in
  close_in fin;
  bmp

let (dct1, idct1) =
  let sumf = List.fold_left (+.) 0.0 in
  let pi = acos (-1.0) in
  let angle n k = cos (pi *. (float_of_int n +. 0.5) *. float_of_int k /. 16.0) in
  let cos_vecs f = List.map (fun k ->
                     List.map (fun n ->
                       f n k) (range 1 17)) (range 1 17) in
  let dct_vecs = cos_vecs angle in
  let idct_vecs = cos_vecs (fun n k -> angle k n) in
  ( (fun v -> List.map (fun u -> sumf (List.map2 ( *. ) v u)) dct_vecs)
  , (fun v -> List.map (fun u -> sumf (List.map2 ( *. ) v u)) idct_vecs) )
