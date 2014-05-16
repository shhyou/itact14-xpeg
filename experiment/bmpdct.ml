(* prelude *)

let rec range begin_ end_ = if begin_ < end_
                              then begin_ :: range (begin_ + 1) end_
                              else [];;

(*  char4_of_int int -> char list  *)
let char4_of_int n =
  List.map (fun i -> char_of_int (n/i mod 256))
  [1; 256; 256*256; 256*256*256]

(*  int_of_char4 : string -> int -> int  *)
let int_of_char4 buffer idx =
  List.fold_left (+) 0
 (List.mapi (fun i base -> int_of_char buffer.[idx + i] * base)
  [1; 256; 256*256; 256*256*256])

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
  (* 2 bytes of # of color planes skipped (must be 1) *)
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

let output_bmp_file_hdr co bfh =
  output_char co (fst bfh.file_type);
  output_char co (snd bfh.file_type);
  List.iter (output_char co) (char4_of_int bfh.bmp_filesize);
  List.iter (output_char co) (char4_of_int 0);
  List.iter (output_char co) (char4_of_int bfh.offset);;

let output_bmp_info_hdr co bih =
  (* 4 *) List.iter (output_char co) (char4_of_int bih.bmp_info_hdrsize);
  (* 4 *) List.iter (output_char co) (char4_of_int bih.width);
  (* 4 *) List.iter (output_char co) (char4_of_int bih.height);
  (* 2 *) output_char co '\x01'; output_char co '\x00'; (* # of color planes *)
  (* 2 *) output_char co '\x18'; output_char co '\x00'; (* # of bits per pixel *)
  (* 4 *) List.iter (output_char co) (char4_of_int 0);  (* BI_RGB *)
          let row_size = (bih.width * 24 + 31) / 32 * 4 in
  (* 4 *) List.iter (output_char co) (char4_of_int (row_size * bih.height));
  (* 4 *) List.iter (output_char co) (char4_of_int 0);  (* v-resolution *)
  (* 4 *) List.iter (output_char co) (char4_of_int 0);  (* h-resolution *)
  (* 4 *) List.iter (output_char co) (char4_of_int 0);  (* # of colors in the palatte *)
  (* 4 *) List.iter (output_char co) (char4_of_int 0);; (* # of important colors *)

let output_bmp co bmp =
  let row_size = (bmp.info.width * 24 + 31) / 32 * 4 in
  let file_size = 14 + 40 + row_size * bmp.info.height in
  output_bmp_file_hdr co { file_type = ('B','M')
                         ; bmp_filesize = file_size
                         ; offset = 14 + 40 };
  output_bmp_info_hdr co bmp.info;
  let row_padding = String.make (row_size - bmp.info.width * 3) '\x00' in
  Array.iter (fun row ->
    Array.iter (fun (r,g,b) ->
      output_char co b;
      output_char co g;
      output_char co r) row;
    output_string co row_padding) bmp.bits;;

let make_bmp height width =
  { info = { bmp_info_hdrsize = 40; width = width; height = height }
  ; bits = Array.make_matrix height width ('\x00','\x00','\x00') };;

exception BitmapFormatNotSupported of char * char;;

let input_bmp_file_hdr ci =
  let buffer = String.create 14 in
  really_input ci buffer 0 14;
  match (buffer.[0], buffer.[1]) with
    ('B', 'M') -> { file_type = (buffer.[0], buffer.[1])
                  ; bmp_filesize = int_of_char4 buffer 2
                  ; offset = int_of_char4 buffer 10 }
  | (m0, m1) -> raise (BitmapFormatNotSupported (m0, m1))

let input_bmp_info_hdr ci =
  let buffer = String.create 12 in
  really_input ci buffer 0 12;
  { bmp_info_hdrsize = int_of_char4 buffer 0
  ; width = int_of_char4 buffer 4
  ; height = int_of_char4 buffer 8; }

let input_bmp ci =
  let bf_hdr = input_bmp_file_hdr ci in
  let bi_hdr = input_bmp_info_hdr ci in
  let row_size = (bi_hdr.width * 24 + 31) / 32 * 4 in
  let bits = Array.make_matrix bi_hdr.width bi_hdr.height ('\x00','\x00','\x00') in
  Array.iteri (fun y arr ->
    seek_in ci (bf_hdr.offset + y * row_size);
    Array.iteri (fun x _ ->
      let b = input_char ci in
      let g = input_char ci in
      let r = input_char ci in
      arr.(x) <- (r, g, b)) arr) bits;
  { info = bi_hdr; bits = bits }

let input_file = "test.bmp";;
let output_file = "testout.bmp";;

let bmp_in =
  let fin = open_in_bin input_file in
  let bmp = input_bmp fin in
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
