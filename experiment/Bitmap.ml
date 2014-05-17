open Prelude;;

(* repeated definition, don't know how to solve this *)
type bmp_info_hdr = { bmp_info_hdrsize: int
                    ; width: int
                    ; height: int };;

type bmp = { info: bmp_info_hdr
           ; bits: (char * char * char) array array };;

exception BitmapFormatNotSupported of char * char;;

module A = Array;;
module L = List;;

(* utility functions *)

(*  char4_of_int int -> char list  *)
let char4_of_int n =
  L.map (fun i -> char_of_int (n/i mod 256))
  [1; 256; 256*256; 256*256*256]

(*  int_of_char4 : string -> int -> int  *)
let int_of_char4 buffer idx =
  L.fold_left (+) 0
 (L.mapi (fun i base -> int_of_char buffer.[idx + i] * base)
  [1; 256; 256*256; 256*256*256])

(* start of main functions *)

type bmp_file_hdr = {
  file_type: char * char;   (*  2 bytes *)
  bmp_filesize: int;        (*  4 bytes *)
  (* 4 bytes of reserved fields skipped *)
  offset: int               (*  4 bytes *)
};;

let output_bmp_file_hdr co bfh =
  output_char co (fst bfh.file_type);
  output_char co (snd bfh.file_type);
  L.iter (output_char co) (char4_of_int bfh.bmp_filesize);
  L.iter (output_char co) (char4_of_int 0);
  L.iter (output_char co) (char4_of_int bfh.offset);;

let output_bmp_info_hdr co bih =
  let puts = L.iter (output_char co) in
  (* 4 *) puts (char4_of_int bih.bmp_info_hdrsize);
  (* 4 *) puts (char4_of_int bih.width);
  (* 4 *) puts (char4_of_int bih.height);
  (* 2 *) puts ['\x01'; '\x00'];  (* # of color planes *)
  (* 2 *) puts ['\x18'; '\x00'];  (* # of bits per pixel *)
  (* 4 *) puts (char4_of_int 0);  (* BI_RGB *)
          let row_size = (bih.width * 24 + 31) / 32 * 4 in
  (* 4 *) puts (char4_of_int (row_size * bih.height));
  (* 4 *) puts (char4_of_int 0);  (* v-resolution *)
  (* 4 *) puts (char4_of_int 0);  (* h-resolution *)
  (* 4 *) puts (char4_of_int 0);  (* # of colors in the palatte *)
  (* 4 *) puts (char4_of_int 0);; (* # of important colors *)

let output_bmp co bmp =
  let row_size = (bmp.info.width * 24 + 31) / 32 * 4 in
  let file_size = 14 + 40 + row_size * bmp.info.height in
  output_bmp_file_hdr co { file_type = ('B','M')
                         ; bmp_filesize = file_size
                         ; offset = 14 + 40 };
  output_bmp_info_hdr co bmp.info;
  let row_padding = String.make (row_size - bmp.info.width * 3) '\x00' in
  flip A.iter bmp.bits (fun row ->
    flip A.iter row (fun (r,g,b) ->
      L.iter (output_char co) [b; g; r]);
    output_string co row_padding);;

let make_bmp height width =
  { info = { bmp_info_hdrsize = 40; width = width; height = height }
  ; bits = A.make_matrix height width ('\x00','\x00','\x00') };;

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
  let bits = A.make_matrix bi_hdr.width bi_hdr.height ('\x00','\x00','\x00') in
  flip A.iteri bits (fun y arr ->
    seek_in ci (bf_hdr.offset + y * row_size);
    flip A.iteri arr (fun x _ ->
      let b = input_char ci in
      let g = input_char ci in
      let r = input_char ci in
      arr.(x) <- (r, g, b)));
  { info = bi_hdr; bits = bits }
