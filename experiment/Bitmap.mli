type bmp_info_hdr = {
  bmp_info_hdrsize: int;    (* 4 bytes *)
  width: int;               (* 4 bytes *)
  height: int               (* 4 bytes *)
  (* 2 bytes of # of color planes skipped (must be 1) *)
  (* 2 bytes of # of bits per pixel skipped; assumed to be 24 *)
  (* 4 bytes of compression method skipped; assumed to be BI_RGB *)
  (* 4 bytes of image size skipped *)
  (* 4 bytes of horizontal resolution skipped *)
  (* 4 bytes of vertical resolution skipped *)
  (* 4 bytes of # of colors in the color palette skipped *)
  (* 4 bytes of # of important colors used skipped *)
}

type bmp = {
  info: bmp_info_hdr;
  bits: (char * char * char) array array
}

exception BitmapFormatNotSupported of char * char

val make_bmp : int -> int -> bmp
val input_bmp : in_channel -> bmp
val output_bmp : out_channel -> bmp -> unit
