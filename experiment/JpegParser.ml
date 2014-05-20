open Prelude;;

(* jpeg parse *)

(* TODO
 * - huffman tables
 * - quantisation table
 * - SOF (start of fragment): height, width, components
 * - SOS (start of segment) : dctable, actable, ncomponents *)

let jpeg_raw =
  let fin = open_in_bin "hd.jpg" in
  let len = in_channel_length fin in
  let data = String.create len in
  really_input fin data 0 len;
  close_in fin;
  fun () -> data;;

exception Jpeg_format_error of string;;

let jpeg_segmenting raw_data =
  let [ch_ff; ch_00; ch_EOI] = List.map char_of_int [0xff; 0x00; 0xd9]
  in let rec find_candidates acc start_idx =
    try let idx = String.index_from raw_data start_idx ch_ff
        in if raw_data.[idx+1] == ch_EOI
             then List.rev acc
             else find_candidates (idx::acc) (idx+1)
    with Not_found -> raise (Jpeg_format_error "EOI (0xff 0xd9) not found")
  in let parse_result =
       find_candidates [] 0
    |> List.filter (fun i -> raw_data.[i+1] <> ch_00 && raw_data.[i+1] <> ch_ff)
    |> List.map (fun i -> (int_of_char raw_data.[i+1], i+2))
  in match parse_result with
      (0xd8, 2)::rest -> rest
    | _ -> raise (Jpeg_format_error "SOI (0xff 0xd8) not found");;

let zigzag_order () =
  let skew_diag y0 x0 =
    (if (y0+x0) mod 2 == 0 then (fun x -> x) else List.rev)
    (List.map (fun i -> (y0-i, x0+i)) (range 0 (min y0 (7-x0) + 1))) in
  List.map2 skew_diag (range 0 8@[7;7;7;7;7;7;7]) ([0;0;0;0;0;0;0]@range 0 8);;

(* parse one 16-bit bigendian number *)
let u16_of_char raw_data idx =
  int_of_char raw_data.[idx] * 256 + int_of_char raw_data.[idx+1];;

let u4_of_char raw_data idx =
  let n = int_of_char raw_data.[idx] in
  (n / 16, n mod 16);;

(* parse DQT table to list of (table_index, raw_data_position) *)
let jpeg_parse_dqt raw_data dqt_idx =
  let size = u16_of_char raw_data dqt_idx - 2 in
  let parse_table idx =
    let (0, table_id) = u4_of_char raw_data idx in
    (table_id, idx+1) in
  if 65 mod size <> 0
    then raise (Jpeg_format_error "DQT table size is not a multiple of 65")
    else List.map (fun i -> parse_table (dqt_idx+2 + i*65)) (range 0 (65/size));;

(* TODOs *)
let jpeg_parse_dht raw_data dht_idx =
  let size = u16_of_char raw_data dht_idx - 2 in
  ();;

let jpeg_parse_sof raw_data sof_idx = ();;
let jpeg_parse_sos raw_data sof_idx = ();;
