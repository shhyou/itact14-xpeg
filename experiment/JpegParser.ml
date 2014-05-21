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

type jpeg_dqt = {
  dqt_id: int;  (* DQT table id *)
  dqt_idx: int  (* DQT start index *)
};;

(* parse DQT table to list of (table_index, raw_data_position) *)
let jpeg_parse_dqt raw_data dqt_idx =
  let size = u16_of_char raw_data dqt_idx - 2 in
  let parse_table idx =
    let (0, table_id) = u4_of_char raw_data idx in
    { dqt_id = table_id; dqt_idx = idx+1 } in
  if 65 mod size <> 0
    then raise (Jpeg_format_error "DQT table size is not a multiple of 65")
    else List.map (fun i -> parse_table (dqt_idx+2 + i*65)) (range 0 (65/size));;

(* print_binary *)
let rec print_binary len n = match len with
    0 -> ()
  | len -> print_binary (len-1) (n/2);
           print_char (if n mod 2 == 1 then '1' else '0');;

type jpeg_dht = {
  dht_type: int;              (* DC = 0, AC = 1 *)
  dht_id: int;                (* DHT table id *)
  dht_tbl: (int * int) array  (* (length, data) pairs, index: 15 bit long *)
};;

(* parse DHT table to *)
let jpeg_parse_dht raw_data dht_idx =
  let size = u16_of_char raw_data dht_idx - 2 - 1 - 16 in
  let (table_type, table_id) = u4_of_char raw_data (dht_idx+2) in
  let tree_sizes = flip List.map (range 0 16) (fun i ->
                   int_of_char raw_data.[dht_idx+3 + i])
                |> List.filter (fun len -> len <> 0) in
  let acts =
    List.concat
    (flip List.mapi tree_sizes (fun depth len ->
    (depth+2, fun n -> (n+1) lsl 1)::List.map (fun _ -> (depth+2, fun n -> n+1))
                                              (range 0 (len-1)))) in
  let tbl = Array.make 32768 (0, 0) in
  let set_code prev_code (depth, act, data) =
    let code = act prev_code in
    for i = 0 to (1 lsl (15-depth) - 1)do
      tbl.(code lsl (15-depth) + i) <- (depth, data)
    done;
    code
  in range 0 size
  |> List.map2 (fun (depth, act) i ->
     (depth, act, int_of_char raw_data.[dht_idx+19 + i])) acts
  |> List.fold_left set_code (-1);
  { dht_type = table_type; dht_id = table_id; dht_tbl = tbl };;

type jpeg_sof_comp = {
    sof_comp_id : int;
    sof_hi : int;
    sof_vi : int;
    sof_comp_height: int;
    sof_comp_width: int;
    sof_tq : int;
};;

type jpeg_sof = {
  sof_height : int;
  sof_width : int;
  sof_comps : jpeg_sof_comp list
};;

let jpeg_parse_sof raw_data sof_idx =
  let nf = int_of_char raw_data.[sof_idx+7] in
  let [height; width] = List.map (u16_of_char raw_data) [sof_idx+3; sof_idx+5] in
  let comps =
     flip List.map (range 0 nf) (fun idx ->
     let pos = sof_idx+8 + idx*3 in
     let (hi, vi) = u4_of_char raw_data (pos+1) in
     { sof_comp_id = int_of_char raw_data.[pos]
     ; sof_hi = hi; sof_vi = vi
     ; sof_comp_height = 0; sof_comp_width = 0
     ; sof_tq = int_of_char raw_data.[pos+2] land 0xf} ) in
  let hmax = maximum (List.map (fun c -> c.sof_hi) comps) in
  let vmax = maximum (List.map (fun c -> c.sof_vi) comps) in
  { sof_height = height
  ; sof_width = width
  ; sof_comps = flip List.map comps (fun comp ->
                { comp with sof_comp_height = height * comp.sof_hi / hmax
                          ; sof_comp_width = width * comp.sof_vi / vmax }) };;

type jpeg_sos_comp = {
  sos_comp_sel: int;
  sos_dc_sel: int;
  sos_ac_sel: int;
};;

type jpeg_sos = {
  sos_comps: jpeg_sos_comp list;
  sos_data: int (* index to the scan data *)
};;

let jpeg_parse_sos raw_data sof_idx =
  let size = u16_of_char raw_data sof_idx in
  let ns = int_of_char raw_data.[sof_idx+2] in
  let comps =
    flip List.map (range 0 ns) (fun idx ->
    let (tdj, taj) = u4_of_char raw_data (sof_idx+3 + idx*2 + 1) in
    { sos_comp_sel = int_of_char raw_data.[sof_idx+3 + idx*2]
    ; sos_dc_sel = tdj
    ; sos_ac_sel = taj }) in
  { sos_comps = comps; sos_data = sof_idx + size };;
