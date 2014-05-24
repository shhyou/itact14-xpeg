open Printf;;
open Prelude;;

(* utilities *)

(* print_binary *)
let rec print_binary len n = match len with
    0 -> ()
  | len -> print_binary (len-1) (n/2);
           print_char (if n mod 2 == 1 then '1' else '0');;

(* parse one 16-bit bigendian number *)
let u16_of_char raw_data idx =
  int_of_char raw_data.[idx] * 256 + int_of_char raw_data.[idx+1];;

let u4_of_char raw_data idx =
  let n = int_of_char raw_data.[idx] in
  (n / 16, n mod 16);;

(* jpeg parse *)

module L = List;;

let jpeg_raw =
  let fin = open_in_bin "rgb.jpg" in
  let len = in_channel_length fin in
  let data = String.create len in
  really_input fin data 0 len;
  close_in fin;
  fun () -> data;;

exception Jpeg_format_error of string;;

let jpeg_segmenting raw_data =
  let [ch_ff; ch_00; ch_EOI] = L.map char_of_int [0xff; 0x00; 0xd9]
  in let rec find_candidates acc start_idx =
    try let idx = String.index_from raw_data start_idx ch_ff
        in if raw_data.[idx+1] == ch_EOI
             then L.rev acc
             else find_candidates (idx::acc) (idx+1)
    with Not_found -> raise (Jpeg_format_error "EOI (0xff 0xd9) not found")
  in let parse_result =
       find_candidates [] 0
    |> L.filter (fun i -> raw_data.[i+1] <> ch_00 && raw_data.[i+1] <> ch_ff)
    |> L.map (fun i -> (int_of_char raw_data.[i+1], i+2))
  in match parse_result with
      (0xd8, 2)::rest -> rest
    | _ -> raise (Jpeg_format_error "SOI (0xff 0xd8) not found");;

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
    else L.map (fun i -> parse_table (dqt_idx+2 + i*65)) (range 0 (65/size));;

type jpeg_dht = {
  dht_type: int;              (* DC = 0, AC = 1 *)
  dht_id: int;                (* DHT table id *)
  dht_tbl: (int * int) array  (* (length, data) pairs, index: 16 bit long *)
};;

(* parse DHT table to *)
let jpeg_parse_dht raw_data dht_idx =
  let size = u16_of_char raw_data dht_idx - 2 - 1 - 16 in
  let (table_type, table_id) = u4_of_char raw_data (dht_idx+2) in
  let tree_sizes = flip L.map (range 0 16) (fun i ->
                   int_of_char raw_data.[dht_idx+3 + i]) in
  let acts =
    let rec create_actions = function
      | (_, shift, []) -> []
      | (depth, shift, 0::lens) -> create_actions (depth+1, shift+1, lens)
      | (depth, shift, len::lens) ->
        let sll = (depth, fun n -> (n+1) lsl shift) in
        let inc = range 0 (len-1) |> L.map (fun _ -> (depth, fun n -> n+1)) in
          sll::(inc@create_actions (depth+1, 1, lens)) in
    create_actions (1, 1, tree_sizes) in
  let tbl = Array.make 65536 (0, 0) in
  let set_code prev_code (depth, act, data) =
    let code = act prev_code in
    for i = 0 to (1 lsl (16-depth) - 1) do
      tbl.(code lsl (16-depth) + i) <- (depth, data)
    done;
    code
  in range 0 size
  |> L.map2 (fun (depth, act) i ->
     (depth, act, int_of_char raw_data.[dht_idx+19 + i])) acts
  |> L.fold_left set_code (-1);
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
  let [height; width] = L.map (u16_of_char raw_data) [sof_idx+3; sof_idx+5] in
  let comps =
     flip L.map (range 0 nf) (fun idx ->
     let pos = sof_idx+8 + idx*3 in
     let (hi, vi) = u4_of_char raw_data (pos+1) in
     { sof_comp_id = int_of_char raw_data.[pos]
     ; sof_hi = hi; sof_vi = vi
     ; sof_comp_height = 0; sof_comp_width = 0
     ; sof_tq = int_of_char raw_data.[pos+2] land 0xf} ) in
  let hmax = maximum (L.map (fun c -> c.sof_hi) comps) in
  let vmax = maximum (L.map (fun c -> c.sof_vi) comps) in
  { sof_height = height
  ; sof_width = width
  ; sof_comps = flip L.map comps (fun comp ->
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
    flip L.map (range 0 ns) (fun idx ->
    let (tdj, taj) = u4_of_char raw_data (sof_idx+3 + idx*2 + 1) in
    { sos_comp_sel = int_of_char raw_data.[sof_idx+3 + idx*2]
    ; sos_dc_sel = tdj
    ; sos_ac_sel = taj }) in
  { sos_comps = comps; sos_data = sof_idx + size };;

type jpeg = {
  dqts: jpeg_dqt list;
  dhts: jpeg_dht list;
  sof: jpeg_sof;
  sos: jpeg_sos;
};;

let parse_jpeg raw_data =
  let segs = jpeg_segmenting raw_data in
  let seg_filter marker fn =
       segs
    |> L.filter (fun (marker_, _) -> marker == marker_)
    |> L.map (fun (_, pos) -> fn raw_data pos) in
  let dqts = L.concat (seg_filter 0xdb jpeg_parse_dqt) in
  let dhts = seg_filter 0xc4 jpeg_parse_dht in
  let [sof] = seg_filter 0xc0 jpeg_parse_sof in
  let [sos] = seg_filter 0xda jpeg_parse_sos in
  { dqts = dqts; dhts = dhts; sof = sof; sos = sos };;

let extract_scan raw_data start_idx =
  let rec count_length acc idx = match (raw_data.[idx], raw_data.[idx+1]) with
      ('\xff', '\x00') -> count_length (acc+1) (idx+2)
    | ('\xff', _) -> (acc, idx)
    | _ -> count_length (acc+1) (idx+1) in
  let (next_idx, len) = count_length 0 start_idx in
  let buf = String.make (len+2) '\x00' in
  let rec blit in_idx out_idx = match (raw_data.[in_idx], raw_data.[in_idx+1]) with
      ('\xff', '\x00') -> buf.[out_idx] <- '\xff'; blit (in_idx+2) (out_idx+1)
    | ('\xff', _) -> ()
    | (d, _) -> buf.[out_idx] <- d; blit (in_idx+1) (out_idx+1) in
  blit start_idx 0;
  (next_idx, buf)

let extract_8x8 raw_data dc_tbl ac_tbl start_idx prev_dc buf =
  let u16_of_bits idx =
    let (arr_idx, bit_idx) = (idx lsr 3, idx land 0x7) in
    (int_of_char raw_data.[arr_idx]  lsl 16 +
     int_of_char raw_data.[arr_idx+1] lsl 8 +
     int_of_char raw_data.[arr_idx+2]) lsl (7 + bit_idx) in
  let get_signed_int bits = function
    | 0 -> 0
    | len -> let msk = lnot (bits asr 30) in
             ((1 lsl len) lxor msk) + (2 land msk) + (bits asr (31-len)) in
(*  printf "bits : %08x\n" (u16_of_bits start_idx); *)
  let (dc_hufflen, dc_huff) = dc_tbl.(u16_of_bits start_idx lsr 15) in
(*  printf "dc_hufflen = %d, dc_huff = %d\n" dc_hufflen dc_huff; *)
  buf.(0) <- prev_dc + get_signed_int (u16_of_bits (start_idx+dc_hufflen)) dc_huff;
  printf "dc: %d\n" buf.(0);
  let rec extract_ac idx = function
    | 64 -> idx
    | n -> match ac_tbl.(u16_of_bits idx lsr 15) with
              (ac_hufflen, 0) -> idx + ac_hufflen
            | (ac_hufflen, ac_huff) ->
              let (rrrr, ssss) = (ac_huff lsr 4, ac_huff land 0xf) in
              buf.(n+rrrr) <- get_signed_int (u16_of_bits (idx+ac_hufflen)) ssss;
              printf "    ac: %d -> %d\n" (n+rrrr) buf.(n+rrrr);
              extract_ac (idx+ac_hufflen+ssss) (n+rrrr+1) in
  extract_ac (start_idx+dc_hufflen+dc_huff) 1;;

let extract_mcu scan jpg start_idx =
  let comp_cnts = L.map (fun c -> c.sof_hi*c.sof_vi) jpg.sof.sof_comps in
  let bufs = Array.of_list (L.map (fun cnt -> Array.make_matrix cnt 64 0) comp_cnts) in
  let dht_select dht_type dht_id  =
    let predicate tbl = tbl.dht_type == dht_type && tbl.dht_id == dht_id in
    match L.filter predicate jpg.dhts with
      [x] -> x.dht_tbl
    | _ -> raise (Failure "Unknown Huffman (type, id)") in
  let (_, next_idx) =
     L.combine comp_cnts jpg.sos.sos_comps
  |> L.fold_left (fun (m, prev_idx) (comp_cnt, sos_comp) ->
       let { sos_dc_sel = dc_sel; sos_ac_sel = ac_sel } = sos_comp in
       printf "dc_sel = %d, ac_sel = %d\n" dc_sel ac_sel;
       let (dc_tbl, ac_tbl) = (dht_select 0 dc_sel, dht_select 1 ac_sel) in
       let rec extract_loop n prev_dc idx =
(*         printf "extract %d: idx=%d\n" n idx; *)
         if n == comp_cnt
           then idx
           else extract_8x8 scan dc_tbl ac_tbl idx prev_dc bufs.(m).(n)
             |> extract_loop (n+1) bufs.(m).(n).(0) in
       (m+1, extract_loop 0 0 prev_idx)) (0, start_idx)
  in (next_idx, bufs)

(* TODO: parse jpeg using a loop (sequentially); remove jpeg_segmenting *)
let test () =
  let raw_data = jpeg_raw () in
  printf "reading...\n";
  let jpg = parse_jpeg raw_data in
  let (next_idx, scan) = extract_scan raw_data jpg.sos.sos_data in
  extract_mcu scan jpg 0;;

let zigzag_order () =
  let skew_diag y0 x0 =
    (if (y0+x0) mod 2 == 0 then (fun x -> x) else L.rev)
    (L.map (fun i -> (y0-i, x0+i)) (range 0 (min y0 (7-x0) + 1))) in
  L.map2 skew_diag (range 0 8@[7;7;7;7;7;7;7]) ([0;0;0;0;0;0;0]@range 0 8);;
