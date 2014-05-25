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
  dqt_tbl: int array (* DQT table *)
};;

(* parse DQT table to list of (table_index, raw_data_position) *)
let jpeg_parse_dqt raw_data dqt_idx =
  let size = u16_of_char raw_data dqt_idx - 2 in
  let parse_table idx =
    let (0, table_id) = u4_of_char raw_data idx in
    { dqt_id = table_id
    ; dqt_tbl = A.init 64 (fun i -> int_of_char raw_data.[idx+1 + i]) } in
  if 65 mod size <> 0
    then raise (Jpeg_format_error "DQT table size is not a multiple of 65")
    else L.map (fun i -> parse_table (dqt_idx+2 + i*65)) (L.range 0 (65/size));;

type jpeg_dht = {
  dht_type: int;              (* DC = 0, AC = 1 *)
  dht_id: int;                (* DHT table id *)
  dht_tbl: (int * int) array  (* (length, data) pairs, index: 16 bit long *)
};;

(* parse DHT table to *)
let jpeg_parse_dht raw_data dht_idx =
  let size = u16_of_char raw_data dht_idx - 2 - 1 - 16 in
  let (table_type, table_id) = u4_of_char raw_data (dht_idx+2) in
  let tree_sizes = flip L.map (L.range 0 16) (fun i ->
                   int_of_char raw_data.[dht_idx+3 + i]) in
  let acts =
    let rec create_actions = function
        (_, shift, []) -> []
      | (depth, shift, 0::lens) -> create_actions (depth+1, shift+1, lens)
      | (depth, shift, len::lens) ->
        let sll = (depth, fun n -> (n+1) lsl shift) in
        let inc = L.range 0 (len-1) |> L.map (fun _ -> (depth, fun n -> n+1)) in
          sll::(inc@create_actions (depth+1, 1, lens)) in
    create_actions (1, 1, tree_sizes) in
  let tbl = A.make 65536 (0, 0) in
  let set_code prev_code (depth, act, data) =
    let code = act prev_code in
    for i = 0 to (1 lsl (16-depth) - 1) do
      tbl.(code lsl (16-depth) + i) <- (depth, data)
    done;
    code
  in L.range 0 size
  |> L.map2 (fun (depth, act) i ->
     (depth, act, int_of_char raw_data.[dht_idx+19 + i])) acts
  |> L.fold_left set_code (-1);
  { dht_type = table_type; dht_id = table_id; dht_tbl = tbl };;

type jpeg_sof_comp = {
    sof_comp_id : int;
    sof_hi : int;
    sof_vi : int;
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
     flip L.map (L.range 0 nf) (fun idx ->
     let pos = sof_idx+8 + idx*3 in
     let (hi, vi) = u4_of_char raw_data (pos+1) in
     { sof_comp_id = int_of_char raw_data.[pos]
     ; sof_hi = hi; sof_vi = vi
     ; sof_tq = int_of_char raw_data.[pos+2] land 0xf} ) in
  { sof_height = height
  ; sof_width = width
  ; sof_comps = comps };;

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
    flip L.map (L.range 0 ns) (fun idx ->
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

let extract_8x8 raw_data (dc_tbl, ac_tbl) start_idx buf =
  let u16_of_bits idx =
    let (arr_idx, bit_idx) = (idx lsr 3, idx land 0x7) in
    (int_of_char raw_data.[arr_idx]  lsl 16 +
     int_of_char raw_data.[arr_idx+1] lsl 8 +
     int_of_char raw_data.[arr_idx+2]) lsl (7 + bit_idx) in
  let get_signed_int bits = function
      0 -> 0
    | len -> let msk = lnot (bits asr 30) in
             ((1 lsl len) lxor msk) + (2 land msk) + (bits asr (31-len)) in
(*  printf "bits : %08x\n" (u16_of_bits start_idx); *)
  let (dc_hufflen, dc_huff) = dc_tbl.(u16_of_bits start_idx lsr 15) in
(*  printf "dc_hufflen = %d, dc_huff = %d\n" dc_hufflen dc_huff; *)
  buf.(0) <- get_signed_int (u16_of_bits (start_idx+dc_hufflen)) dc_huff; (*diff*)
  printf "dc_diff: %d\n" buf.(0);
  let rec extract_ac idx = function
      64 -> idx
    | n -> match ac_tbl.(u16_of_bits idx lsr 15) with
              (ac_hufflen, 0) -> idx + ac_hufflen
            | (ac_hufflen, ac_huff) ->
              let (rrrr, ssss) = (ac_huff lsr 4, ac_huff land 0xf) in
              buf.(n+rrrr) <- get_signed_int (u16_of_bits (idx+ac_hufflen)) ssss;
(*              printf "    ac: %d -> %d\n" (n+rrrr) buf.(n+rrrr); *)
              extract_ac (idx+ac_hufflen+ssss) (n+rrrr+1) in
  extract_ac (start_idx+dc_hufflen+dc_huff) 1;;

type jpeg_info = {
  comp_sizes : int list;
  comp_idxs : int list; (* scanl (+) 0 of comp_sizes *)
  comp_tbls : (int * ((int * int) array * (int * int) array) * int array) array;
  comp_size : int;
  mcu_cnt : int;
  block_cnt : int;
};;

let get_jpeg_info jpg =
  let huff_tbls =
    let dht_sel dht_type dht_id  =
      let predicate tbl = tbl.dht_type == dht_type && tbl.dht_id == dht_id in
      match L.filter predicate jpg.dhts with
        [x] -> x.dht_tbl
      | _ -> raise (Failure "Unknown Huffman (type, id)")
    in L.map (fun c -> ( dht_sel 0 c.sos_dc_sel
                       , dht_sel 1 c.sos_ac_sel)) jpg.sos.sos_comps in
  let quant_tbls =
    let dqt_sel dqt_id =
      let predicate tbl = tbl.dqt_id == dqt_id in
      match L.filter predicate jpg.dqts with
        [x] -> x.dqt_tbl
      | _ -> raise (Failure "Unknown Quantization id")
    in L.map (fun c -> dqt_sel c.sof_tq) jpg.sof.sof_comps in
  let comp_sizes = L.map (fun c -> c.sof_hi*c.sof_vi) jpg.sof.sof_comps in
  let comp_idxs = L.tl (L.scan_left (+) 0 comp_sizes) in
  let comp_tbls = L.map3 (fun a b c -> (a, b, c)) comp_idxs huff_tbls quant_tbls
               |> A.of_list in
  let comp_size = L.sum comp_sizes in
  let mcu_cnt =
    let hmax = L.map (fun c -> c.sof_hi) jpg.sof.sof_comps |> L.maximum in
    let vmax = L.map (fun c -> c.sof_vi) jpg.sof.sof_comps |> L.maximum in
    let hcomps = (jpg.sof.sof_height + 8*hmax - 1)/(8*hmax) in
    let vcomps = (jpg.sof.sof_width  + 8*vmax - 1)/(8*vmax) in
    printf "hcomps=%d,vcomps=%d\n" hcomps vcomps;
    hcomps * vcomps in
  let block_cnt = mcu_cnt*comp_size in
  { comp_sizes = comp_sizes
  ; comp_idxs = comp_idxs
  ; comp_tbls = comp_tbls
  ; comp_size = comp_size
  ; mcu_cnt = mcu_cnt
  ; block_cnt = block_cnt };;

let extract_mcus scan jpg info start_idx =
  let bufs = A.make_matrix info.block_cnt 64 0 in
  printf "comp_size=%d,mcu_cnt=%d\n" info.comp_size info.mcu_cnt;
  printf "bufs length=%d\n" (A.length bufs);
  let rec ext_loop block_idx bit_idx =
    let rec ext_mcu (n, bit_idx) (comp_cnt, huff_tbl, quant_tbl) =
      if n == comp_cnt
        then (n, bit_idx)
        else let next_idx = extract_8x8 scan huff_tbl bit_idx bufs.(block_idx+n) in
             ext_mcu (n+1, next_idx) (comp_cnt, huff_tbl, quant_tbl) in
(*    printf "extract_loop: %d\n" block_idx; *)
    if block_idx == info.block_cnt
      then bit_idx
      else let (_, next_idx) = A.fold_left ext_mcu (0, bit_idx) info.comp_tbls in
           ext_loop (block_idx+info.comp_size) next_idx in
  let next_idx = ext_loop 0 start_idx in
  let rec fix_diff_dc block_idx =
    if block_idx == info.block_cnt
      then ()
      else info.comp_tbls
        |> flip A.fold_left block_idx (fun n (comp_cnt, _, _) ->
           let rec set_acc prev_dc m =
             if m < block_idx+comp_cnt then begin
               bufs.(m).(0) <- bufs.(m).(0) + prev_dc;
               set_acc bufs.(m).(0) (m+1)
             end
           in set_acc 0 n; block_idx+comp_cnt)
        |> fix_diff_dc in
  fix_diff_dc 0;
  (next_idx, bufs);;

(* TODO: parse jpeg using a loop (sequentially); remove jpeg_segmenting *)
let test () =
  let raw_data = jpeg_raw () in
  printf "reading...\n";
  let jpg = parse_jpeg raw_data in
  let info = get_jpeg_info jpg in
  let (next_idx, scan) = extract_scan raw_data jpg.sos.sos_data in
  let (final_idx, bufs) = extract_mcus scan jpg info 0 in
  (final_idx, bufs);;

let zigzag_order () =
  let skew_diag y0 x0 =
    (if (y0+x0) mod 2 == 0 then (fun x -> x) else L.rev)
    (L.map (fun i -> (y0-i, x0+i)) (L.range 0 (min y0 (7-x0) + 1))) in
  L.map2 skew_diag (L.range 0 8@[7;7;7;7;7;7;7]) ([0;0;0;0;0;0;0]@L.range 0 8);;
