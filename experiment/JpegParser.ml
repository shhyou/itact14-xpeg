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

let fst3 (a, _, _) = a;;

(* jpeg parse *)

let inname = "..\phw_jpeg\monalisa.jpg";; (* Sys.argv.(1) *)
let () = printf "reading file...\n";;
let jpeg_raw =
  let fin = open_in_bin inname in
  let len = in_channel_length fin in
  printf "String.create...\n";
  let data = String.create len in
  printf "really_input...\n";
  really_input fin data 0 len;
  close_in fin;
  fun () -> data;;
let () = printf "reading done\n";;

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
(*  printf "dc_diff: %d\n" buf.(0); *)
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
  comps_h : int;
  comps_v : int;
  comps_hmax : int;
  comps_vmax : int;
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
  let hmax = L.map (fun c -> c.sof_hi) jpg.sof.sof_comps |> L.maximum in
  let vmax = L.map (fun c -> c.sof_vi) jpg.sof.sof_comps |> L.maximum in
  let hcomps = (jpg.sof.sof_height + 8*hmax - 1)/(8*hmax) in
  let vcomps = (jpg.sof.sof_width  + 8*vmax - 1)/(8*vmax) in
  printf "height=%d,width=%d, vmax=%d,hmax=%d\n" jpg.sof.sof_height jpg.sof.sof_width vmax hmax;
  printf "hcomps=%d,vcomps=%d\n" hcomps vcomps;
  let mcu_cnt =
    hcomps * vcomps in
  let block_cnt = mcu_cnt*comp_size in
  { comp_sizes = comp_sizes
  ; comp_idxs = comp_idxs
  ; comp_tbls = comp_tbls
  ; comp_size = comp_size
  ; comps_h = hcomps
  ; comps_v = vcomps
  ; comps_hmax = hmax
  ; comps_vmax = vmax
  ; mcu_cnt = mcu_cnt
  ; block_cnt = block_cnt };;

let blit_plane jpg info bufs_idct =
  let buf = A.make_matrix jpg.sof.sof_height jpg.sof.sof_width (0,0,0) in
  let vi_hi = L.map (fun c -> (c.sof_vi, c.sof_hi)) jpg.sof.sof_comps
           |> A.of_list in
  let blit_mcu y0 x0 block_idx =
    for y = 0 to info.comps_vmax*8 do
      for x = 0 to info.comps_hmax*8 do
        if  y0+y<jpg.sof.sof_height && x0+x<jpg.sof.sof_width then begin
          let get_val comp_idx =
            let yreal = y*fst vi_hi.(comp_idx)/info.comps_vmax in
            let xreal = x*snd vi_hi.(comp_idx)/info.comps_hmax in
            let (v, y8) = (yreal / 8, yreal mod 8) in
            let (h, x8) = (xreal / 8, xreal mod 8) in
            let n = if comp_idx > 0
                      then fst3 info.comp_tbls.(comp_idx-1)
                      else 0 in
            let m = v*snd vi_hi.(comp_idx) + h in
            printf "  (%d,%d) -> (%d+%d+%d,%d,%d)\n" y x block_idx n m y8 x8; 
            bufs_idct.(block_idx+n+m).(y8).(x8) in
          printf "(%d,%d)" (y0+y) (x0+x);
          buf.(y0+y).(x0+x) <- (get_val 0, get_val 1, get_val 2)
        end
      done
    done in
  for v = 0 to info.comps_v do
    for h = 0 to info.comps_h do
      blit_mcu (v*info.comps_vmax*8) (h*info.comps_hmax*8)
               ((v*info.comps_h + h)*info.comp_size)
    done
  done;
  buf;;

let extract_mcus scan info start_idx =
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
  let fix_diff_dc comp_idx =
    let begin_ = if comp_idx == 0 then 0 else fst3 info.comp_tbls.(comp_idx-1) in
    let end_ = fst3 info.comp_tbls.(comp_idx) in
    let rec fix_loop prev_dc block_idx =
      if block_idx < info.block_cnt then begin
        let rec set_acc n dc =
          if n == end_
            then dc
            else begin
              bufs.(block_idx+n).(0) <- bufs.(block_idx+n).(0) + dc;
              set_acc (n+1) bufs.(block_idx+n).(0)
            end in
        let next_dc = set_acc begin_ prev_dc in
        fix_loop next_dc (block_idx+info.comp_size)
      end in
    fix_loop 0 0 in
  for idx = 0 to 2 do
    fix_diff_dc idx;
  done;
  (next_idx, bufs);;

let dequantize info bufs =
  let rec deq_loop block_idx =
(*    printf "block_idx=%d\n" block_idx; *)
    if block_idx == info.block_cnt
      then block_idx
      else info.comp_tbls
        |> flip A.fold_left block_idx (fun n (comp_cnt, _, quant_tbl) ->
           let rec mul_acc m =
             if m < block_idx+comp_cnt then begin
               A.iteri (fun i q -> bufs.(m).(i) <- bufs.(m).(i)*q) quant_tbl;
               mul_acc (m+1)
             end
           in mul_acc n; block_idx+comp_cnt)
        |> deq_loop
  in let _ = deq_loop 0
  in ();;

let unzigzag info bufs_flat =
  let zigzag_order =
    let skew_diag y0 x0 =
      (if (y0+x0) mod 2 == 0 then (fun x -> x) else L.rev)
      (L.map (fun i -> (y0-i, x0+i)) (L.range 0 (min y0 (7-x0) + 1))) in
       L.map2 skew_diag (L.range 0 8@[7;7;7;7;7;7;7]) ([0;0;0;0;0;0;0]@L.range 0 8)
    |> L.concat
    |> A.of_list in
  let bufs = A.init info.block_cnt (fun _ -> A.make_matrix 8 8 0) in
  flip A.iteri bufs_flat (fun block_idx block ->
    flip A.iteri zigzag_order (fun idx (y, x) ->
      bufs.(block_idx).(y).(x) <- block.(idx)));
  bufs;;

let idct info bufs_8x8s =
  let idct1_vecs =
    let pi = acos (-1.0) in
    let normalize_factor = sqrt (2.0 /. 8.0) in
    let angle n k = cos (pi *. (float_of_int n +. 0.5) *. float_of_int k /. 8.0) in
    let cos_vecs f = flip L.map (L.range 0 8) (fun k ->
                       flip L.map (L.range 0 8) (fun n ->
                         f n k *. normalize_factor)) in
    let fix_x0_coef (_::xs) = sqrt (1.0 /. 2.0) *. normalize_factor::xs in
       L.map fix_x0_coef (cos_vecs (fun n k -> angle k n))
    |> L.map A.of_list
    |> A.of_list in
  let idct vec =
    let dot u v =
      let rec sumf i acc =
        if i == 8
          then acc
          else sumf (i+1) (acc +. u.(i) *. v.(i)) in
      sumf 0 0.0 in
    A.map (dot vec) idct1_vecs in
  let bufs_float = A.init info.block_cnt (fun i ->
    A.init 8 (fun j ->
      A.init 8 (fun k ->
        float_of_int bufs_8x8s.(i).(k).(j)))) in
  flip A.iter bufs_float (fun block ->
    A.iteri (fun i col -> block.(i) <- idct col) block);
  A.iter A.transpose bufs_float;
  flip A.iter bufs_float (fun block ->
    A.iteri (fun i row -> block.(i) <- idct row) block);
  let bufs = A.init info.block_cnt (fun i ->
    A.init 8 (fun j ->
      A.init 8 (fun k ->
        int_of_float (bufs_float.(i).(j).(k) +. 0.5)))) in
  bufs;;

let rgb_conv jpg bufs_ycbcr bufs =
  flip A.iteri bufs_ycbcr (fun y row ->
    flip A.iteri row (fun x (yi, cbi, cri) ->
      let [yf; cbf; crf] = L.map float_of_int [yi; cbi; cri] in
      let rf = yf +. 1.402 *. crf in
      let gf = yf -. 0.34414 *. cbf -. 0.71414 *. crf in
      let bf = yf +. 1.772 *. cbf in
      let fix_shift f =
        let m = 128 + int_of_float f in
        if m < 0 then 0
        else if m > 255 then 255
        else m in
      let [rc; gc; bc] = L.map fix_shift [rf; gf; bf]
                      |> L.map char_of_int in
      bufs.(jpg.sof.sof_height - 1 - y).(x) <- (rc, gc, bc)));;

(* TODO: parse jpeg using a loop (sequentially); remove jpeg_segmenting *)
let test () =
  let raw_data = jpeg_raw () in
  printf "reading...\n";
  let jpg = parse_jpeg raw_data in
  let info = get_jpeg_info jpg in
  printf "extract_scan...\n";
  let (next_idx, scan) = extract_scan raw_data jpg.sos.sos_data in
  printf "extract_mcus...\n";
  let (final_idx, bufs_flat) = extract_mcus scan info 0 in
  printf "dequantize...\n";
  dequantize info bufs_flat;
  printf "unzigzag...\n";
  let bufs_8x8s = unzigzag info bufs_flat in
  printf "idct...\n";
  let bufs_idct = idct info bufs_8x8s in
  printf "blit_plane...\n";
  let bufs_ycbcr = blit_plane jpg info bufs_idct in
  let bmp = Bitmap.make jpg.sof.sof_height jpg.sof.sof_width in
  printf "rvb_conv...\n";
  rgb_conv jpg bufs_ycbcr bmp.bits;
  let fout = open_out_bin "out.bmp" in
  Bitmap.output_bmp fout bmp;
  close_out fout;
  bmp;;

test ();;
