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

exception Jpeg_format_error of string;;

let jpeg_segmenting raw_data =
  let [ch_ff; ch_00; ch_EOI] = L.map char_of_int [0xff; 0x00; 0xd9]
  in let rec find_candidates acc start_idx =
    try let idx = String.index_from raw_data start_idx ch_ff in
        match raw_data.[idx+1] with
          '\xd9' -> L.rev acc
        | '\x00' -> find_candidates acc (idx+1)
        | '\xff' -> find_candidates acc (idx+1)
        | n when (0xe0 <= int_of_char n && int_of_char n <= 0xef)
              || n == '\xfe' || n == '\xc0' || n == '\xda'
              || n == '\xc4' || n == '\xdb' ->
          find_candidates (idx::acc) (idx+2 + u16_of_char raw_data (idx+2))
        | _ -> find_candidates (idx::acc) (idx+1)
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
  if size mod 65 <> 0
    then raise (Jpeg_format_error "DQT table size is not a multiple of 65")
    else L.map (fun i -> parse_table (dqt_idx+2 + i*65)) (L.range 0 (size/65));;

type jpeg_dht = {
  dht_type: int;                  (* DC = 0, AC = 1 *)
  dht_id: int;                    (* DHT table id *)
  dht_tbl: (int * int) array;     (* (length, data) pairs, index: 16 bit long *)
  dht_inv_tbl: (int * int) array  (* (length, bits) pairs, index: 8 bit long *)
};;

(* parse DHT table to *)
let jpeg_parse_dht raw_data dht_idx =
  let size = dht_idx + u16_of_char raw_data dht_idx in
  let parse_dht idx =
    let (table_type, table_id) = u4_of_char raw_data idx in
    let tree_sizes = flip L.map (L.range 0 16) (fun i ->
                     int_of_char raw_data.[idx+1 + i]) in
    let node_cnt = L.sum tree_sizes in
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
    let inv_tbl = A.make 256 (0, 0) in
    let set_code prev_code (depth, act, data) =
      let code = act prev_code in
      for i = 0 to (1 lsl (16-depth) - 1) do
        tbl.(code lsl (16-depth) + i) <- (depth, data)
      done;
      inv_tbl.(data) <- (depth, code lsl (16-depth));
      code in
    let _ = L.range 0 node_cnt
         |> L.map2 (fun (depth, act) i ->
            (depth, act, int_of_char raw_data.[idx+17 + i])) acts
         |> L.fold_left set_code (-1) in
    (idx+17+node_cnt, { dht_type = table_type
                      ; dht_id = table_id
                      ; dht_tbl = tbl
                      ; dht_inv_tbl = inv_tbl }) in
  let rec parse_dhts idx =
    if idx == size
      then []
      else let (next_idx, tbl) = parse_dht idx in
           tbl::parse_dhts next_idx in
  parse_dhts (dht_idx+2);;

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
  let dhts = L.concat (seg_filter 0xc4 jpeg_parse_dht) in
  let [sof] = seg_filter 0xc0 jpeg_parse_sof in
  let [sos] = seg_filter 0xda jpeg_parse_sos in
  { dqts = dqts; dhts = dhts; sof = sof; sos = sos };;

type jpeg_info = {
  comp_sizes : int list;
  comp_idxs : int list; (* scanl (+) 0 of comp_sizes *)
  comp_tbls : (int * ((int * int) array * (int * int) array) * int array) array;
  comp_inv_tbls : ((int * int) array * (int * int) array) array;
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
        [x] -> (x.dht_tbl, x.dht_inv_tbl)
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
  let comp_tbls = L.map3 (fun a ((b1, _), (b2, _)) c -> (a, (b1, b2), c))
                  comp_idxs huff_tbls quant_tbls
               |> A.of_list in
  let comp_inv_tbls = L.map (fun ((_, b1), (_, b2)) -> (b1, b2)) huff_tbls
                   |> A.of_list in
  let comp_size = L.sum comp_sizes in
  let vmax = L.map (fun c -> c.sof_vi) jpg.sof.sof_comps |> L.maximum in
  let hmax = L.map (fun c -> c.sof_hi) jpg.sof.sof_comps |> L.maximum in
  let vcomps = (jpg.sof.sof_height  + 8*vmax - 1)/(8*vmax) in
  let hcomps = (jpg.sof.sof_width + 8*hmax - 1)/(8*hmax) in
  printf "[+] height=%d,width=%d, vmax=%d,hmax=%d\n" jpg.sof.sof_height jpg.sof.sof_width vmax hmax;
  printf "[+] hcomps=%d,vcomps=%d\n" hcomps vcomps;
  let mcu_cnt =
    hcomps * vcomps in
  let block_cnt = mcu_cnt*comp_size in
  { comp_sizes = comp_sizes
  ; comp_idxs = comp_idxs
  ; comp_tbls = comp_tbls
  ; comp_inv_tbls = comp_inv_tbls
  ; comp_size = comp_size
  ; comps_h = hcomps
  ; comps_v = vcomps
  ; comps_hmax = hmax
  ; comps_vmax = vmax
  ; mcu_cnt = mcu_cnt
  ; block_cnt = block_cnt };;

let quantize info bufs =
  let rec qnt_loop block_idx =
    if block_idx == info.block_cnt
      then block_idx
      else info.comp_tbls
        |> flip A.fold_left block_idx (fun n (comp_cnt, _, quant_tbl) ->
           let rec mul_acc m =
             if m < block_idx+comp_cnt then begin
               A.iteri (fun i q -> bufs.(m).(i) <- bufs.(m).(i)/q) quant_tbl;
               mul_acc (m+1)
             end
           in mul_acc n; block_idx+comp_cnt)
        |> qnt_loop
  in let _ = qnt_loop 0
  in ();;

let zigzag info bufs_flat =
  let zigzag_order =
    let skew_diag y0 x0 =
      (if (y0+x0) mod 2 == 0 then (fun x -> x) else L.rev)
      (L.map (fun i -> (y0-i, x0+i)) (L.range 0 (min y0 (7-x0) + 1))) in
       L.map2 skew_diag (L.range 0 8@[7;7;7;7;7;7;7]) ([0;0;0;0;0;0;0]@L.range 0 8)
    |> L.concat
    |> A.of_list in
  let bufs = A.make_matrix info.block_cnt 64 0 in
  flip A.iteri bufs_flat (fun block_idx block ->
    flip A.iteri zigzag_order (fun idx (y, x) ->
      bufs.(block_idx).(idx) <- block.(y*8+x)));
  bufs;;

let ( +: ) a b = a + b;;
let ( -: ) a b = a - b;;
let ( *: ) a b = (a * b) / 256;;
let f8_of_float f = int_of_float (f *. 256.0 +. 0.5);;
let f8_of_int n = n lsl 8;;
let int_of_f8 n = n / 256;;

let bmp_flatten (bmp : Bitmap.bmp) =
  let height = (bmp.info.height + 7)/8 in
  let width  = (bmp.info.width  + 7)/8 in
  let buf = A.make_matrix (height*width*3) 64 0 in
  for v = 0 to height-1 do
    for h = 0 to width-1 do
      let (y0, x0) = (v*8, h*8) in
      let block_idx = (v*width+h) * 3 in
      for y = 0 to 7 do
        for x = 0 to 7 do
          let (rf, gf, bf) =
            let ystar = bmp.info.height-1-(if y0+y >= bmp.info.height then 0 else y0+y) in
            let xstar = if x0+x >= bmp.info.width  then bmp.info.width -1 else x0+x in
            let (r, g, b) = bmp.bits.(ystar).(xstar) in
            ( float_of_int (int_of_char r - 128)
            , float_of_int (int_of_char g - 128)
            , float_of_int (int_of_char b - 128) ) in
          buf.(block_idx).(y*8+x) <- 0.299*.rf +. 0.587*.gf +. 0.114*.bf |> f8_of_float;
          buf.(block_idx+1).(y*8+x) <- -0.168736*.rf -. 0.331264*.gf +. 0.5*.bf |> f8_of_float;
          buf.(block_idx+2).(y*8+x) <- 0.5*.rf -. 0.418688*.gf -. 0.081312*.bf |> f8_of_float;
        done
      done
    done
  done;
  (height*width, buf);;

let dct bufs_flat =
  let transpose block =
    for i = 0 to 7 do
      for j = i+1 to 7 do
        let t = block.(i*8 + j) in
        block.(i*8 + j) <- block.(j*8 + i);
        block.(j*8 + i) <- t
      done
    done in
  let dct_vecs =
    let pi = acos (-1.0) in
    let normalize_factor = sqrt (2.0 /. 8.0) in
    let angle n k = cos (pi *. (float_of_int n +. 0.5) *. float_of_int k /. 8.0) in
    let cos_vecs f = flip L.map (L.range 0 8) (fun k ->
                       flip L.map (L.range 0 8) (fun n ->
                         f n k *. normalize_factor)) in
    let fix_x0_coef (xs::xss) = L.map (fun a -> a /. sqrt 2.0) xs::xss in
    cos_vecs angle |> fix_x0_coef |> L.map (L.map f8_of_float)
    |> L.map A.of_list |> A.of_list in
  let dots block base_idx =
    let buf = A.make 8 (f8_of_int 0) in
    let dot idx v =
      let rec sumf i acc =
        if i == 8
          then buf.(idx) <- acc
          else sumf (i+1) (acc +: (block.(base_idx+i) *: v.(i))) in
      sumf 0 (f8_of_int 0) in
    A.iteri dot dct_vecs;
    A.blit buf 0 block base_idx 8 in
  printf "    buf init\n%!";
  let bufs_f8s = flip A.map bufs_flat (fun block_flat ->
    let block = A.copy block_flat in
    transpose block;
    block) in
  printf "    dot\n%!";
  flip A.iter bufs_f8s (fun block ->
    for i = 0 to 7 do
      dots block (i*8)
    done);
  printf "    transpose\n%!";
  A.iter transpose bufs_f8s;
  printf "    dot\n%!";
  flip A.iter bufs_f8s (fun block ->
    for i = 0 to 7 do
      dots block (i*8)
    done);
  printf "    blit back\n%!";
  let bufs = A.init (A.length bufs_f8s) (fun i -> A.map int_of_f8 bufs_f8s.(i)) in
  bufs;;

let fix_dc_diff mcu_cnt bufs =
  let rec subtract dc0 dc1 dc2 idx =
    if idx == mcu_cnt*3
      then ()
      else begin
        let ndc0 = bufs.(idx).(0) in
        let ndc1 = bufs.(idx+1).(0) in
        let ndc2 = bufs.(idx+2).(0) in
        bufs.(idx).(0) <- ndc0 - dc0;
        bufs.(idx+1).(0) <- ndc1 - dc1;
        bufs.(idx+2).(0) <- ndc2 - dc2;
        subtract ndc0 ndc1 ndc2 (idx+3)
      end in
  subtract 0 0 0 0;;

type jpeg_buf = {
  jpeg_buf_idx: int;
  jpeg_buf: string;
};;

let make_buf len = { jpeg_buf_idx = 0; jpeg_buf = String.make len '\x00' };;
let set_buf (len, bit16) jpg_buf =
  if len==0 && bit16==0 then begin
    raise (Failure "set_buf got (0,0)")
  end;
  let (arr_idx, bit_idx) = ( jpg_buf.jpeg_buf_idx lsr 3
                           , jpg_buf.jpeg_buf_idx land 7) in
  let mask0 = (bit16 lsr (bit_idx+8)) in
  let mask1 = (bit16 lsr bit_idx) land 0xff in
  let mask2 = (bit16 lsl (8 - bit_idx)) land 0xff in
  let buf = jpg_buf.jpeg_buf in
  buf.[arr_idx] <- char_of_int (int_of_char buf.[arr_idx] lor mask0);
  buf.[arr_idx+1] <- char_of_int (int_of_char buf.[arr_idx+1] lor mask1);
  buf.[arr_idx+2] <- char_of_int (int_of_char buf.[arr_idx+2] lor mask2);
  { jpg_buf with jpeg_buf_idx = jpg_buf.jpeg_buf_idx+len };;

let encode_s m =
  let rec lg2 = function
      1 -> 0
    | n -> 1 + lg2 (n / 2) in
  if m == 0 then (0, 0)
  else let len = 1 + lg2 (abs m) in
       (len, (m + (1 lsl len - 1) land (m asr 30)) lsl (16 - len))

let encode_mcus mcu_cnt info bmp_dct buf_out =
  let ac_tbls = A.map snd info.comp_inv_tbls in
  let dc_tbls = A.map fst info.comp_inv_tbls in
  let rec enc_mcu buf_cur block_idx comp_idx =
    let rec enc_acs buf idx zero_cnt _ac_idx =
      match _ac_idx with
        64 ->
          if zero_cnt <> 0
            then set_buf ac_tbls.(comp_idx).(0) buf
            else buf
      | ac_idx when bmp_dct.(idx).(ac_idx) == 0 ->
        if zero_cnt == 15
          then let new_buf = set_buf ac_tbls.(comp_idx).(0xf0) buf in
               enc_acs new_buf idx 0 (ac_idx+1)
          else enc_acs buf idx (zero_cnt+1) (ac_idx+1)
      | ac_idx ->
        let (len, bits) as s = encode_s bmp_dct.(idx).(ac_idx) in
        let new_buf = buf |> set_buf ac_tbls.(comp_idx).((zero_cnt lsl 4) lor len)
                          |> set_buf s in
        enc_acs new_buf idx 0 (ac_idx+1) in
    if comp_idx == 3
      then buf_cur
      else let block = bmp_dct.(block_idx+comp_idx) in
           let (len, bits) as s = encode_s block.(0) in
           let new_buf0 = set_buf dc_tbls.(comp_idx).(len) buf_cur in
           let new_buf1 = if len==0 then new_buf0 else set_buf s new_buf0 in
           let new_buf2 = enc_acs new_buf1 (block_idx+comp_idx) 0 1 in
           enc_mcu new_buf2 block_idx (comp_idx+1) in
  let rec enc_loop block_idx buf =
    if block_idx == mcu_cnt*3
      then buf
      else enc_mcu buf block_idx 0
        |> enc_loop (block_idx+3) in
  enc_loop 0 buf_out;;

let fix_jpg_info raw_data (bmp : Bitmap.bmp) =
  let rec search idx =
    try let magic = String.index_from raw_data idx '\x05' in
        match raw_data.[magic+1] with
          '\x14' -> magic
        | _ -> search (magic+1)
    with Not_found -> raise (Failure "cannot find magic 514 in jpegdata") in
  let pos = search 0 in
  let char_of_u16 idx u =
    raw_data.[idx] <- char_of_int (u lsr 8);
    raw_data.[idx+1] <- char_of_int (u land 0xff) in
  char_of_u16 pos     bmp.info.height;
  char_of_u16 (pos+2) bmp.info.width;;

let add_ff00 len buf =
  let marked_buf = String.create (len*2) in
  let rec write_marker out_idx in_idx =
    if in_idx == len then out_idx
    else begin
      marked_buf.[out_idx] <- buf.[in_idx];
      if buf.[in_idx] == '\xff'
        then (marked_buf.[out_idx+1] <- '\x00'; write_marker (out_idx+2) (in_idx+1))
        else write_marker (out_idx+1) (in_idx+1)
    end in
  let written_len = write_marker 0 0 in
  (written_len, marked_buf);;

let main () =
  if A.length Sys.argv<2 || A.length Sys.argv>3
    then raise (Invalid_argument "Usage: jpegparser INPUT.JPG [OUT.BMP]");
  let inname = Sys.argv.(1) in
  let outname = if A.length Sys.argv==3 then Sys.argv.(2) else "out.jpg" in
  printf "[+] reading jpegdata...\n%!";
  let jpgraw_data =
    let fin = open_in_bin "jpegdata" in
    let len = in_channel_length fin in
    let data = String.create len in
    really_input fin data 0 len;
    close_in fin;
    data in
  printf "[+] reading file...\n%!";
  let bmp =
    let fin = open_in_bin inname in
    let data = Bitmap.input_bmp fin in
    close_in fin;
    data in
  printf "[+] fixing info...\n%!";
  fix_jpg_info jpgraw_data bmp;
  let jpg = parse_jpeg jpgraw_data in
  let info = get_jpeg_info jpg in
  printf "[+] flatten bmp...\n%!";
  let (mcu_cnt, bmp_flat) = bmp_flatten bmp in
  printf "[+] dct...\n%!";
  let bmp_dct = dct bmp_flat in
  printf "[+] zigzag...\n%!";
  let bmp_linear = zigzag info bmp_dct in
  printf "[+] quantize...\n%!";
  quantize info bmp_linear;
  printf "[+] fixing diff...\n%!";
  fix_dc_diff mcu_cnt bmp_linear;
  let jpeg_buf = make_buf (mcu_cnt*3*64) in
  printf "[+] encode MCUs...\n%!";
  let final_buf = encode_mcus mcu_cnt info bmp_linear jpeg_buf in
  let (bmp_raw_len, bmp_raw) =
    add_ff00 ((final_buf.jpeg_buf_idx+7)/8) final_buf.jpeg_buf in
  printf "result data length: %.2f KB\n%!" (float_of_int ((final_buf.jpeg_buf_idx + 7)/8) /. 1024.0);
  let () =
    let fout = open_out_bin outname in
    output fout jpgraw_data 0 (String.length jpgraw_data-2);
    output fout bmp_raw 0 bmp_raw_len;
    output fout jpgraw_data (String.length jpgraw_data-2) 2;
    close_out fout in
  ();;

main ();;
