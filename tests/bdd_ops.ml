open LibASL
open Asl_utils
open AST
open LibASL.Primops
open LibASL.Utils

module TC  = Tcheck
module AST = Asl_ast

let man = MLBDD.init ~cache:1024 () 


let from_bitsLit x =
  let x' = Value.drop_chars x ' ' in
  (Primops.mkBits (String.length x') (Z.of_string_base 2 x'))

let it i = assert (MLBDD.is_true i || MLBDD.is_false i ) ; MLBDD.is_true i  

let bdd_to_bv (b:MLBDD.t list) = let l = List.map (fun i -> 
    assert (MLBDD.is_true i || MLBDD.is_false i ) ; MLBDD.is_true i) b  
  in  let l = List.map (function 
      | true -> '1'
      | false -> '0') l |> List.to_seq |> String.of_seq in
  from_bitsLit l

let prim_cvt_bits_str    (x: bitvector): string =
    let n = Z.of_int x.n in
    if Z.equal n Z.zero then begin
        "''"
    end else begin
        let s = Z.format "%0b" x.v in
        let pad = String.make (Z.to_int n - String.length s) '0' in
        pad ^ s
    end


let bv_to_bdd (bv:bitvector) = 
  let l = prim_cvt_bits_str bv in 
  let l = String.to_seq l |> List.of_seq  in
  List.map (function 
      | '1' -> MLBDD.dtrue man 
      | '0' -> MLBDD.dfalse man
      | x -> failwith (Printf.sprintf "no %c" x) ) l

let ppbit = prim_cvt_bits_str



let test_add =  
  let width = 8 in
  let cases = List.init 256  (fun i -> (i - 128))  in
  let cases_bits = List.map (fun i -> mkBits width (Z.of_int i)) cases in
  let cases_bdd = List.map bv_to_bdd cases_bits  in
  let compar = List.concat_map (fun i -> List.map (fun j -> i , j) cases_bits) cases_bits in
  let to_check = List.concat_map (fun i -> List.map (fun j -> i , j) cases_bdd) cases_bdd in
  let res_add = List.map2 (fun (i,j) (i', j') -> (i, j), prim_add_bits i j, bdd_to_bv (LibASL.Transforms.BDDSimp.signed_add_wrap i' j')) compar to_check in 
  List.iter (fun ((i,j),c,r) -> Alcotest.(check string) (Printf.sprintf "%s + %s = %s" (ppbit i) (ppbit j) (ppbit c)) (prim_cvt_bits_str c) (prim_cvt_bits_str r)) res_add ;

  let res_sub = List.map2 (fun (i,j) (i', j') -> (i, j), prim_sub_bits i j, bdd_to_bv (LibASL.Transforms.BDDSimp.signed_sub_wrap man i' j')) compar to_check in 
  List.iter (fun ((i,j),c,r) -> Alcotest.(check string) (Printf.sprintf "%s - %s = %s" (ppbit i) (ppbit j) (ppbit c)) (prim_cvt_bits_str c) (prim_cvt_bits_str r)) res_sub ; 

  let compare_binop format f1 f2 = 
    let res = List.map2 (fun (i,j) (i', j') -> (i, j), f1 i j, (f2 i' j')) compar to_check in 
    List.iter (fun ((i,j),c,r) -> Alcotest.(check bool) (Printf.sprintf format (ppbit i) (ppbit j) (c)) (c) r) res in
  compare_binop "%s > %s = %b" (fun x y -> prim_gt_int (Primops.prim_cvt_bits_uint x) (Primops.prim_cvt_bits_uint y)) (fun x y -> it (LibASL.Transforms.BDDSimp.bvugt man x y)) ;
  compare_binop "%s < %s = %b" (fun x y -> prim_lt_int (Primops.prim_cvt_bits_sint x) (Primops.prim_cvt_bits_sint y)) (fun x y -> it (LibASL.Transforms.BDDSimp.bvslt man x y)) ;
  compare_binop "%s <= %s = %b" (fun x y -> prim_le_int (Primops.prim_cvt_bits_sint x) (Primops.prim_cvt_bits_sint y)) (fun x y -> it (LibASL.Transforms.BDDSimp.bvsle man x y)) ;






