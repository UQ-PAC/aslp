(* Common config for dis transforms *)
open Asl_ast

(* Memory prims *)
let mem_read_prim     = FIdent("Memory.read",0)
let mem_set_prim      = FIdent("Memory.set",0)

(* Atomic bounds prims *)
let atomic_start_prim = FIdent("AtomicStart",0)
let atomic_end_prim   = FIdent("AtomicEnd",0)

(* Other memory prims TODO: Can't these be in the override? *)
let other_mem_prims = [
  FIdent("AArch64.MemTag.read",0);
  FIdent("AArch64.MemTag.set",0);
]

(* FP prims *)
let fp_prims = [
  FIdent("FPConvert",0);
  FIdent("FPRoundInt",0);
  FIdent("FPRoundIntN",0);
  FIdent("FPToFixed",0);
  FIdent("FixedToFP",0);
  FIdent("FPCompare",0);
  FIdent("FPCompareEQ",0);
  FIdent("FPCompareGE",0);
  FIdent("FPCompareGT",0);
  FIdent("FPToFixedJS_impl",0);
  FIdent("FPSqrt",0);
  FIdent("FPAdd",0);
  FIdent("FPMul",0);
  FIdent("FPDiv",0);
  FIdent("FPMulAdd",0);
  FIdent("FPMulAddH",0);
  FIdent("FPMulX",0);
  FIdent("FPMax",0);
  FIdent("FPMin",0);
  FIdent("FPMaxNum",0);
  FIdent("FPMinNum",0);
  FIdent("FPSub",0);
  FIdent("FPRecpX",0);
  FIdent("FPRecipStepFused",0);
  FIdent("FPRSqrtStepFused",0);
  FIdent("FPRoundBase",0);
  FIdent("FPConvertBF",0);
  FIdent("BFRound",0);
  FIdent("BFAdd",0);
  FIdent("BFMul",0);
  FIdent("FPRecipEstimate",0);
]

let impure_prims =
  [mem_read_prim; mem_set_prim; atomic_start_prim; atomic_end_prim] @
  other_mem_prims @
  fp_prims
