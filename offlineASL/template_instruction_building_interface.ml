module type IBI = sig
  type bigint
  type bitvector
  type expr
  type lexpr
  type stmt
  type branch

  val bigint_of_string : string -> bigint
  val bigint_of_int : int -> bigint
  val bigint_zero : bigint
  val bigint_add : bigint -> bigint -> bigint
  val bigint_sub : bigint -> bigint -> bigint
  val bigint_mul : bigint -> bigint -> bigint

  val undefined : unit -> expr
  val mkBits : bigint -> bigint -> bitvector
  val from_bitsLit : string -> bitvector
  val frem_int : bigint -> bigint -> bigint
  val extract_bits : bitvector -> bigint -> bigint -> bitvector

  val f_Elem_set :
    bigint -> bigint -> bitvector -> bigint -> bigint -> bitvector -> bitvector
  (** [f_Elem_set operand_width elem_width operand elem_index elem_width elem]
  *)

  val f_eq_bits : bigint -> bitvector -> bitvector -> bool
  val f_ne_bits : bigint -> bitvector -> bitvector -> bool
  val f_add_bits : bigint -> bitvector -> bitvector -> bitvector
  val f_sub_bits : bigint -> bitvector -> bitvector -> bitvector
  val f_mul_bits : bigint -> bitvector -> bitvector -> bitvector
  val f_and_bits : bigint -> bitvector -> bitvector -> bitvector
  val f_or_bits : bigint -> bitvector -> bitvector -> bitvector
  val f_eor_bits : bigint -> bitvector -> bitvector -> bitvector
  val f_not_bits : bigint -> bitvector -> bitvector
  val f_slt_bits : bigint -> bitvector -> bitvector -> bool
  val f_sle_bits : bigint -> bitvector -> bitvector -> bool
  val f_zeros_bits : bigint -> bitvector
  val f_ones_bits : bigint -> bitvector

  val f_replicate_bits : bigint -> bigint -> bitvector -> bigint -> bitvector
  (** [f_replicate_bits operand_width num_replications operand num_replications]
  *)

  val f_append_bits : bigint -> bigint -> bitvector -> bitvector -> bitvector
  (** [f_append_bits w1 w2 x1 x2] *)

  val f_ZeroExtend : bigint -> bigint -> bitvector -> bigint -> bitvector
  (** [f_ZeroExtend operand_width result_width operand result_width] *)

  val f_SignExtend : bigint -> bigint -> bitvector -> bigint -> bitvector
  (** [f_SignExtend operand_width result_width operand result_width] *)

  val f_lsl_bits : bigint -> bigint -> bitvector -> bitvector -> bitvector
  (** [f_lsl_bits operand_width shift_width operand shift] *)

  val f_lsr_bits : bigint -> bigint -> bitvector -> bitvector -> bitvector
  (** [f_lsr_bits operand_width shift_width operand shift] *)

  val f_asr_bits : bigint -> bigint -> bitvector -> bitvector -> bitvector
  (** [f_asr_bits operand_width shift_width operand shift] *)

  val f_cvt_bits_uint : bigint -> bitvector -> bigint
  (** [f_cvt_bits_uint operand_width operand] *)

  val f_sdiv_int : bigint -> bigint -> bigint
  val f_shl_int : bigint -> bigint -> bigint
  val v_PSTATE_C : expr
  val v_PSTATE_Z : expr
  val v_PSTATE_V : expr
  val v_PSTATE_N : expr
  val v__PC : expr
  val v__R : expr
  val v__Z : expr
  val v_SP_EL0 : expr
  val v_FPSR : expr
  val v_FPCR : expr
  val v_PSTATE_A : expr
  val v_PSTATE_D : expr
  val v_PSTATE_DIT : expr
  val v_PSTATE_F : expr
  val v_PSTATE_I : expr
  val v_PSTATE_PAN : expr
  val v_PSTATE_SP : expr
  val v_PSTATE_SSBS : expr
  val v_PSTATE_TCO : expr
  val v_PSTATE_UAO : expr
  val v_PSTATE_BTYPE : expr
  val v_BTypeCompatible : expr
  val v___BranchTaken : expr
  val v_BTypeNext : expr
  val v___ExclusiveLocal : expr
  val f_switch_context : branch -> unit
  val f_gen_branch : expr -> branch * branch * branch
  val f_true_branch : branch * branch * branch -> branch
  val f_false_branch : branch * branch * branch -> branch
  val f_merge_branch : branch * branch * branch -> branch
  val f_gen_assert : expr -> unit
  val f_gen_bit_lit : 'a -> bitvector -> expr
  val f_gen_bool_lit : bool -> expr
  val f_gen_int_lit : bigint -> expr
  val f_decl_bv : string -> bigint -> expr
  val f_decl_bool : string -> expr
  val f_gen_load : 'a -> 'a
  val f_gen_store : expr -> expr -> unit
  val f_gen_array_load : expr -> bigint -> expr
  val f_gen_array_store : expr -> bigint -> expr -> unit
  val f_gen_Elem_read : bigint -> bigint -> expr -> expr -> expr -> expr
  val f_gen_Elem_set : bigint -> bigint -> expr -> expr -> expr -> expr -> expr
  val f_gen_Mem_set : bigint -> expr -> 'a -> expr -> expr -> unit
  val f_gen_Mem_read : bigint -> expr -> 'a -> expr -> expr
  val f_AtomicStart : unit -> unit
  val f_AtomicEnd : unit -> unit
  val f_gen_AArch64_MemTag_set : 'a -> 'b -> 'c -> unit
  val f_gen_AArch64_MemTag_read : 'a -> 'b -> 'c
  val f_gen_and_bool : expr -> expr -> expr
  val f_gen_or_bool : expr -> expr -> expr
  val f_gen_not_bool : expr -> expr
  val f_gen_cvt_bits_uint : bigint -> expr -> expr
  val f_gen_eq_bits : bigint -> expr -> expr -> expr
  val f_gen_ne_bits : bigint -> expr -> expr -> expr
  val f_gen_not_bits : bigint -> expr -> expr
  val f_gen_cvt_bool_bv : expr -> expr
  val f_gen_or_bits : bigint -> expr -> expr -> expr
  val f_gen_eor_bits : bigint -> expr -> expr -> expr
  val f_gen_and_bits : bigint -> expr -> expr -> expr
  val f_gen_add_bits : bigint -> expr -> expr -> expr
  val f_gen_sub_bits : bigint -> expr -> expr -> expr
  val f_gen_sdiv_bits : bigint -> expr -> expr -> expr
  val f_gen_sle_bits : bigint -> expr -> expr -> expr
  val f_gen_slt_bits : bigint -> expr -> expr -> expr
  val f_gen_mul_bits : bigint -> expr -> expr -> expr
  val f_gen_append_bits : bigint -> bigint -> expr -> expr -> expr
  val f_gen_lsr_bits : bigint -> bigint -> expr -> expr -> expr
  val f_gen_lsl_bits : bigint -> bigint -> expr -> expr -> expr
  val f_gen_asr_bits : bigint -> bigint -> expr -> expr -> expr
  val f_gen_replicate_bits : bigint -> bigint -> expr -> 'a -> expr
  val f_gen_ZeroExtend : bigint -> bigint -> expr -> 'a -> expr
  val f_gen_SignExtend : bigint -> bigint -> expr -> 'a -> expr
  val f_gen_slice : expr -> bigint -> bigint -> expr
  val f_gen_FPCompare : bigint -> expr -> expr -> expr -> expr -> expr
  val f_gen_FPCompareEQ : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPCompareGE : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPCompareGT : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPAdd : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPSub : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPMulAdd : bigint -> expr -> expr -> expr -> expr -> expr
  val f_gen_FPMulAddH : bigint -> expr -> expr -> expr -> expr -> expr
  val f_gen_FPMulX : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPMul : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPDiv : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPMin : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPMinNum : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPMax : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPMaxNum : bigint -> expr -> expr -> expr -> expr
  val f_gen_FPRecpX : bigint -> expr -> expr -> expr
  val f_gen_FPSqrt : bigint -> expr -> expr -> expr
  val f_gen_FPRecipEstimate : bigint -> expr -> expr -> expr
  val f_gen_UnsignedRSqrtEstimate : bigint -> expr -> expr
  val f_gen_FPRSqrtEstimate : bigint -> expr -> expr -> expr
  val f_gen_BFAdd : expr -> expr -> expr
  val f_gen_BFMul : expr -> expr -> expr
  val f_gen_FPConvertBF : expr -> expr -> expr -> expr
  val f_gen_FPRecipStepFused : bigint -> expr -> expr -> expr
  val f_gen_FPRSqrtStepFused : bigint -> expr -> expr -> expr

  val f_gen_FPToFixed :
    bigint -> bigint -> expr -> expr -> expr -> expr -> expr -> expr

  val f_gen_FixedToFP :
    bigint -> bigint -> expr -> expr -> expr -> expr -> expr -> expr

  val f_gen_FPConvert : bigint -> bigint -> expr -> expr -> expr -> expr
  val f_gen_FPRoundInt : bigint -> expr -> expr -> expr -> expr -> expr
  val f_gen_FPRoundIntN : bigint -> expr -> expr -> expr -> expr -> expr
  val f_gen_FPToFixedJS_impl : bigint -> bigint -> expr -> expr -> expr -> expr
end
