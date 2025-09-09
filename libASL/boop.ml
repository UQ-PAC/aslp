
module type LifterInterface = sig
  type expr
  type lexpr
  type stmt
  type integer = Z.t
  type bv
  val undefined : unit -> expr
  val f_eq_bits : integer -> bv -> bv -> bool
  val f_and_bits : integer -> bv -> bv -> bv
  val f_add_bits : integer -> bv -> bv -> bv
  val from_bitsLit : string -> bv
  val mkBits : integer -> bv -> expr
  val extract_bits : bv -> integer -> integer -> bv
  val f_gen_eq_bits : integer -> expr -> expr -> expr
  (* val f_gen_add_bits : integer -> expr -> expr -> expr *)
  val f_gen_add_bits : integer -> expr -> expr -> expr
  val f_append_bits : integer -> integer -> bv -> bv -> bv
  val f_SignExtend : integer -> integer -> bv -> integer -> bv
  val f_ZeroExtend : integer -> integer -> bv -> integer -> bv
  val f_gen_and_bool : expr -> expr -> expr
  val f_gen_not_bool : expr -> expr
  val f_gen_load : lexpr -> expr
  val f_gen_bit_lit : integer -> bv -> expr
  val f_gen_bool_lit : bool -> expr
  val f_gen_store : lexpr -> expr -> stmt
  val f_gen_if : expr -> stmt list -> stmt list -> stmt
  val v_PSTATE_Z : lexpr
  val v_PSTATE_C : lexpr
  val v_PSTATE_V : lexpr
  val v_PSTATE_N : lexpr
  val v__PC : lexpr
  val v___BranchTaken : lexpr

end




let f_aarch64_branch_conditional_cond (type expr stmt bv) (module L : LifterInterface with type expr = expr and type stmt = stmt and type bv = bv) v_enc v_pc  =
  let open L in
  begin
    let v_ConditionHolds1__2_copyprop = ref (undefined ()) in
    let v_result__2_copyprop = ref (undefined ()) in
    if f_eq_bits (Z.of_string "32") (f_and_bits (Z.of_string "32") (v_enc) (from_bitsLit "00000000000000000000000000001110")) (from_bitsLit "00000000000000000000000000000000") then
    begin
      v_result__2_copyprop := f_gen_eq_bits (Z.of_string "1") (f_gen_load (v_PSTATE_Z)) (f_gen_bit_lit (Z.of_string "1") (from_bitsLit "1"))
    end
    else
    begin
      if f_eq_bits (Z.of_string "32") (f_and_bits (Z.of_string "32") (v_enc) (from_bitsLit "00000000000000000000000000001110")) (from_bitsLit "00000000000000000000000000000010") then
      begin
        v_result__2_copyprop := f_gen_eq_bits (Z.of_string "1") (f_gen_load (v_PSTATE_C)) (f_gen_bit_lit (Z.of_string "1") (from_bitsLit "1"))
      end
      else
      begin
        if f_eq_bits (Z.of_string "32") (f_and_bits (Z.of_string "32") (v_enc) (from_bitsLit "00000000000000000000000000001110")) (from_bitsLit "00000000000000000000000000000100") then
        begin
          v_result__2_copyprop := f_gen_eq_bits (Z.of_string "1") (f_gen_load (v_PSTATE_N)) (f_gen_bit_lit (Z.of_string "1") (from_bitsLit "1"))
        end
        else
        begin
          if f_eq_bits (Z.of_string "32") (f_and_bits (Z.of_string "32") (v_enc) (from_bitsLit "00000000000000000000000000001110")) (from_bitsLit "00000000000000000000000000000110") then
          begin
            v_result__2_copyprop := f_gen_eq_bits (Z.of_string "1") (f_gen_load (v_PSTATE_V)) (f_gen_bit_lit (Z.of_string "1") (from_bitsLit "1"))
          end
          else
          begin
            if f_eq_bits (Z.of_string "32") (f_and_bits (Z.of_string "32") (v_enc) (from_bitsLit "00000000000000000000000000001110")) (from_bitsLit "00000000000000000000000000001000") then
            begin
              v_result__2_copyprop := f_gen_and_bool (f_gen_eq_bits (Z.of_string "1") (f_gen_load (v_PSTATE_C)) (f_gen_bit_lit (Z.of_string "1") (from_bitsLit "1"))) (f_gen_eq_bits (Z.of_string "1") (f_gen_load (v_PSTATE_Z)) (f_gen_bit_lit (Z.of_string "1") (from_bitsLit "0")))
            end
            else
            begin
              if f_eq_bits (Z.of_string "32") (f_and_bits (Z.of_string "32") (v_enc) (from_bitsLit "00000000000000000000000000001110")) (from_bitsLit "00000000000000000000000000001010") then
              begin
                v_result__2_copyprop := f_gen_eq_bits (Z.of_string "1") (f_gen_load (v_PSTATE_N)) (f_gen_load (v_PSTATE_V))
              end
              else
              begin
                if f_eq_bits (Z.of_string "32") (f_and_bits (Z.of_string "32") (v_enc) (from_bitsLit "00000000000000000000000000001110")) (from_bitsLit "00000000000000000000000000001100") then
                begin
                  v_result__2_copyprop := f_gen_and_bool (f_gen_eq_bits (Z.of_string "1") (f_gen_load (v_PSTATE_N)) (f_gen_load (v_PSTATE_V))) (f_gen_eq_bits (Z.of_string "1") (f_gen_load (v_PSTATE_Z)) (f_gen_bit_lit (Z.of_string "1") (from_bitsLit "0")))
                end
                else
                begin
                  v_result__2_copyprop := f_gen_bool_lit (true)
                end
              end
            end
          end
        end
      end
    end;
    if ((f_eq_bits (Z.of_string "32") (f_and_bits (Z.of_string "32") (v_enc) (from_bitsLit "00000000000000000000000000001001")) (from_bitsLit "00000000000000000000000000000001")) || (f_eq_bits (Z.of_string "32") (f_and_bits (Z.of_string "32") (v_enc) (from_bitsLit "00000000000000000000000000000101")) (from_bitsLit "00000000000000000000000000000001"))) || (f_eq_bits (Z.of_string "32") (f_and_bits (Z.of_string "32") (v_enc) (from_bitsLit "00000000000000000000000000000011")) (from_bitsLit "00000000000000000000000000000001")) then
    begin
      v_ConditionHolds1__2_copyprop := f_gen_not_bool (!v_result__2_copyprop)
    end
    else
    begin
      v_ConditionHolds1__2_copyprop := !v_result__2_copyprop
    end;
    [
      f_gen_if (!v_ConditionHolds1__2_copyprop)
      begin
        [
          f_gen_store (v___BranchTaken) (f_gen_bool_lit (true));
          f_gen_store (v__PC) (f_gen_bit_lit (Z.of_string "64") (f_add_bits (Z.of_string "64") (v_pc) (f_SignExtend (Z.of_string "21") (Z.of_string "64") (f_append_bits (Z.of_string "19") (Z.of_string "2") (extract_bits (v_enc) (Z.of_string "5") (Z.of_string "19")) (from_bitsLit "00")) (Z.of_string "64"))))
        ]
      end
      begin
        []
      end
    ]
  end



let f_A64_decoder (type expr stmt bv) (module L : LifterInterface with type expr = expr and type stmt = stmt and type bv = bv) v_enc v_pc =
  let open L in
  if (true) && ((f_eq_bits (Z.of_string "5") (f_and_bits (Z.of_string "5") (extract_bits (v_enc) (Z.of_string "24") (Z.of_string "5")) (from_bitsLit "11110")) (from_bitsLit "00000")) && (true)) then begin
    failwith "unsupported"
  end else if (true) && ((f_eq_bits (Z.of_string "5") (extract_bits (v_enc) (Z.of_string "24") (Z.of_string "5")) (from_bitsLit "00011")) && (true)) then begin
    failwith "unsupported"
  end else if (true) && ((f_eq_bits (Z.of_string "5") (f_and_bits (Z.of_string "5") (extract_bits (v_enc) (Z.of_string "24") (Z.of_string "5")) (from_bitsLit "11110")) (from_bitsLit "00100")) && (true)) then begin
    failwith "unsupported"
  end else if (true) && ((f_eq_bits (Z.of_string "5") (f_and_bits (Z.of_string "5") (extract_bits (v_enc) (Z.of_string "24") (Z.of_string "5")) (from_bitsLit "11110")) (from_bitsLit "00110")) && (true)) then begin
    failwith "unsupported"
  end else if (true) && ((f_eq_bits (Z.of_string "5") (f_and_bits (Z.of_string "5") (extract_bits (v_enc) (Z.of_string "24") (Z.of_string "5")) (from_bitsLit "11100")) (from_bitsLit "10000")) && (true)) then begin
    failwith "unsupported"
  end else if (true) && ((f_eq_bits (Z.of_string "5") (f_and_bits (Z.of_string "5") (extract_bits (v_enc) (Z.of_string "24") (Z.of_string "5")) (from_bitsLit "11100")) (from_bitsLit "10100")) && (true)) then begin
    if (f_eq_bits (Z.of_string "3") (extract_bits (v_enc) (Z.of_string "29") (Z.of_string "3")) (from_bitsLit "010")) && ((true) && ((f_eq_bits (Z.of_string "14") (f_and_bits (Z.of_string "14") (extract_bits (v_enc) (Z.of_string "12") (Z.of_string "14")) (from_bitsLit "10000000000000")) (from_bitsLit "00000000000000")) && ((true) && (true)))) then begin
      let v_o1 = extract_bits (v_enc) (Z.of_string "24") (Z.of_string "1") in
      let v_o0 = extract_bits (v_enc) (Z.of_string "4") (Z.of_string "1") in
      if (f_eq_bits (Z.of_string "1") (v_o1) (from_bitsLit "0")) && (f_eq_bits (Z.of_string "1") (v_o0) (from_bitsLit "0")) then begin
        f_aarch64_branch_conditional_cond (module L) (v_enc) (v_pc)
      end else if (f_eq_bits (Z.of_string "1") (v_o1) (from_bitsLit "0")) && (f_eq_bits (Z.of_string "1") (v_o0) (from_bitsLit "1")) then begin
        failwith "unsupported"
      end else if (f_eq_bits (Z.of_string "1") (v_o1) (from_bitsLit "1")) && (true) then begin
        failwith "unsupported"
      end else begin
        failwith "unsupported"
      end
    end else if (f_eq_bits (Z.of_string "3") (extract_bits (v_enc) (Z.of_string "29") (Z.of_string "3")) (from_bitsLit "110")) && ((true) && ((f_eq_bits (Z.of_string "14") (f_and_bits (Z.of_string "14") (extract_bits (v_enc) (Z.of_string "12") (Z.of_string "14")) (from_bitsLit "11000000000000")) (from_bitsLit "00000000000000")) && ((true) && (true)))) then begin
      failwith "unsupported"
    end else if (f_eq_bits (Z.of_string "3") (extract_bits (v_enc) (Z.of_string "29") (Z.of_string "3")) (from_bitsLit "110")) && ((true) && ((f_eq_bits (Z.of_string "14") (extract_bits (v_enc) (Z.of_string "12") (Z.of_string "14")) (from_bitsLit "01000000110010")) && ((true) && (f_eq_bits (Z.of_string "5") (extract_bits (v_enc) (Z.of_string "0") (Z.of_string "5")) (from_bitsLit "11111"))))) then begin
      failwith "unsupported"
    end else if (f_eq_bits (Z.of_string "3") (extract_bits (v_enc) (Z.of_string "29") (Z.of_string "3")) (from_bitsLit "110")) && ((true) && ((f_eq_bits (Z.of_string "14") (extract_bits (v_enc) (Z.of_string "12") (Z.of_string "14")) (from_bitsLit "01000000110011")) && ((true) && (true)))) then begin
      failwith "unsupported"
    end else if (f_eq_bits (Z.of_string "3") (extract_bits (v_enc) (Z.of_string "29") (Z.of_string "3")) (from_bitsLit "110")) && ((true) && ((f_eq_bits (Z.of_string "14") (f_and_bits (Z.of_string "14") (extract_bits (v_enc) (Z.of_string "12") (Z.of_string "14")) (from_bitsLit "11111110001111")) (from_bitsLit "01000000000100")) && ((true) && (true)))) then begin
      failwith "unsupported"
    end else if (f_eq_bits (Z.of_string "3") (extract_bits (v_enc) (Z.of_string "29") (Z.of_string "3")) (from_bitsLit "110")) && ((true) && ((f_eq_bits (Z.of_string "14") (f_and_bits (Z.of_string "14") (extract_bits (v_enc) (Z.of_string "12") (Z.of_string "14")) (from_bitsLit "11110110000000")) (from_bitsLit "01000010000000")) && ((true) && (true)))) then begin
      failwith "unsupported"
    end else if (f_eq_bits (Z.of_string "3") (extract_bits (v_enc) (Z.of_string "29") (Z.of_string "3")) (from_bitsLit "110")) && ((true) && ((f_eq_bits (Z.of_string "14") (f_and_bits (Z.of_string "14") (extract_bits (v_enc) (Z.of_string "12") (Z.of_string "14")) (from_bitsLit "11110100000000")) (from_bitsLit "01000100000000")) && ((true) && (true)))) then begin
      failwith "unsupported"
    end else if (f_eq_bits (Z.of_string "3") (extract_bits (v_enc) (Z.of_string "29") (Z.of_string "3")) (from_bitsLit "110")) && ((true) && ((f_eq_bits (Z.of_string "14") (f_and_bits (Z.of_string "14") (extract_bits (v_enc) (Z.of_string "12") (Z.of_string "14")) (from_bitsLit "10000000000000")) (from_bitsLit "10000000000000")) && ((true) && (true)))) then begin
      failwith "unsupported"
    end else if (f_eq_bits (Z.of_string "3") (f_and_bits (Z.of_string "3") (extract_bits (v_enc) (Z.of_string "29") (Z.of_string "3")) (from_bitsLit "011")) (from_bitsLit "000")) && ((true) && ((true) && ((true) && (true)))) then begin
      failwith "unsupported"
    end else if (f_eq_bits (Z.of_string "3") (f_and_bits (Z.of_string "3") (extract_bits (v_enc) (Z.of_string "29") (Z.of_string "3")) (from_bitsLit "011")) (from_bitsLit "001")) && ((true) && ((f_eq_bits (Z.of_string "14") (f_and_bits (Z.of_string "14") (extract_bits (v_enc) (Z.of_string "12") (Z.of_string "14")) (from_bitsLit "10000000000000")) (from_bitsLit "00000000000000")) && ((true) && (true)))) then begin
      failwith "unsupported"
    end else if (f_eq_bits (Z.of_string "3") (f_and_bits (Z.of_string "3") (extract_bits (v_enc) (Z.of_string "29") (Z.of_string "3")) (from_bitsLit "011")) (from_bitsLit "001")) && ((true) && ((f_eq_bits (Z.of_string "14") (f_and_bits (Z.of_string "14") (extract_bits (v_enc) (Z.of_string "12") (Z.of_string "14")) (from_bitsLit "10000000000000")) (from_bitsLit "10000000000000")) && ((true) && (true)))) then begin
      failwith "unsupported"
    end else begin
      failwith "unsupported"
    end
  end else if (true) && ((f_eq_bits (Z.of_string "5") (f_and_bits (Z.of_string "5") (extract_bits (v_enc) (Z.of_string "24") (Z.of_string "5")) (from_bitsLit "01010")) (from_bitsLit "01000")) && (true)) then begin
    failwith "unsupported"
  end else if (true) && ((f_eq_bits (Z.of_string "5") (f_and_bits (Z.of_string "5") (extract_bits (v_enc) (Z.of_string "24") (Z.of_string "5")) (from_bitsLit "01110")) (from_bitsLit "01010")) && (true)) then begin
    failwith "unsupported"
  end else if (true) && ((f_eq_bits (Z.of_string "5") (f_and_bits (Z.of_string "5") (extract_bits (v_enc) (Z.of_string "24") (Z.of_string "5")) (from_bitsLit "01110")) (from_bitsLit "01110")) && (true)) then begin
    failwith "unsupported"
  end else begin
    failwith "unsupported"
  end


