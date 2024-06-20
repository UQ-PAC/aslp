(* defines the evaluation environment for the bundled Arm spsecifications. *)

let aarch64_asl_dir: string option = 
    List.nth_opt Res.Sites.aslfiles 0

let aarch64_asl_files: (LoadASL.source * LoadASL.source list) option = 
    let aarch64_file_load_order = 
       ["mra_tools/arch/regs.asl"; "mra_tools/types.asl"; "mra_tools/arch/arch.asl"; "mra_tools/arch/arch_instrs.asl"; "mra_tools/arch/regs_access.asl";
        "mra_tools/arch/arch_decode.asl"; "mra_tools/support/aes.asl"; "mra_tools/support/barriers.asl"; "mra_tools/support/debug.asl"; 
        "mra_tools/support/feature.asl"; "mra_tools/support/hints.asl"; "mra_tools/support/interrupts.asl"; "mra_tools/support/memory.asl"; 
        "mra_tools/support/stubs.asl"; "mra_tools/support/fetchdecode.asl"; "tests/override.asl"; "tests/override.prj"]
    in Option.bind aarch64_asl_dir (fun dir ->
        let filenames = List.map (Filename.concat dir) aarch64_file_load_order in
        let prelude = Filename.concat dir "prelude.asl" in
        let prelude = LoadASL.FileSource prelude in
        let filenames = List.map (fun x -> LoadASL.FileSource x) filenames in
        Some (prelude, filenames))

let aarch64_evaluation_environment ?(verbose = false) (): Eval.Env.t option = 
    Option.bind aarch64_asl_files 
        (fun (prelude, filenames) -> Eval.evaluation_environment prelude filenames verbose)

