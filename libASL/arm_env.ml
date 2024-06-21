(* defines the evaluation environment for the bundled Arm spsecifications. *)

let aarch64_asl_dir: string option = 
    None

let prelude_blob : LoadASL.source = DataSource ("prelude.asl", [%blob "../prelude.asl"])

let asl_blobs : LoadASL.source list = [
    DataSource ("mra_tools/arch/regs.asl", [%blob "../mra_tools/arch/regs.asl"]);
    DataSource ("mra_tools/types.asl", [%blob "../mra_tools/types.asl"]);
    DataSource ("mra_tools/arch/arch.asl", [%blob "../mra_tools/arch/arch.asl"]);
    DataSource ("mra_tools/arch/arch_instrs.asl", [%blob "../mra_tools/arch/arch_instrs.asl"]);
    DataSource ("mra_tools/arch/regs_access.asl", [%blob "../mra_tools/arch/regs_access.asl"]);
    DataSource ("mra_tools/arch/arch_decode.asl", [%blob "../mra_tools/arch/arch_decode.asl"]);
    DataSource ("mra_tools/support/aes.asl", [%blob "../mra_tools/support/aes.asl"]);
    DataSource ("mra_tools/support/barriers.asl", [%blob "../mra_tools/support/barriers.asl"]);
    DataSource ("mra_tools/support/debug.asl", [%blob "../mra_tools/support/debug.asl";]);
    DataSource ("mra_tools/support/feature.asl", [%blob "../mra_tools/support/feature.asl"]);
    DataSource ("mra_tools/support/hints.asl", [%blob "../mra_tools/support/hints.asl"]);
    DataSource ("mra_tools/support/interrupts.asl", [%blob "../mra_tools/support/interrupts.asl"]);
    DataSource ("mra_tools/support/memory.asl", [%blob "../mra_tools/support/memory.asl";]);
    DataSource ("mra_tools/support/stubs.asl", [%blob "../mra_tools/support/stubs.asl"]);
    DataSource ("mra_tools/support/fetchdecode.asl", [%blob "../mra_tools/support/fetchdecode.asl"]);
    DataSource ("tests/override.asl", [%blob "../tests/override.asl"]);
    DataSource ("tests/override.prj", [%blob "../tests/override.prj"]);
]

let aarch64_asl_files: (LoadASL.source * LoadASL.source list) option =
    Some (prelude_blob, asl_blobs)


let aarch64_evaluation_environment ?(verbose = false) (): Eval.Env.t option = 
    Option.bind aarch64_asl_files 
        (fun (prelude, filenames) -> Eval.evaluation_environment prelude filenames verbose)

