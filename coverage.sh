#!/bin/bash

# performs regression testing based on :coverage of particular groups of instructions

INSTRUCTION_GROUPS="aarch64_integer.+ aarch64_branch.+"
INSTRUCTION_GROUPS+=' aarch64_vector_arithmetic_unary_\(not\|rbit\|rev\|shift\|cnt\|clsz\)'
INSTRUCTION_GROUPS+=' aarch64_vector_arithmetic_binary_.*_int.*'
INSTRUCTION_GROUPS+=' aarch64_memory_single_general.*'
ASL_FILES="prelude.asl ./mra_tools/arch/regs.asl ./mra_tools/types.asl ./mra_tools/arch/arch.asl ./mra_tools/arch/arch_instrs.asl ./mra_tools/arch/arch_decode.asl ./mra_tools/support/aes.asl ./mra_tools/support/barriers.asl ./mra_tools/support/debug.asl ./mra_tools/support/feature.asl ./mra_tools/support/hints.asl ./mra_tools/support/interrupts.asl ./mra_tools/support/memory.asl ./mra_tools/support/stubs.asl ./mra_tools/support/fetchdecode.asl"
ASL_FILES+=" tests/override.asl"
ASL_FILES+=" tests/override.prj"

COVERAGE_DIR="./tests/coverage"
COVERAGE_TEMP=$(mktemp -d)

MODE=""
if [[ "$1" == "test" ]]; then 
    MODE=test
elif [[ "$1" == "update" ]]; then 
    MODE=update
else 
    echo "requires 'test' or 'update' as first argument."
    exit 1
fi

mkdir -p "$COVERAGE_DIR"
mkdir -p "$COVERAGE_TEMP"


RESULT=0
for inst in $INSTRUCTION_GROUPS; do 
    fname="$(tr -c '[:alnum:]_' _ <<< "$inst")"
    new="$COVERAGE_TEMP/$fname"
    echo "::group::$inst"
    echo "$new"
    time echo ":coverage A64 $inst" | dune exec asli $ASL_FILES > "$new"
    old="$COVERAGE_DIR/$fname"

    if [[ $MODE == update ]]; then
        echo "overwriting coverage results with updated results."
        cp -v "$new" "$old"
    else
        echo "testing coverage with previous results."
        diff -Nu --color=auto "$old" "$new"
        RESULT=$(($RESULT + $?))
    fi
    echo "::endgroup::"
    echo 
done 

if [[ -z "$GITHUB_OUTPUT" ]]; then 
    GITHUB_OUTPUT=/dev/null
fi

echo "OUTPUT=$COVERAGE_TEMP" >> $GITHUB_OUTPUT


exit $RESULT

