#!/bin/bash

# performs regression testing based on :coverage of particular groups of instructions

INSTRUCTION_GROUPS="aarch64_integer.+ aarch64_branch.+"
INSTRUCTION_GROUPS+=' aarch64_float_.+'
INSTRUCTION_GROUPS+=' aarch64_vector_.+'
INSTRUCTION_GROUPS+=' aarch64_memory_.+'
ASL_FILES="cpus/armv8.6.cpu"

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

