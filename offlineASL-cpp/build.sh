#!/bin/bash

set -o pipefail

GEN_DIR="${GEN_DIR:-$(dirname $0)/../../aslp-lifter-cpp}"
set -xe

[[ -d "$GEN_DIR" ]]
if [[ -z "$CXX" ]]; then
  export CXX=$(command -v clang++)
fi

echo ":gen A64 .+ cpp $GEN_DIR/subprojects/aslp-lifter" | dune exec asli

cd $GEN_DIR
rm -rf build

meson setup build $MESONFLAGS
meson compile -C build
