#!/bin/bash -e

set -o pipefail

# echo ":gen A64 aarch64_integer_arithmetic.+ cpp $(pwd)" | dune exec asli
dir="$(dirname "$0")"
cd "$dir"
# echo ":gen A64 aarch64.+ cpp ./subprojects" | dune exec asli

CXX=$(which clang++) CXX_LD=$(which ld.lld) meson setup --reconfigure build
pushd build
meson compile
DESTDIR=$(pwd)/prefix meson install
popd
