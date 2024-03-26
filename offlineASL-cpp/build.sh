#!/bin/bash -e

set -o pipefail

# echo ":gen A64 aarch64_integer_arithmetic.+ cpp $(pwd)" | dune exec asli
cd "$(dirname "$0")"/..
echo ":gen A64 aarch64.+ cpp" | dune exec asli
# dune build
