#!/bin/bash -e

set -o pipefail

# echo ":gen A64 aarch64_integer_arithmetic.+ cpp $(pwd)" | dune exec asli
echo ":gen A64 aarch64.+ cpp $(pwd)" | dune exec asli
# dune build
