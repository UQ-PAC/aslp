#!/bin/bash -e

pattern="${1:?requires regex pattern as first argument}"

# asl_dir="$(asli --aarch64-dir)"
output="$(asloff-coverage "$pattern")"

echo "$output" | sed 's#<data:\([^>]*\)>#./\1#g'
