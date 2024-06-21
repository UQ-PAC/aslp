#!/bin/bash -e

pattern="${1:?requires regex pattern as first argument}"

output="$(echo ":coverage A64 $pattern" | asli)"

# substitute paths of the form <data:[...]> with ./[...]
echo "$output" | sed 's#<data:\([^>]*\)>#./\1#g'
