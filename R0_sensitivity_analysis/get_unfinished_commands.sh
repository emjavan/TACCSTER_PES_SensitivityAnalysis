#!/bin/bash
# extract unfinished commands

input_file="queuestate"
output_file="unfinished_commands.txt"

sed '/completed/q' "$input_file" \
  | grep -v -E '^(queued|running|completed)$' \
  | cut -d':' -f2- \
  | sed 's/^[[:space:]]*//' \
  > "$output_file"

