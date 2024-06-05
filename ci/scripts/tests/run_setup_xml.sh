#!/usr/bin/env bash

output="testdata/output.txt"

rm -f "${output}"
export ACCOUNT=foo
echo "ACCOUNT=$ACCOUNT" > $output

../../../workflow/setup_xml.py "${1}"