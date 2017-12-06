#!/bin/bash

module=main
example_base=${1:-'example'}

echo "Reading from file(s) \'${example_base}NN.in\', executing module \'$module\'..."

let i=1
let failCount=0
while [[ -e "${example_base}${i}.in" ]]; do
  echo -n "$i: ... "
  actual=$(runhaskell $module < "${example_base}${i}.in")
  expected=$(cat "${example_base}${i}.out")
  if [[ "$actual" = "$expected" ]]; then
    echo PASS
  else
    echo "FAIL ($actual != $expected)"
    let failCount=$failCount+1
  fi

  let i=$i+1
done
exit $failCount
