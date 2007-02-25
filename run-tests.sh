#!/bin/bash

# Options to pass to the mitchell compiler for all tests.
GLOBAL_OPTS="--Werror"

failed=0
passed=0
skipped=0

# The result of our test - 1 = pass, 2 = fail
result=0

crunch() {
  while read FOO ; do
    echo $FOO
  done
}

for t in tests/*.mitchell; do
   # First, skip tests that are disabled.
   if [[ "$(grep '# Disabled' $t)" != "" ]]; then
      skipped=$(expr $skipped + 1)
      continue
   fi

   # Extract information about the test from it.
   LOCAL_OPTS="$(grep '# Options' $t | cut -d':' -f2- | crunch)"
   EXPECTED="$(grep '# Expected' $t | cut -d':' -f2 | crunch)"

   echo -n "$(basename $t)... "

   # Run and capture the return code.
   errmsg=$(src/kern/mitchell $GLOBAL_OPTS $LOCAL_OPTS $t 2>&1)
   retval=$?

   # Interpret return code.
   if [[ $retval == 0 && $EXPECTED == "PASS" ]]; then
      result=1
   elif [[ $retval == 1 && $EXPECTED == "FAIL" ]]; then
      # If we were supposed to fail, check to see what reason was given and if
      # it matches what we got from the compiler.
      HOW_FAIL="$(grep '# HowFail' $t | cut -d':' -f2- | crunch)"

      if [[ "$errmsg" != "" && "$HOW_FAIL" != "" && \
            "$(echo "$errmsg" | grep "$HOW_FAIL")" != "" ]]; then
         result=1
      else
         result=2
      fi
   else
      result=2
   fi

   # Now print the result of the test.
   case $result in
      1) (( passed++ ))
         echo "PASS"
         ;;

      2) (( failed++ ))
         echo "FAIL"
         ;;
   esac
done

echo "---------------"
echo "$passed tests passed"
echo "$failed tests failed"
echo "$skipped tests skipped"
