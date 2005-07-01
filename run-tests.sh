#!/bin/sh
# $Id: run-tests.sh,v 1.2 2005/06/30 22:52:42 chris Exp $

GLOBAL_OPTS="-Werror"

dumped=0
failed=0
passed=0
skipped=0

crunch() {
  while read FOO ; do
    echo $FOO
  done
}

for t in tests/*.mitchell; do
   if [ ! -z "$(grep Disabled $t)" ]; then
      skipped=$(expr $skipped + 1)
      continue
   fi

	LOCAL_OPTS="$(grep Options $t | cut -d':' -f2- | crunch)"
   EXPECTED="$(grep Expected $t | cut -d':' -f2 | crunch)"

	echo -n "$(basename $t)... "

	src/kern/mitchell $GLOBAL_OPTS $LOCAL_OPTS $t 1>&- 2>&-
	retval=$?

	if [ $retval -gt 128 ]; then
      dumped=$(expr $dumped + 1)
		echo "FAIL (CORE DUMPED)"
   elif [ $retval -eq 0 -a $EXPECTED = "PASS" -o \
          $retval -eq 1 -a $EXPECTED = "FAIL" ]; then
		passed=$(expr $passed + 1)
		echo "PASS"
	else
		failed=$(expr $failed + 1)
		echo "FAIL"
	fi
done

echo "---------------"
echo "$passed tests passed"
echo "$failed tests failed"
echo "$dumped tests dumped core"
echo "$skipped tests skipped"
