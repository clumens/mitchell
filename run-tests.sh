#!/bin/sh

GLOBAL_OPTS="-Werror"

passed=0
failed=0
dumped=0

for t in tests/*.mitchell; do
	LOCAL_OPTS="$(grep Options $t | cut -d':' -f2-)"

	echo -n "$(basename $t)... "

	src/kern/mitchell $GLOBAL_OPTS $LOCAL_OPTS $t 1>&- 2>&-
	retval=$?

	if [ $retval -gt 128 ]; then
      dumped=$(expr $dumped + 1)
		echo "FAIL (CORE DUMPED)"
	elif [ $retval -eq 0 -a ! -z "$(grep PASS $t)" -o  \
	       $retval -eq 1 -a ! -z "$(grep FAIL $t)" ]; then
		passed=$(expr $passed + 1)
		echo "PASS"
	else
		failed=$(expr $failed + 1)
		echo "FAIL"
	fi
done

echo "----------"
echo "$passed tests passed"
echo "$failed tests failed"
echo "$dumped tests dumped core"
