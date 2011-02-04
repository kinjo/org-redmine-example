#!/bin/sh
EMACS=emacs
OPTIONS="-L . -L ~/.emacs.d/org-mode/lisp"
OUTPUT=/tmp/.el-expectations
$EMACS -q --no-site-file --batch $OPTIONS -l org-redmine -l el-expectations -f batch-expectations $OUTPUT "$@"
ret=$?
cat $OUTPUT
rm $OUTPUT
exit $rete
