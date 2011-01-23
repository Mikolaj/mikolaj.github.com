#!/bin/sh
#
# Copyright (C) 2005 Pawel Findeisen
#
# This file is part of the Dule compiler.
# The Dule compiler is released under the GNU General Public License (GPL).
# Please see the file Dule-LICENSE for license information.
#
# $Id: performance.sh,v 1.4 2006-11-08 19:00:18 mikon Exp $
# 

rm ../test/performance.time.tmp
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
do
 echo "===============    $i    ======   $1   ===============";
 ocaml $1 $i > ../test/performance.case.tmp;

if echo "== $i == $1 ==" >> ../test/performance.time.tmp; \
(bash -c "time ../code/dule --no-prelude -c ../test/performance.case.tmp") 2>> ../test/performance.time.tmp; \
then echo "OK"; \
else echo "Error in this test case!"; exit 1; fi

done

