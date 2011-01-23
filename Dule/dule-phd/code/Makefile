# Copyright (C) 2003 Mikolaj Konarski
#
# This file is part of the Dule compiler.
# The Dule compiler is released under the GNU General Public License (GPL).
# Please see the file Dule-LICENSE for license information.
#
# $Id: Makefile,v 1.68 2007-08-25 13:21:30 mikon Exp $

SOURCES := \
hashcons.mli hashcons.ml \
\
tools.ml error_rep.ml \
\
core_back.ml core_middle.ml core_front.ml \
\
middle_middle.ml \
\
mod_back.ml mod_middle.ml mod_front.ml \
\
main_common.ml parser.mly lexer.mll main.ml

all: nc #debug-code #nc #bc #pnc
RESULT := dule

OCAMLFLAGS := -dtypes #-noassert #-dtypes 
OCAMLNCFLAGS := -inline 100

OCAMLC := ocamlc.opt
OCAMLOPT := ocamlopt.opt

.PHONY: test
test: 
	bash -c "time ./dule --no-prelude --composition \
	../test/strange_commented.dul"

.PHONY: test-debugging
test-debugging: 
	bash -c "time ./dule --no-prelude --composition --debugging \
	../test/strange_commented.dul"

.PHONY: test-good
test-good: 
	bash -c "time ./dule --composition ../test/strange_commented.dul \
	--overwriting ../test/regression_short.dul \
	--no-composition --no-overwriting ../test/tutorial_modules.dul"

.PHONY: test-complex
test-complex: 
	bash -c "time ./dule --complex-structures --composition ../test/strange_commented.dul \
	--overwriting ../test/regression_short.dul \
	--no-composition --no-overwriting ../test/tutorial_modules.dul"

.PHONY: test-good-no-execution
test-good-no-execution: 
	bash -c "time ./dule --no-execution \
	--composition ../test/strange_commented.dul \
	--overwriting ../test/regression_short.dul \
	--no-composition --no-overwriting ../test/tutorial_modules.dul"

.PHONY: test-anonymous
test-anonymous: 
	bash -c "time ./dule ../test/anonymous.dul"

.PHONY: tests-all
tests-all: 
	bash -c "time ./dule -c ../test/tutorial_core.dul -c ../test/core.dul -c ../test/core2.dul && \
	time ./dule --no-prelude --composition ../test/strange_commented.dul && \
	time ./dule --no-prelude --composition --overwriting ../test/regression_short.dul && \
	time ./dule ../test/tutorial_modules.dul && \
	time ./dule --overwriting ../test/old_compiler1.dul && \
	time ./dule --no-prelude --composition --overwriting ../test/regression_long.dul && \
	time ./dule --overwriting ../test/old_compiler2.dul && \
	time ./dule --overwriting ../test/old_compiler3.dul && \
	time ./dule --overwriting ../test/old_compiler4.dul && \
	time ./dule ../test/compiler.dul && \
	time ./dule ../test/anonymous.dul "

.PHONY: tests-all-no-execution
tests-all-no-execution: 
	bash -c "time ./dule --no-verification --no-execution -c ../test/tutorial_core.dul -c ../test/core.dul -c ../test/core2.dul && \
	time ./dule --no-verification --no-execution --no-prelude --composition ../test/strange_commented.dul && \
	time ./dule --no-verification --no-execution --no-prelude --composition --overwriting ../test/regression_short.dul && \
	time ./dule --no-verification --no-execution ../test/tutorial_modules.dul && \
	time ./dule --no-verification --no-execution --overwriting ../test/old_compiler1.dul && \
	time ./dule --no-verification --no-execution --no-prelude --composition --overwriting ../test/regression_long.dul && \
	time ./dule --no-verification --no-execution --overwriting ../test/old_compiler2.dul && \
	time ./dule --no-verification --no-execution --overwriting ../test/old_compiler3.dul && \
	time ./dule --no-verification --no-execution --overwriting ../test/old_compiler4.dul && \
	time ./dule --no-verification --no-execution ../test/compiler.dul && \
	time ./dule --no-verification --no-execution ../test/anonymous.dul "

.PHONY: tests-all-no-fixpoints
tests-all-no-fixpoints: 
	bash -c "time ./dule --no-fixpoints -c ../test/tutorial_core.dul -c ../test/core.dul -c ../test/core2.dul && \
	time ./dule --no-fixpoints --no-prelude --composition ../test/strange_commented.dul && \
	time ./dule --no-fixpoints --no-prelude --composition --overwriting ../test/regression_short.dul && \
	time ./dule --no-fixpoints ../test/tutorial_modules.dul && \
	time ./dule --no-fixpoints --overwriting ../test/old_compiler1.dul && \
	time ./dule --no-fixpoints --no-prelude --composition --overwriting ../test/regression_long.dul && \
	time ./dule --no-fixpoints --overwriting ../test/old_compiler2.dul && \
	time ./dule --no-fixpoints --overwriting ../test/old_compiler3.dul && \
	time ./dule --no-fixpoints --overwriting ../test/old_compiler4.dul && \
	time ./dule --no-fixpoints ../test/compiler.dul && \
	time ./dule --no-fixpoints ../test/anonymous.dul "

.PHONY: test-all-at-once
test-all-at-once: 
	bash -c "time ./dule -c ../test/tutorial_core.dul -c ../test/core.dul -c ../test/core2.dul \
	--composition ../test/strange_commented.dul \
	--overwriting ../test/regression_short.dul \
	../test/regression_long.dul \
	--no-composition --no-overwriting ../test/tutorial_modules.dul \
	--overwriting ../test/old_compiler1.dul \
	../test/old_compiler2.dul \
	../test/old_compiler3.dul \
	../test/old_compiler4.dul \
	--no-overwriting ../test/compiler.dul \
	../test/anonymous.dul "

.PHONY: test-tutorial-core
test-tutorial-core: 
	bash -c "time ./dule -c ../test/tutorial_core.dul"

.PHONY: test-core
test-core: 
	bash -c "time ./dule -c ../test/core.dul -c ../test/core2.dul"

.PHONY: test-strange
test-strange: 
	bash -c "time ./dule --no-prelude --composition \
	../test/strange_commented.dul"

.PHONY: test-short
test-short: 
	bash -c "time ./dule --no-prelude --composition --overwriting \
	../test/regression_short.dul"

.PHONY: test-long
test-long: 
	bash -c "time ./dule --no-prelude --composition --overwriting \
	../test/regression_long.dul"

.PHONY: test-tutorial
test-tutorial: 
	bash -c "time ./dule ../test/tutorial_modules.dul"

.PHONY: test-compiler
test-compiler: 
	bash -c "time ./dule ../test/compiler.dul"

.PHONY: test-compiler-no-execution
test-compiler-no-execution:
	bash -c "time ./dule --no-verification --no-pp --no-execution \
	../test/compiler.dul"

.PHONY: test-compiler-no-fixpoints
test-compiler-no-fixpoints: 
	bash -c "time ./dule --no-verification --no-pp --no-fixpoints \
	../test/compiler.dul"

.PHONY: test-errors
test-errors: 
	for i in ../test/error_reporting/*.dul; do \
          if echo; ./dule --no-prelude --composition $$i; \
		then echo; echo "The file "; echo $$i; \
                     echo "is not erroneous! Aborting!"; \
                     break; fi \
	done

.PHONY: test-performance
test-performance: 
	bash -c "time for j in ../test/performance/*.ml; do if ! ../test/performance.sh \$$j; then break; fi; done"

ll:     top
	$(OCAMLC) -I +camlp4 -pp "camlp4o pa_extend.cmo q_MLast.cmo" \
	-c ll_parser.ml


OCAMLMAKEFILE := OCamlMakefile
-include $(OCAMLMAKEFILE)
