LISP_PROG := @LISP_PROGRAM@
LISP_FLAGS := --no-userinit --non-interactive

makefile_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

lisp_eval := $(LISP_PROG) $(LISP_FLAGS) \
             --eval '(require "asdf")' \
             --eval '(asdf:load-asd "$(makefile_dir)/chil.asd")' \
             --eval

lisp_quit := --eval '(uiop:quit)'

chil_files := chil.asd $(shell find . -type f -name '*.lisp')

.PHONY: build
build: $(chil_files)
	$(lisp_eval) '(asdf:compile-system :chil :force t)'

chil_tests := chil.asd $(shell find ./tests/ -type f -name '*.lisp')
.PHONY: check
check: $(chil_files) $(chil_tests)
	$(lisp_eval) '(asdf:test-system :chil :force t)'

include doc/local.mk
