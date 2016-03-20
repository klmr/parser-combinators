.PHONY: test
test:
	Rscript __init__.r

.PHONY: testing
testing:
	Rscript -e 'library("testthat"); modules::import(".", TRUE); auto_test(".", "_tests")'
