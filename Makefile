test:
	emacs -batch -L `pwd` -l ert.el -l calctex-test.el -f toggle-debug-on-error -f ert-run-tests-batch-and-exit

