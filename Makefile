images = $(addprefix resources/, 2a.png 4a.png 2a_lowres.png joules.png)

clean :
	rm $(images)

clean_workdir :
	rm -rf workdir* && mkdir workdir

reference_images : $(images)

resources/%.dvi : resources/%.tex
	pwd && latex -interaction nonstopmode -output-directory resources $<

resources/%_lowres.png : resources/%.dvi
	dvipng $< -o $@ -T tight -D 150

resources/%.png : resources/%.dvi
	dvipng -fg "rgb 0 0 0" -bg "rgb 1 1 1" -D 428 -T tight -o $@ $<

test: calctex_test org_calctex_test calctex_contrib_test

calctex_test: clean_workdir reference_images
	emacs -batch -L `pwd` -l color.el -l ert.el -l test-utils.el -l test/calctex-test.el \
		--eval="(setq calctex-test-resources-dir \"$$(pwd)/resources\")" \
		--eval="(setq temporary-file-directory \"$$(pwd)/workdir\")" \
		-f toggle-debug-on-error \
		-f ert-run-tests-batch-and-exit

org_calctex_test: clean_workdir reference_images resources/test.org
	emacs -batch -L `pwd` -l color.el -l org.el -l ert.el -l test-utils.el -l test/org-calctex-test.el \
		--eval="(setq calctex-test-resources-dir \"$$(pwd)/resources\")" \
		--eval="(setq temporary-file-directory \"$$(pwd)/workdir\")" \
		-f toggle-debug-on-error \
		-f ert-run-tests-batch-and-exit

calctex_contrib_test: calctex-contrib-test.el
	emacs -batch -L `pwd` -l ert.el -l calctex-contrib-test.el \
		-f ert-run-tests-batch-and-exit

vendor/texd/bin/dvichop: vendor/texd/dvichop.c vendor/texd/dviop.h
	cd vendor/texd && mkdir -p bin && make dvichop && mv dvichop bin

vendor/texd: vendor/texd/dvichop.sty vendor/texd/bin/dvichop

pkg: vendor/texd calctex.el calctex-pkg.el
	mkdir -p dist && tar -c -f dist/calctex-0.1 calctex.el calctex-pkg.el vendor/texd
