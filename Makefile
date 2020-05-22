calctex_images = $(addprefix calctex/test/resources/, 2a.png 4a.png 2a_lowres.png joules.png)

clean :
	rm $(images)

clean_workdir :
	rm -rf workdir* && mkdir workdir

calctex/reference_images : $(calctex_images)

calctex/test/resources/%.dvi : calctex/test/resources/%.tex
	pwd && latex -interaction nonstopmode -output-directory calctex/test/resources $<

calctex/test/resources/%_lowres.png : calctex/test/resources/%.dvi
	dvipng $< -o $@ -T tight -D 150

calctex/test/resources/%.png : calctex/test/resources/%.dvi
	dvipng -fg "rgb 0 0 0" -bg "rgb 1 1 1" -D 428 -T tight -o $@ $<

test: calctex_test org_calctex_test calctex_contrib_test

calctex_test: clean_workdir calctex/reference_images
	export TEXD_DIR=$$(cd vendor/texd; pwd); cd calctex; cask exec ert-runner test/calctex-test.el

org_calctex_test: clean_workdir reference_images resources/test.org
	emacs -batch -L `pwd` -l color.el -l org.el -l ert.el -l test/test-helper.el -l test/org-calctex-test.el \
		--eval="(setq calctex-test-resources-dir \"$$(pwd)/resources\")" \
		--eval="(setq temporary-file-directory \"$$(pwd)/workdir\")" \
		-f toggle-debug-on-error \
		-f ert-run-tests-batch-and-exit

calctex_contrib_test: calctex-contrib-test.el
	emacs -batch -L `pwd` -l ert.el -l calctex-contrib-test.el \
		-f ert-run-tests-batch-and-exit

vendor/texd:
	cd vendor && git clone git@github.com:johnbcoughlin/texd.git

vendor/texd/bin/dvichop: vendor/texd vendor/texd
	cd vendor/texd && mkdir -p bin && make dvichop && mv dvichop bin

pkg: vendor/texd calctex.el calctex-pkg.el
	mkdir -p dist && tar -c -f dist/calctex-0.1 calctex.el calctex-pkg.el vendor/texd


foo:
	export TEXD_DIR=$$(cd vendor/texd; pwd); echo $$TEXD_DIR
