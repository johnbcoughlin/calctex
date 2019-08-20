images = $(addprefix resources/, 2a.png 4a.png 2a_lowres.png)

clean :
	rm $(images)

reference_images : $(images)

resources/%.dvi : resources/%.tex
	pwd && latex -interaction nonstopmode -output-directory resources $<

resources/%_lowres.png : resources/%.dvi
	dvipng $< -o $@ -T tight -D 150

resources/%.png : resources/%.dvi
	dvipng -fg "rgb 0 0 0" -bg "rgb 1 1 1" -D 429 -T tight -o $@ $<

test: reference_images
	emacs -batch -L `pwd` -l ert.el -l calctex-test.el -f toggle-debug-on-error -f ert-run-tests-batch-and-exit

