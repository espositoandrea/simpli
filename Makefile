docs:
	cd docs && latexmk -pdf -interaction=batchmode documentation.tex

clean-docs:
	cd docs && latexmk -c
	cd docs && rm -f *.bbl *.run.xml *.snm *.vrb *.nav

presentation:
	cd docs && latexmk -pdf -interaction=batchmode presentation.tex

dist: docs presentation
	cabal sdist

clean: | clean-docs
	cd docs && latexmk -C
	cabal clean

.PHONY: docs clean-docs dist clean presentation
