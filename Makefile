docs:
	cd docs && latexmk -pdf -interaction=batchmode documentation.tex

clean-docs:
	cd docs && latexmk -c
	cd docs && rm -f *.bbl *.run.xml

dist: docs
	cabal sdist

clean: | clean-docs
	cd docs && latexmk -C
	cabal clean

.PHONY: docs clean-docs dist clean
