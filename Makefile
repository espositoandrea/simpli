docs:
	cd docs && latexmk -pdf -interaction=batchmode documentation.tex

clean-docs:
	cd docs && latexmk -c
	cd docs && rm -f *.bbl *.run.xml

presentation:
	cd docs && libreoffice --convert-to pdf presentation.odp

dist: docs presentation
	cabal sdist

clean: | clean-docs
	cd docs && latexmk -C
	rm -f docs/presentation.pdf
	cabal clean

.PHONY: docs clean-docs dist clean presentation
