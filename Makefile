documentation:
	cd docs && latexmk -pdf -interaction=batchmode documentation.tex

dist: documentation
	cabal sdist

clean:
	cd docs && latexmk -C
	cabal clean

.PHONY: documentation dist clean
