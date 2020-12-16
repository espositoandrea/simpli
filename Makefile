docs:
	cd docs && latexmk -pdf -interaction=batchmode documentation.tex

clean-docs:
	cd docs && latexmk -c
	cd docs && rm -f *.bbl *.run.xml *.snm *.vrb *.nav

presentation:
	cd docs && latexmk -pdf -interaction=batchmode presentation.tex

dist: | docs presentation
	cabal clean
	cabal sdist
	cd dist-newstyle/sdist && \
		tar -xzf *.tar.gz && \
		cd simpli-* && zip -qr ../andrea-esposito-parser.zip .

clean: | clean-docs
	cd docs && latexmk -C
	cabal clean

.PHONY: docs clean-docs dist clean presentation
