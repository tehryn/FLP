simplify-bkg: simplify-bkg.hs
	ghc simplify-bkg.hs

.PHONY clean zip
clean:
	rm -f simplify-bkg.hi simplify-bkg.o simplify-bkg

zip:
	rm -f flp-fun-xmatej52.zip
	zip flp-fun-xmatej52.zip simplify-bkg.hs README Makefile
