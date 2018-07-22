PROJECT=mbc_msc_thesis
TEX=pdflatex
BIBTEX=bibtex
BUILDTEX=$(TEX) $(PROJECT).tex
DIRS = Chapters

all:
	$(BUILDTEX)
	$(BIBTEX) $(PROJECT)
	$(BUILDTEX)
	$(BUILDTEX)

clean-all:
	rm -f *.dvi *.log *.bak *.aux *.bbl *.blg *.idx *.ps *.eps *.toc *.out *.fdb_latexmk *.fls *.lof *.lot *-blx.bib *.run.xml *~ *.synctex.gz *.pdf

.PHONY: clean

clean:
	rm -f *.dvi *.log *.bak *.aux *.bbl *.blg *.idx *.ps *.eps *.toc *.out *.fdb_latexmk *.fls *.lof *.lot *-blx.bib *.run.xml *.synctex.gz
	$(MAKE) -C $(DIRS) clean
