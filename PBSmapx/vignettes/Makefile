## twice seems enough for now -- could also use texi2dvi, not portably
../inst/doc/PBSmapx-UG.pdf: PBSmapx-UG.Rnw
	pdflatex PBSmapx-UG.Rnw
	pdflatex PBSmapx-UG.Rnw
	mv PBSmapx-UG.pdf ../inst/doc
	@rm -f PBSmapx-UG.aux PBSmapx-UG.log PBSmapx-UG.out
