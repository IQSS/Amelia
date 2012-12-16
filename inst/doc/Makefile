REVAL=TRUE

all:	amelia.pdf

%.tex:	%.Rnw
	echo "Sweave(\"\$<\", debug=TRUE, eval=\$(REVAL))" | R --slave

%.pdf:	%.tex
	pdflatex \$*
	bibtex \$*
	pdflatex \$*
	pdflatex \$*
	pdflatex \$*
