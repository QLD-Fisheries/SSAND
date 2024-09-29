template.pdf : template.Rtex
	Rscript -e 'knitr::knit("template.Rtex")'
	pdflatex template
	biber template # --configfile bib/biber.conf
	pdflatex template
	git clean -x -f -e "*.pdf" -e "*.tex"
	open template.pdf

clean :
	git clean -f -d
