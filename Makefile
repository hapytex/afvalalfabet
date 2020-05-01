afvalalfabet.pdf : dictionary.sty data/data.csv
	stack run > afvalalfabet.tex
	pdflatex --interaction=nonstopmode afvalalfabet.tex || true
	makeindex afvalalfabet
	pdflatex --interaction=nonstopmode afvalalfabet.tex || true
	makeindex afvalalfabet
	pdflatex --interaction=nonstopmode afvalalfabet.tex || true
	rm afvalafvalbet.tex
