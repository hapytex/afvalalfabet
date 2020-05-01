#!/bin/bash

mkdir -p out
stack run > out/afvalalfabet.tex
ln *.sty *.cls out
cd out

for i in `seq 5`; do
  pdflatex --interaction=nonstopmode afvalalfabet.tex
  makeindex afvalalfabet
done

rm *.aux *.glo *.idx *.ilg *.ind *.ist *.log *.out *.tex *.sty *.cls
exit 0
