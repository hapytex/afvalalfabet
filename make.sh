#!/bin/bash

mkdir -p out
stack run > out/afvalalfabet.tex
ln *.sty *.cls out
cd out

for i in `seq 5`; do
  pdflatex --interaction=nonstopmode afvalalfabet.tex
  for f in *.adx; do
    fb=$(basename "$f" '.adx')
    makeindex "$f" -o "$fb.and"
  done
  makeindex afvalalfabet
done

rm *.aux *.glo *.idx *.ilg *.ind *.ist *.log *.out *.tex *.sty *.cls *.adx *.and
exit 0
