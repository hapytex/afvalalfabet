#!/bin/bash

mkdir -p out
stack run | tee out/afvalwoordenboek.tex
ln *.sty *.cls out
cd out

for i in `seq 5`; do
  pdflatex --interaction=nonstopmode afvalwoordenboek.tex
  for f in *.adx; do
    fb=$(basename "$f" '.adx')
    makeindex "$f" -o "$fb.and"
  done
  makeindex afvalwoordenboek
done

rm *.aux *.glo *.idx *.ilg *.ind *.ist *.log *.out *.tex *.sty *.cls *.adx *.and
exit 0
