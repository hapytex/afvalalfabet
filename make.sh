#!/bin/bash

mkdir -p out
stack run | tee out/afvalwoordenboek_light.tex
stack run -- --dark | tee out/afvalwoordenboek_dark.tex
ln *.sty *.cls out
cd out

for t in dark light; do
  for i in `seq 5`; do
    pdflatex --interaction=nonstopmode "afvalwoordenboek_$t.tex"
    for f in *.adx; do
      fb=$(basename "$f" '.adx')
      makeindex "$f" -o "$fb.and"
    done
    makeindex "afvalwoordenboek_$t"
  done
done

rm *.aux *.glo *.idx *.ilg *.ind *.ist *.log *.out *.tex *.sty *.cls *.adx *.and
exit 0
