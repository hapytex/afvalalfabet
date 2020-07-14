#!/bin/bash

# constants
latexc='lualatex'
midx='makeindex'

makename () {
  local i="$1"
  if [ ! -z "$i" ]; then
    name="-$i"
    flag="--$i"
  else
    name=''
    flag=''
  fi
}

mkdir -p out
ln -f *.sty *.cls out

for i in no-tips ''; do
  makename "$i"; naa="$name"; fla="$flag"
  for j in no-dialect ''; do
    makename "$j"; nab="$name"; flb="$flag"
    stack run -- "$fla" "$flb" | tee "out/afvalwoordenboek_light$naa$nab".tex
    stack run -- --dark "$fla" "$flb" | tee "out/afvalwoordenboek_dark$naa$nab.tex"
  done
done
cd out

for fn in *.tex; do
  bn=$(basename "$fn" '.tex')
  for i in `seq 5`; do
    $latexc --interaction=nonstopmode "$fn" >/dev/null 2>/dev/null
    for f in *.adx; do
      fb=$(basename "$f" '.adx')
      $midx "$f" -o "$fb.and" || true
    done
    makeindex "$bn" || true
  done
done

rm *.aux *.glo *.idx *.ilg *.ind *.ist *.log *.out *.tex *.sty *.cls *.adx *.and

exit 0
