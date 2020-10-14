#!/bin/bash

# constants
latexc='lualatex'
midx='makeindex'

makename () {
  local i="$1"
  if [ ! -z "$i" ]; then
    name="_$i"
    flag="--$i"
  else
    name=''
    flag=''
  fi
}

mkdir -p out
ln -f *.sty *.cls out
ln -f fonts/* out

for i in no-tips ''; do
  makename "$i"; naa="$name"; fla="$flag"
  for j in no-dialect ''; do
    makename "$j"; nab="$name"; flb="$flag"
    for k in dyslexic ''; do
      makename "$k"; nac="$name"; flb="$flag"
      stack run -- "$fla" "$flb" > "out/afvalwoordenboek_light$naa$nab$nac".tex
      stack run -- --dark "$fla" "$flb" > "out/afvalwoordenboek_dark$naa$nab$nac.tex"
    done
  done
done
cd out

for fn in *.tex; do
  bn=$(basename "$fn" '.tex')
  for i in `seq 5`; do
    $latexc --interaction=nonstopmode "$fn"
    for f in *.adx; do
      fb=$(basename "$f" '.adx')
      $midx "$f" -o "$fb.and" || true
    done
    makeindex "$bn" || true
  done
  rm *.adx
done

rm *.aux *.glo *.idx *.ilg *.ind *.ist *.log *.out *.tex *.sty *.cls *.adx *.and *.ttf

exit 0
