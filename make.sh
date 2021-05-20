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

buildpdf () {
  bn=$(basename "$1" '.tex')
  for i in `seq 5`; do
    $latexc --interaction=nonstopmode "$1"
    for f in *.adx; do
      fb=$(basename "$f" '.adx')
      $midx "$f" -o "$fb.and" || true
    done
    makeindex "$bn" || true
  done
  rm *.adx
  pdftk "$bn.pdf" update_info meta.txt output "out-$bn.pdf"
  mv -f "out-$bn.pdf" "$bn.pdf"
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
echo "afvalwoordenboek.django-antipatterns.com" > CNAME
echo '<!DOCTYPE html><meta charset="utf-8"><title>Redirecting to /afvalwoordenboek_dark.pdf</title><meta http-equiv="refresh" content="0; URL=/afvalwoordenboek_dark.pdf"><link rel="canonical" href="/afvalwoordenboek_dark.pdf">' > index.html
cat > meta.txt <<EOF
InfoKey: Title
InfoValue: Afval-sorteer-woordenboek
InfoKey: Subject
InfoValue: recycleren van afval
InfoKey: Author
InfoValue: Willem Van Onsem and Lindsey Louwyck
InfoKey: Keywords
InfoValue: afval, recyclage, pmd
EOF

for fn in *.tex; do
  buildpdf "$fn" &
done
wait

rm *.aux *.glo *.idx *.ilg *.ind *.ist *.log *.out *.tex *.sty *.cls *.adx *.and *.ttf meta.txt

exit 0
