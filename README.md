# Afvalwoordenboek

[![Build Status](https://travis-ci.com/hapytex/afvalwoordenboek.svg?branch=master)](https://travis-ci.com/hapytex/afvalwoordenboek)

A dictionary how to handle (and recycle) waste in Belgium, in Dutch.

Data is mainly taken from the [*afvalalfabet*](https://www.merelbeke.be/sites/default/files/bijlage/Afvalalfabet.pdf) from the municipality of *Merelbeke*.

You can look at the [**`data.csv`** file](data/data.csv) for a list of
items, and how to get rid of these.

The [**`tips.csv`** file](data/tips.csv) contains a set of *tips and tricks* to avoid
waste. The [**`synonyms.csv`** file](data/synonyms.csv) lists synonyms. These
synonyms will be typesetted a second time under a different name. This can be
useful if the same entry occurs multiple times with a different specification.

If there are still items not on the list, or you want to make a correction, you
can open a *pull request*.

The [*travis build*](travis-ci.com/github/hapytex/afvalwoordenboek) makes a [*rendered version of the dictionary*](https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_light.pdf) [*(dark)*](https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_dark.pdf).

---

Een woordenboek dat beschrijft hoe men afval in België hoort te sorteren. Het is
opgesteld in het Nederlands.

De data komt hoofdzakelijk van het [*afvalalfabet*](https://www.merelbeke.be/sites/default/files/bijlage/Afvalalfabet.pdf) van de gemeente *Merelbeke*.

Het [**`data.csv`** bestand](data/data.csv) bevat een lijst van items en hoe
deze te sorteren.

Het [**`tips.csv`** bestand](data/tips.csv) bevat een lijst met "*tips and
tricks*" hoe afval voorkomen. Het [**`synonyms.csv`** bestand](data/synonyms.csv) bevat
een lijst met synoniemen. Dit is handig wanneer eenzelfde object met
verschillende spcifictaties voorkomt, omdat al die items dan worden gekopieerd
onder een andere naam.

Men kan een *pull request* maken om items toe te voegen of te corrigeren.

De [*travis build*](travis-ci.com/github/hapytex/afvalwoordenboek) maakt een [pdf-versie van de data](https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_light.pdf) [*(donker)*](https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_dark.pdf).

Volledig overzicht versies:

<table>
<thead>
  <tr>
    <th rowspan="2"></th>
    <th colspan="2">met tips</th>
    <th colspan="2">zonder tips</th>
  </tr>
  <tr>
    <th>dialect</th>
    <th colspan="2">zonder dialect</th>
    <th>dialect</th>
  </tr>
</thead>
<tbody>
  <tr>
    <th>donker</th>
    <td><a href="https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_dark.pdf">bestand</a></td>
    <td><a href="https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_dark-no_dialect.pdf">bestand</a></td>
    <td><a href="https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_dark_no-tips_no-dialect.pdf">bestand</a></td>
    <td><a href="https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_dark_no-tips.pdf">bestand</a></td>
  </tr>
  <tr>
    <th>licht</th>
    <td><a href="https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_light.pdf">bestand</a></td>
    <td><a href="https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_light-no_dialect.pdf">bestand</a></td>
    <td><a href="https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_light_no-tips_no-dialect.pdf">bestand</a></td>
    <td><a href="https://hapytex.github.io/afvalwoordenboek/afvalwoordenboek_light_no-tips.pdf">bestand</a></td>
  </tr>
</tbody>
</table>
