4 Randomisierung

# Randomisierung[^9]

Zufallszahlen sind eine tolle Möglichkeit, Deine Musik interessant zu 
gestalten. Sonic Pi bietet einige Funktionen, um Zufallsfaktoren in 
Deine Musik einzubauen. Aber bevor wir starten, müssen wir noch einer 
schockierenden Wahrheit ins Gesicht sehen: In Sonic Pi bedeutet 
*zufällig nicht wirklich zufällig*. Was zum Teufel soll das bedeuten? 
Nun, das verrate ich Dir jetzt.

## Wiederholbarkeit

Eine wirklich nützliche Zufallsfunktion ist `rrand`. Sie liefert Dir 
einen zufälligen Wert zwischen zwei Zahlen - einem *Minimal-* und einem 
*Maximalwert*. (`rrand` ist ein Kürzel für das englische *ranged 
random*, also eine Zufallszahl innerhalb eines bestimmten 
Wertebereichs).

```
play rrand(50, 95)
```

Oh, eine zufällige Note wird gespielt. Es war die Note `83.7527`. Eine 
nette Note zwischen 50 und 100. Aber hallo, habe ich gerade diese 
angeblich zufällige Note exakt vorhergesagt? Da ist doch etwas nicht 
ganz astrein. Lasse den Code noch einmal ablaufen. Wieder `83.7527`, 
oder? Das kann doch kein Zufall sein!

Die Antwort ist, es ist nicht wirklich zufällig, sondern 
pseudo-zufällig. Sonic Pi liefert Dir Reihenfolgen von Zufallszahlen, 
die wiederholbar sind. Das ist sehr nützlich, denn so ist
sichergestellt, dass die Musik von Deinem Rechner auf anderen Rechnern
identisch klingt - sogar dann, wenn Du einen Zufallsfaktor einbaust.

Klar, wenn in einem bestimmten Musikstück jedesmal die `83.7527` als 
'zufällige' Zahl gewählt würde, dann wäre das nicht besonders 
interessant. Aber so ist es auch nicht. Versuch folgendes:

```
loop do
  play rrand(50, 95)
  sleep 0.5
end 
```

Jawohl! Nun klingt es zufällig. Innerhalb eines bestimmten 
Code-Durchgangs liefern Aufrufe von Zufallsfunktionen auch zufällige 
Werte. Der nächste Durchgang wird jedoch genau die selbe Folge von 
Zufallswerten liefern und also auch genau gleich klingen. Es ist, als 
ob der Code immer zu demselben Zeitpunkt zurückspringt, wenn 
der Ausführen-Button geklickt wird. Es ist der Groundhog-Day[^10] der 
musikalischen Synthese.

## Ruhelose Glocken

Ein großartiges Beispiel von Zufall in Aktion bietet der
"Haunted Bells"-Code. Die "ruhelosen Glocken" spielen das Sample
`:perc_bell` mit einer zufälligen Samplerate und Pausenzeit
in einer Endlosschleife[^11] ab:

```
loop do
  sample :perc_bell, rate: (rrand 0.125, 1.5)
  sleep rrand(0.2, 2)
end
```

## Zufällig abschneiden (random cutoff)

Ein anderes spannendes Beispiel für die Randomisierung ist das 
zufällige Abschneiden[^12] eines Synth-Klangs. Der `:tb303`-Emulator ist 
ein guter Synth, um das auszuprobieren:

```
use_synth :tb303

loop do
  play 50, release: 0.1, cutoff: rrand(60, 120)
  sleep 0.125
end
```

## Startpunkt der Zufallsfolge (random seed)

Was aber, wenn Du die Abfolge von Zufallszahlen, die Sonic Pi Dir 
liefert, nicht magst? Nun, mit `use_random_seed`[^13] kannst Du 
unterschiedliche Startpunkte für diese Folge angeben. Der 
Standard-Startpunkt ist die 0. Wähle also einfach einen anderen 
Startpunkt und mache eine andere Zufallserfahrung!

Sieh Dir den folgenden Code an:

```
5.times do
  play rrand(50, 100)
  sleep 0.5
end
```

Jedes Mal, wenn Du den Code ablaufen läßt, hörst Du dieselbe Folge von
5 Tönen. Um eine andere Folge zu bekommen, setze einfach einen anderen 
Startpunkt:

```
use_random_seed 40
5.times do
  play rrand(50, 100)
  sleep 0.5
end
```

Nun produziert Sonic Pi eine andere Folge von 5 Tönen. Indem Du den 
Startpunkt wechselst und Dir die Ergebnisse anhörst, kannst Du eine 
Folge finden, die Dir gefällt - und wenn Du den Code dann an andere
weitergibst, werden sie genau das hören, was auch Du gehört hast.

Schauen wir uns noch eine andere nützliche Zufallsfunktion an.

## Auswählen (choose)

Häufig kommt es vor, dass man aus einer Liste von Dingen eines zufällig 
auswählen möchte. Zum Beispiel möchte ich einen Ton aus der folgenden 
Liste auswählen: 60, 65 oder 72. Dafür ist `choose` da. 
Zuerst musst Du Deine Zahlen in eine Liste packen. Dafür schreibst Du 
sie jeweils durch Kommata getrennt in eckige Klammern. Dann übergibst 
Du diese Liste dem Kommando `choose`:

```
choose([60, 65, 72])
```

Hören wir uns das an:

```
loop do
  play choose([60, 65, 72])
  sleep 1
end
```

## rrand

`rrand` haben wir schon kennengelernt, aber sehen wir uns das noch 
einmal genauer an. Es liefert eine zufällige Zahl zwischen zwei Werten, 
aber ohne diese Werte selbst; man sagt auch *exklusiv* dieser beiden 
Werte. Das bedeutet, dass sowohl der minimale als auch der maximale 
Wert niemals ausgegeben werden, immer nur eine Zahl *zwischen* diesen 
beiden Werten. Die Zahl wird immer eine Gleitkommazahl (engl. *Float*) 
sein, also keine ganze Zahl, sondern eine mit einem Komma[^14]. Einige 
Beispiele für Gleitkommazahlen, die der wiederholte Aufruf von 
`rrand(20, 110)` ausgeben könnte:

* 87.5054931640625
* 86.05255126953125
* 61.77825927734375

## rrand_i

Manchmal braucht man eine zufällige, aber ganze Zahl, eben keine 
Gleitkommazahl. Hier rettet einen `rrand_i`[^15]. Es funktioniert 
ähnlich `rrand`, kann jedoch auch den minimalen oder maximalen Wert, 
den man übergeben hat, als mögliche Zufallszahl auswählen (man kann 
auch sagen: es ist *inklusiv*, also nicht exklusive der Werte, mit 
denen man den Bereich für die Auswahl festgelegt hat). `rrand_i(20, 
110)` könnte zum Beispiel die folgenden Werte ausgeben:

* 88
* 86
* 62

## rand

`rand` gibt eine zufällige Gleitkommazahl zwischen 0 (inklusiv) und 
einem übergebenen Maximalwert (exklusiv) zurück. Standardmäßig - wenn 
also kein Maximalwert angegeben wird - wird ein Wert zwischen 0 und 1 
geliefert. Deshalb kann man `rand` gut dafür gebrauchen, zufällige 
Werte für `amp:` (also die Lautstärke) auszuwählen.

```
loop do
  play 60, amp: rand
  sleep 0.25
end
```

## rand_i

Ähnlich wie bei `rrand_i` und `rrand`, wählt `rand_i` eine ganze Zahl 
zwischen 0 und einem angegebenen Maximalwert aus.

## dice

Manchmal möchte man so tun, als würde man würfeln (engl. *to dice*) - 
das ist ein Sonderfall von `rrand_i`, wobei der kleinste Wert immer 
die 1 ist. Wenn man `dice` verwendet, muss man dabei immer bestimmen, 
wie viele Seiten der Würfel hat. Ein normaler Würfel hat 6 Seiten, also 
wird `dice(6)` entsprechend funktionieren und den Wert 1, 2, 3, 4, 5 
oder 6 zurückgeben. Aber - angenommen wir befänden uns in einen 
Fantasy-Rollenspiel - ist es Dir vielleicht lieber, wenn der Würfel 4 
oder 12, 20 oder sogar 120 Seiten hat!?

## one_in

Schließlich könnte es sein, das Du so tun willst, als ob Du beim 
Würfeln eine 6 hast - also den höchten Wert erreichst. `one_in` gibt - 
mit einer Wahrscheinlichkeit 1 im Verhältnis zur Menge der Würfelseiten 
- den Wert *wahr* (engl. *true*) zurück, falls die höchste Zahl 
gewürfelt wurde. `one_in(6)` wird also mit einer Wahrscheinlichkeit von 
1 zu 6 wahr, ansonsten *falsch* (engl. *false*). Wahr- und Falsch-Werte 
sind sehr nützlich, wenn es um `if`-Anweisungen geht, die wir in einem 
folgenden Kapitel dieses Tutorials besprechen.

Jetzt los, bring Deinen Code mit ein paar Zufälligkeiten durcheinander!

[^9]: Randomisierung (engl. *Randomisation*) bedeutet hier, dass man 
    eine Auswahl von Zahlen zufällig gestaltet, also jedesmal eine
    andere Zahl bekommt.

[^10]: Im Film *Groundhog Day* (deutsch: *Und täglich grüßt das
    Murmeltier*) erlebt Bill Murray immer wieder denselben Tag.

[^11]: Ein Schleife wird mit dem Ausdruck `loop` eingeleitet. Alles was
    innerhalb der Schleife steht, wird so oft wie angegeben oder
    unendlich oft wiederholt. 

[^12]: In Sonic Pi wird das das Abschneiden oder Verkürzen mit dem
    Ausdruck `cutoff` bezeichnet.

[^13]: Das englische Wort *Seed* bedeutet im Deutschen *Keim* oder
    *Samen*; hier wird es als *Startpunkt* übersetzt. 

[^14]: Die Sache wird noch dadurch ein wenig komplizierter, dass im
    Englischen anstelle eines Kommas ein Punkt steht. Die Gleitkommazahl
    `5,978` ist also im Englischen die *Floating Point Number* (kurz:
    *Float*) `5.978`. Alle Zahlen mit Kommawerten werden also in Sonic
    Pi mit einem Punkt dargestellt.

[^15]: Das kleine *i* in `rrand_i` steht für englisch *Integer* als eine
    ganze Zahl im Unterschied zu den Gleitkommazahlen.
