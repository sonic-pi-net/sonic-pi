4 Losowość

# Losowość

Wspaniałym sposobem aby dodać odrobine intrygi do twojej muzyki, jest 
wykorzystanie paru losowych liczb. Sonic Pi posiada parę fajnych 
funkcjonalności, które umożliwiają dodanie przypadkowości do twojej 
muzyki. Zanim jednak zaczniemy musimy nauczyć się jednej szokującej prawdy: 
w Soni Pi *losowość nie jest tak naprawdę losowa*. Co to do diaska znaczy? 
No cóż, zobaczmy.

## Powtarzalność

Bardzo przydatną funkcją do generowania liczb losowych jest `rrand`, która 
zwróca losową wartość pomiędzy dwoma liczbami - minimum *min* i maksimum 
*max*. (`rrand` to skrót od angielskiego ranged random - liczba losowa 
z określonego przedziału). Spróbujmy zagrać losową nutę:

```
play rrand(50, 100)
```

Oh, została zagrana losowa nuta. Zagrana nuta to `77.4407`. Fajna liczba 
losowa pomiędzy 50 a 100. Zaraz zaraz, czy przypadkiem nie zgadłem dokładnej 
wartości losowej nuty, którą przed chwilą otrzymałeś? Dzieje się tu coś 
podejrzanego. Spróbuj uruchomić kod ponownie. I co? Znowu została wybrana 
liczba `77.4407`? Toć to nie jest liczba losowa!

Odpowiedzią jest fakt, że tak naprawde wynik nie jest naprawdę losowy, 
jest pseudo losowy. Sonic Pi wygeneruje dla Ciebie liczby, które wyglądają 
jak losowe w sposób powtarzalny. Takie podejście jest bardzo przydatne 
jeśli chcesz być pewny, że muzyka, którą tworzysz na swojej maszynie będzie 
brzmieć identycznie na każdym innym komputerze - nawet jeśli używasz 
w swojej kompozycji pewnej losowości.

Oczywiście, jeśli w danym utworze, ta 'losowo wybrana liczba' będzie 
za każdym razem miała wartość `77.4407`, to nie będzie to zbyt interesujące. 
Jednakże jest inaczej. Spróbój poniższego kawałka kodu: 

```
loop do
  play rrand(50, 100)
  sleep 0.5
end 
```

Tak! Wreszcie uzyskaliśmy losowe dźwieki. W ramach danego *uruchomienia* 
kolejnych wywołań funkcji losującej zostaną zwrócone wartości losowe. 
Jendakże, kolejne uruchomienie spowoduje wyprodukowanie dokładnie takiej 
samej sekwencji losowych wartości i brzmienie będzie takie same jak 
przy pierwszym uruchomieniu. To tak jakby cały kod Sonic Pi cofnął się 
w czasie dokładnie do tego samego punktu za każdym razem gdy zostaje 
wciśnięty przycisk Run (Uruchom). Toć to Dzien Świstaka dla syntezy 
muzycznej!

## Haunted Bells (Nawiedzone Dzwony)

Piękną ilustracją randomizacji w akcji jest przykład nawiedzonych dzwonów 
(Haunted Bells), który zapętla sampel `:perc_bell` nadając mu przy tym 
losowe tempo (parametr rate) oraz czas przerwy (polecenie sleep) pomiędzy 
poszczególnymi dźwiękami dzwonów:

```
loop do
  sample :perc_bell, rate: (rrand 0.125, 1.5)
  sleep rrand(0.2, 2)
end
```

## Losowe odcięcie

Innym ciekawym przykładem losowości jest modyfikacja odcięcia 
syntezatora losowo. Świetnym syntezatorem, na którym możemy tego 
spróbować jest syntezator emulujący `:tb303`:


```
use_synth :tb303

loop do
  play 50, release: 0.1, cutoff: rrand(60, 120)
  sleep 0.125
end
```

## Losowe zarzewie

A co, jeśli nie podoba Ci się ta konkretna sekwencja liczb losowych, 
które generuje Sonic Pi? Nic nie stoi na przeszkodzie abyś wybrał 
inny punkt rozpoczęcia za pomocą polecenia `use_random_seed`. Domyślną 
wartością dla funkcji odsiewu (seed?) jest 0, wystarczy więc wybrać 
inną wartość aby uzyskać inne wartości losowe.

Weźmy pod uwagę poniższy przykład:

```
5.times do
  play rrand(50, 100)
  sleep 0.5
end
```

Za każdym razem gdy uruchomisz powyższy kod, usłyszysz sekwencję 
tych samych 5 nut. Aby uzyskać inną sekwencję wystarczy zmienić 
wartość odsiewu (?seed):

```
use_random_seed 40
5.times do
  play rrand(50, 100)
  sleep 0.5
end
```

Spowoduje to wygenerowanie sekwencji 5 innych losowych nut. Zmieniając 
parametr odsiewu (?seed) i słuchając wyników jakie to spowodowało, możesz 
w końcu znaleźć taka sekwencję, która Ci się spodoba - a gdy wtedy podzielisz 
się nią z innymi, usłyszą oni dokładnie to samo co ty.

A teraz przyjrzyjmy się kilku innym, przydatnym funkcją generującym wartości 
losowe.

## choose (wybierz)

Bardzo popularną rzeczą jest wybór losowego obiektu z listy znanych nam 
obiektów. Na przykład, mogę chcieć zagrać jedną z następujących nut: 
60, 65 lub 72. Żeby to osiągnąć mogę użyć funkcji `choose`, która pozwala 
mi na wybór jednego obiektu z istniejącej listy obiektów. Po pierwsze, 
muszę wsadzić moje liczby (nuty) do listy. Aby tego dokonać wystarczy, 
że opakujemy nasze liczby w nawiasy kwadratowe i oddzielimy każdą z nich 
przecinkiem: `[60, 65, 72]`. Następnie wystarczy przekazać je jako 
parametr do polecenia `choose`: 

```
choose([60, 65, 72])
```

Posłuchajmy zatem jak to brzmi: 

```
loop do
  play choose([60, 65, 72])
  sleep 1
end
```

## rrand

Widzieliśmy już w akcji funkcję `rrand`, ale spróbujmy uruchomić ją raz jeszcze. 
Zwraca ona losową liczbę z podanego przedziału, który jest w tym wypadku przedziałem 
otwartym. Oznacza to, że żadna z podanych liczb jako wartości minimalna i maksymalna 
nigdy nie zostanie zwrócona - zawsze coś pomiędzy tymi dwoma liczbami. Zwrócona
liczba będzie liczbą zmiennoprzecinkową - oznacza to, że nie jest to liczba całkowita 
tylko ułamek. Oto kilka przykładów liczb zmiennoprzecinkowych zwracanych przez 
polecenie `rrand(20,110)`:

* 20.343235
* 42.324324
* 100.93423

## rrand_i

Sporadycznie może się zdarzyć, że będziesz potrzebował liczbę losową, która jest 
liczbą całkowitą, a nie zmiennoprzecinkową. W tym przypadku z pomocą przychodzi 
polecenie `rrand_i`. Polecenie to działa bardzo podobnie do `rrand`, z tą jednak różnicą, 
że może zwracać teź liczbę minimalną i maksymalną z podanego przedziału jako potencjalna 
wartość losowa (co oznacza, że podawany przedział jest domknięty, a nie otwarty). Poniżej 
przykłady liczb losowych, które mogą zostać zwrócone przez polecenie `rrand_i(20,110)`:

* 20
* 46
* 99

## rand

Te polecenie zwraca losową liczbę zmiennoprzecinkową pomiędzy 0 (przedział domknięty) 
a maksymalną liczbą, którą podajesz jako parametr (przedział otwarty). Domyślnie 
(jeśli nie podamy parametru) zostanie zwrócona liczba z przedziału od 0 do 1. Warto 
zauważyć, że polecenie to może być bardzo użyteczne do ustawiania losowego poziomu 
poziomu amplitudy `amp:` 

```
loop do
  play 60, amp: rand
  sleep 0.25
end
```

## rand_i

Przy tym poleceniu mamy relację podobną jak w przypadku poleceń `rrand_i` 
i `rrand`. Polecenie `rand_i` zwraca losową liczbę całkowitą z zakresu 
od 0 do maksymalnej wartości, którą podasz.

## dice

Czasami chciałbyś zasymulować rzut kostką - jest to specyficzny przypadek 
dla polecenia `rrand_i`, dla którego najniższa liczba to zawsze 1. Uruchomienie 
polecenia rzut kostką `dice` wymaga abyś podał liczbę oczek na kostce. Standardowa 
kostka ma 6 oczek, więc polecenie `dice(6)` zachowa się bardzo podobnie - 
każde uruchomienie będzie zwracać jedną z wartości 1, 2, 3, 4, 5 lub 6. Jednakże, 
tak damo jak w grach RPG (role-playing games), może się zdarzyć, że będziesz 
potrzebować kostki o 4 oczkach, albo o 12, albo o 20 - być może będziesz 
potrzebować nawet kostki która będzie miała 120 oczek!

## one_in

Na koniec może się zdarzyć, że będziesz chciał zasymulować wyrzucenie 
najlepszego wyniku dla danej kostki, np. 6 oczek dla standardowej kostki. 
Polecenie `one_in` zwraca prawdę (true) z prawdopodobieństwem, że zostało 
wyrzucone jedno oczko. Idąc dalej polecenie `one_in(6)` zwróci prawdę (true) 
z prawdopodobieństwem 1 do 6-sciu lub fałsz. Wartości prawda (true) 
i fałsz (false) są bardzo przydatne przy korzystaniu z polecenie warunkowego 
`if`, które omówimy w jednej z kolejnych sekcji tego tutoriala.

A teraz, spróbuj trochę zagmatwać twój kod i muzykę odrobiną losowości!
