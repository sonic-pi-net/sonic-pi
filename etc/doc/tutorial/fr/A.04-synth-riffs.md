A.4 Riffs de synthés

# Riffs de synthés

Que ce soit la dérive hantée d'oscillateurs grondants ou le coup de
poing désaccordé des ondes scies perçant à travers le mix, le synthé
principal joue un rôle essentiel dans chaque piste électronique. Dans
l'édition du mois dernier de cette série de tutoriels nous avons
couvert comment coder nos rythmes. Dans ce tutoriel nous verrons
comment coder les trois composants principaux d'un riff de synthé : le
timbre, la mélodie et le rythme.

OK, allumez votre Raspberry Pi, ouvrez Sonic Pi (version 2.6 ou plus)
et faisons du bruit !


## Les possibilités du timbre

Une partie essentielle de n'importe quel riff de synthé est de jouer
avec le timbre des sons. On peut contrôler le timbre dans Sonic Pi de
deux manières : en choisissant différents synthés pour un changement
dramatique et en définissant les diverses options des synthés pour des
modifications plus subtiles. On peut aussi utiliser des effets, mais
ce sera pour un autre tutoriel...

Créons une boucle interactive simple où l'on modifiera en continu le
synthé courant :

    live_loop :timbre do
      use_synth (ring :tb303, :blade, :prophet, :saw, :beep, :tri).tick
      play :e2, attack: 0, release: 0.5, cutoff: 100
      sleep 0.5
    end

Regardez ce code. On parcourt (avec `tick`) juste un anneau de noms de
synthés (on boucle sur chacun d'eux à leur tour puis on répéte la
liste encore et encore). On passe ce nom de synthé à la fonction (fn)
`use_synth`, ce qui changera le synthé courant de la `live_loop`. On
joue aussi la note `:e2` (E (ou Mi) à la deuxième octave), avec un
temps de relâche de 0.5 battement (une demi seconde au BPM par défaut
de 60) et avec l'option `:cutoff` fixée à 100.

Vous entendez que les différents synthés ont des sons très différents
même s'ils jouent tous la même note. Maintenant expérimentez et jouez.
Changez le temps de relâche en lui donnant des valeurs plus grandes et
plus petites. Par exemple, changez les options `attack:` et `release:`
pour voir comme des temps différents de fondu d'entrée et de sortie
ont un grand effet sur le son. Enfin changez l'option `cutoff:` pour
voir comment différentes valeurs de coupure ont une influence massive
sur le timbre (des valeurs entre 60 et 130 sont bonnes). Voyez combien
de sons vous pouvez créer juste en changeant quelques valeurs. Une
fois que vous maîtrisez cela, allez dans l'onglet Synthés dans le
système d'aide pour voir la liste entière des synthés et des options
que chacun d'eux supporte pour voir l'étendue du pouvoir que vous avez
au bouts de vos doigts de codeur.

## Timbre

Le timbre est juste un mot savant pour décrire comment sonne un son.
Si vous jouez la même note avec différents instruments comme un
violon, une guitare, ou un piano, la fréquence (si elle sonne haut ou
bas) sera la même, mais la qualité du son sera différente. La qualité
du son, ce qui fait qu'on peut différencier un piano et une guitare,
c'est le timbre.

## Composition mélodique

Un autre aspect important de notre synthé principal est le choix de
notes que l'on veut jouer. Si vous avez déjà une bonne idée, alors
vous pouvez juste créer un anneau avec vos notes et les parcourir :

    live_loop :riff do
      use_synth :prophet
      riff = (ring :e3, :e3, :r, :g3, :r, :r, :r, :a3)
      play riff.tick, release: 0.5, cutoff: 80
      sleep 0.25
    end

Ici nous avons défini une mélodie avec un anneau qui inclut des notes
comme `:e3` et des silences représentés par `:r`. On utilise ensuite
`.tick` pour jouer chaque note à son tour, ce qui nous donne un riff
qui se répète.

## Mélodie automatique

Ce n'est pas toujours facile d'inventer un riff sympa. C'est souvent
plus simple de demander à Sonic Pi une sélection de riffs aléatoires
et de choisir celui que l'on préfère. Pour faire cela on doit combiner
trois choses : des anneaux, de l'aléatoire et des graines aléatoires.
Regardons un exemple :

    live_loop :random_riff do
      use_synth :dsaw
      use_random_seed 3
      notes = (scale :e3, :minor_pentatonic).shuffle
      play notes.tick, release: 0.25, cutoff: 80
      sleep 0.25
    end

Plusieurs choses se passent : regardons les une par une. On commence
par spécifier la graine aléatoire 3. Qu'est-ce que cela signifie ? Eh
bien, c'est utile parce que quand on définit la graine, on sait
prédire quelle sera la prochaine valeur aléatoire : ce sera la même
que la dernière fois qu'on a choisi la graine 3 ! Une autre chose
utile à savoir est que mélanger un anneau de notes fonctionne de la
même façon. Dans l'exemple ci-dessus c'est comme si on demandait le
'troisième mélange' dans la liste standard de mélanges : il sera le
même chaque fois comme on définit toujours la graine aléatoire à la
même valeur avant le mélange. Enfin on parcourt juste nos notes
mélangées pour jouer le riff.

Maintenant, c'est ici qu'on commence à s'amuser. Si on change la
valeur de la graine aléatoire, par exemple en 3000, on a un mélange
entièrement différent des notes. Ainsi il est très facile d'explorer
de nouvelles mélodies. Il suffit de choisir la liste de notes que l'on
veut mélanger (les gammes sont un très bon endroit pour commencer) et
ensuite de choisir la graine avec laquelle on veut les mélanger. Si on
n'aime pas la mélodie, on peut juste changer une de ces deux choses et
essayer à nouveau. Répétez jusqu'à ce que vous aimiez ce que vous
entendez !


## Pseudo aléatoire

L'aléatoire de Sonic Pi n'est pas vraiment aléatoire, on appelle ça du
pseudo aléatoire. Imaginez que vous jetez un dé 100 fois et que vous
écrivez le résultat de chaque jet sur une feuille de papier. Sonic Pi
a l'équivalent de cette liste de résultats qu'il utilise quand on
demande une valeur aléatoire. Au lieu de jeter un vrai dé, il prend
juste la valeur suivante dans la liste. Définir la graine aléatoire
revient à sauter à un endroit particulier de cette liste.

## Trouver votre rythme

Un autre aspect important de notre riff est le rythme : quand jouer
une note et quand ne pas le faire. Comme on a vu, on peut utiliser
`:r` dans nos anneaux pour insérer des silences. Une autre manière
puissante consiste à utiliser des 'spreads' que l'on couvrira dans un
futur tutoriel. Aujourd'hui nous allons utiliser l'aléatoire pour nous
aider à trouver notre rythme. Au lieu de jouer toutes les notes on
peut utiliser une condition pour jouer une note avec une probabilité
donnée. Voyons cela :

    live_loop :random_riff do
      use_synth :dsaw
      use_random_seed 30
      notes = (scale :e3, :minor_pentatonic).shuffle
      16.times do
        play notes.tick, release: 0.2, cutoff: 90 if one_in(2)
        sleep 0.125
      end
    end

Une fonction très utile à connaître est `one_in` qui nous donne une
valeur `true` ou `false` (vrai ou faux) avec la probabilité spécifiée.
Ici nous utilisons une valeur de 2 donc en moyenne un appel sur deux à
`one_in` retournera `true`. En d'autres termes, elle retournera `true`
50% du temps. Si on choisit des valeurs plus grandes elle retournera
`false` plus souvent ce qui mettra plus d'espace dans notre riff.

Remarquez qu'on a introduit un peu d'itération ici avec `16.times`.
C'est parce qu'on ne veut redéfinir notre valeur de graine aléatoire
que toutes les 16 notes pour que notre rythme se répète toutes les 16
fois. Cela n'affecte pas le mélange comme il est toujours fait juste
après avoir défini la graine. On peut utiliser la longueur d'itération
pour modifier la longueur de notre riff. Essayez de changer le 16 en 8
ou même en 4 ou 3 et voyez comment cela affecte le rythme du riff.

## Rassemblons tout

OK, combinons tout ce que nous avons appris dans un dernier exemple.
A la prochaine !

    live_loop :random_riff do
      #  uncomment to bring in:
      #  synth :blade, note: :e4, release: 4, cutoff: 100, amp: 1.5
      use_synth :dsaw
      use_random_seed 43
      notes = (scale :e3, :minor_pentatonic, num_octaves: 2).shuffle.take(8)
      8.times do
        play notes.tick, release: rand(0.5), cutoff: rrand(60, 130) if one_in(2)
        sleep 0.125
      end
    end
     
    live_loop :drums do
      use_random_seed 500
      16.times do
        sample :bd_haus, rate: 2, cutoff: 110 if rand < 0.35
        sleep 0.125
      end
    end
     
    live_loop :bd do
      sample :bd_haus, cutoff: 100, amp: 3
      sleep 0.5
    end




