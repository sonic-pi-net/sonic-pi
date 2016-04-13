A.11 Tic tac

# Suivre le rythme

Le mois dernier dans cette série nous avons regardé en détail comment
fonctionne le système de gestion de l'aléatoire de Sonic Pi. On a
exploré comment on peut l'utiliser de manière déterministe pour avoir
de nouveaux niveaux de contrôle dynamique sur notre code. Ce mois-ci
nous allons continuer notre exploration technique et regarder le
système unique de 'tick' de Sonic Pi. D'ici la fin de cet article vous
parcourrez des rythmes et mélodies sur votre chemin pour devenir un DJ
de programmation interactive.


# Compter les temps

Quand on fait de la musique on a souvent envie de faire quelque chose
de différent en fonction du temps sur lequel on est. Sonic Pi a un
système spécial pour compter les temps appelé `tick` pour vous donner
un contrôle précis sur quand est-ce qu'un battement arrive réellement
et il supporte même des battements multiples avec leur propre tempo.

Amusons-nous : pour avancer le temps on a juste besoin d'appeler
`tick`. Ouvrez un buffer libre, tapez le code suivant et cliquez sur
le bouton 'Run' :

```
puts tick #=> 0
```

Cela retournera le battement courant : `0`. Remarquez que même si
vous cliquez plusieurs fois sur le bouton 'Run', il retournera
toujours `0`. Cela parce que chaque exécution commence avec un
compteur qui part de 0. Cependant, pendant que le programme tourne, on
peut avancer le compteur autant qu'on veut :

```
puts tick #=> 0
puts tick #=> 1
puts tick #=> 2
```

<breakout> Quand vous voyez le symbole `#=>` à la fin d'une ligne de
code cela veut dire que cette ligne va noter ce texte dans la partie
de droite de Sonic Pi. Par exemple, `puts foo #=> 0` veut dire que le
code `puts foo` affiche `0` dans le log à cet endroit du programme.
</breakout>

# Vérifier le compteur

On a vu que `tick` fait deux choses. Il incrémente (ajoute un) et
retourne le compteur courant. Parfois on veut juste regarder le
battement courant sans devoir l'incrémenter et on peut faire cela via
`look`.

``` 
puts tick #=> 0
puts tick #=> 1
puts look #=> 1
puts look #=> 1
``` 

Dans ce code on incrémente le compteur deux fois puis on appelle
`look` deux fois. On verra les valeurs suivantes dans le log : `0`,
`1`, `1`, `1`. Les deux premiers `tick`s ont retourné `0` et `1`,
comme attendu, puis les deux `look`s ont juste retourné la dernière
valeur du compteur deux fois, donc `1`.


# Anneaux

On peut donc avancer le compteur avec `tick` et en connaître la valeur
avec `look`. Qu'est-ce qui vient suite ? On a besoin de quelque chose
à parcourir. Sonic Pi utilise les anneaux pour représenter les
mélodies et rythmes et le système de tick a été conçu spécialement
pour fonctionner avec eux. En fait, les anneaux ont leur propre
version de `tick` qui fait deux choses. D'un côté elle agit comme un
tick normal et incrémente le compteur. D'un autre, elle donne une
valeur de l'anneau en utilisant le compteur comme index. Voyons cela :

```
puts (ring :a, :b, :c).tick #=> :a
```

`.tick` est une version spéciale avec un point de `tick` qui nous
retourne la première valeur de l'anneau: : `:a`. On peut attraper
chacune des notes de l'anneau en appelant `:tick` plusieurs fois :

```
puts (ring :a, :b, :c).tick #=> :a
puts (ring :a, :b, :c).tick #=> :b
puts (ring :a, :b, :c).tick #=> :c
puts (ring :a, :b, :c).tick #=> :a
puts look                   #=> 3
```

Regardez le log et vous verrez `:a`, `:b`, `:c` et puis de nouveau
`:a`. Remarquez que `look` retourne `3`. Les appels à `.tick` se
comportent comme les appels à `tick` : ils incrémentent le compteur
local.


# Un arpégiateur dans une boucle interactive

La véritable puissance vient quand on mélange `tick` avec des anneaux
et des `live_loop`s. En les combinant on a tous les outils dont on a
besoin pour construire et comprendre un arpégiateur simple. On a juste
besoin de quatre choses :

1. Un anneau qui contient les notes sur lesquelles on va boucler
2. Une manière d'incrémenter et de lire le compteur
3. La capacité de jouer une note en se basant sur le compteur courant
4. Une structure de boucle qui répète l'arpégiateur en continu

Ces concepts se retrouvent tous dans le code suivant :

```
notes = (ring 57, 62, 55, 59, 64)

live_loop :arp do
  use_synth :dpulse
  play notes.tick, release: 0.2
  sleep 0.125
end
```

Regardons chacune de ces lignes. On commence par définir notre anneau
de notes que nous allons jouer en continu. On crée ensuite une
`live_loop` nommée `:arp` qui va boucler pour nous. A chaque itération
de la `live_loop` on choisit un synthé `:dpulse` et on joue ensuite la
note suivante de notre anneau en utilisant `.tick`. Souvenez-vous que
cela va incrémenter notre compteur de temps et en utiliser la valeur
comme index dans notre anneau de notes. Enfin on attend un huitième de
temps avant de recommencer la boucle.

# Battements multiples simultanés

Une chose très importante à savoir est que les `tick`s sont liés à la
`live_loop`. Cela veut dire que chaque `live_loop` a son propre
compteur indépendant. C'est beaucoup plus puissant que d'avoir un
métronome et battement global. Regardons ce que cela donne :

notes = (ring 57, 62, 55, 59, 64)

with_fx :reverb do
  live_loop :arp do
    use_synth :dpulse
    play notes.tick + 12, release: 0.1
    sleep 0.125
  end
end

live_loop :arp2 do
  use_synth :dsaw
  play notes.tick - 12, release: 0.2
  sleep 0.75
end

# Collisions de battements

Une grande source de confusion dans le système de tick de Sonic Pi est
quand on veut parcourir plusieurs anneaux dans le même `live_loop`.

use_bpm 300
use_synth :blade
live_loop :foo do
  play (ring :e1, :e2, :e3).tick
  play (scale :e3, :minor_pentatonic).tick
  sleep 1
end

Même si chaque `live_loop` a son compteur indépendant, ici on appelle
`.tick` deux fois dans la même `live_loop`. Cela veut dire que le
compteur sera incrémenté deux fois à chaque boucle. Cela peut produire
des polyrythmes intéressants mais ce n'est souvent pas ce que l'on
souhaite. Il y a deux solutions à ce problème. Une option est
d'appeler `tick` manuellement au début de la `live_loop` puis
d'utiliser `look` pour chercher la valeur courante du compteur dans
chaque `live_loop`. La seconde solution est de passer un nom unique
à chaque appel à `.tick`, comme par exemple `.tick(:foo)`. Sonic Pi
créera alors un compteur séparé pour chaque tick nommé que vous
utiliserez. Ainsi on peut travailler avec autant de compteurs que
nécessaire ! Lisez le section 9.4 sur les ticks nommés dans le
tutoriel inclus dans Sonic Pi pour plus d'informations.

# Rassemblons tout

Combinons nos connaissances sur les `tick`s, `ring`s (anneaux) et
`live_loop`s pour un dernier exemple amusant. Comme d'habitude, ne
traitez pas ceci comme un morceau terminé. Commencez à changer des
choses et amusez-vous avec et voyez en quoi vous pouvez le
transformer. A la prochaine...


use_bpm 240
notes = (scale :e3, :minor_pentatonic).shuffle

live_loop :foo do
  use_synth :blade
  with_fx :reverb, reps: 8, room: 1 do
    tick
    co = (line 70, 130, steps: 32).tick(:cutoff)
    play (octs :e3, 3).look, cutoff: co, amp: 2
    play notes.look, amp: 4
    sleep 1
  end
end

live_loop :bar do
  tick
  sample :bd_ada if (spread 1, 4).look
  use_synth :tb303
  co = (line 70, 130, steps: 16).look
  r = (line 0.1, 0.5, steps: 64).mirror.look
  play notes.look, release: r, cutoff: co
  sleep 0.5
end
