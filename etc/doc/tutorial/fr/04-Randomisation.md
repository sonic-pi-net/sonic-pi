4 Randomisation

# Randomisation

Un grand moyen d'ajouter de l'intérêt à votre musique est d'utiliser 
quelques nombres aléatoires ("random"). Sonic Pi a des fonctions 
remarquables pour ajouter de l'aléatoire à votre musique, mais avant 
de commencer, nous devons apprendre une vérité choquante : dans Sonic 
Pi *aléatoire n'est pas vraiment aléatoire*. Qu'est-ce qu'ici-bas cela 
signifie ? Eh bien, voyons-le.

## Répétabilité

Une fonction vraiment utile est `rrand` qui vous donnera une valeur 
aléatoire comprise entre deux nombres - un *min* et un *max*. (`rrand` 
est l'abréviation de "ranged random"). Essayons de jouer une note 
aléatoire :

```
play rrand(50, 100)
```

Ooh, une note aléatoire a été jouée. La note `77.4407` a été 
jouée - une note aléatoire sympathique entre 50 et 100. Wooh , 
attendez, est-ce que je viens juste de prédire exactement la note que 
vous avez obtenue ?  Quelque chose de louche se passe ici. Essayez 
d'exécuter le code une nouvelle fois. Quoi ? `77.4407` a encore été 
choisi ? Ce ne peut être aléatoire !

La réponse est que ce n'est pas vraiment aléatoire, c'est pseudo-aléatoire.
Sonic Pi vous donne une suite d'un semblant de nombres aléatoires de 
manière reproductible. C'est très utile pour s'assurer que la musique 
que vous avez créée sur votre machine sonnera de façon identique sur 
n'importe quelle autre machine - même si vous utilisez de l'aléatoire 
dans votre composition.

Bien sûr, dans un morceau de musique donné, si `77.4407` était choisi 
'aléatoirement' à chaque fois, ce ne serait pas très intéressant. Ce 
n'est cependant pas le cas. Essayez ce qui suit :

```
loop do
  play rrand(50, 100)
  sleep 0.5
end 
```

Oui ! ça sonne aléatoire finalement. A l'intérieur d'un *run* donné, 
des appels successifs à des fonction aléatoires retournent des 
valeurs aléatoires. Cependant, une nouvelle exécution produira 
exactement la même séquence de valeurs aléatoires et sonnera exactement 
pareil. C'est comme si tout le code de Sonic Pi revenait en arrière 
exactement au même point de départ chaque fois que le bouton "Run" 
était pressé. C'est le jour de la marmotte de la synthèse musicale.

## Cloches hantées

Une agréable illustration de la randomisation en action est l'exemple 
des cloches hantées en faisant boucler l'échantillon `:perc_bell` avec 
une vitesse et un temps de repos aléatoires entre les sons de cloche :

```
loop do
  sample :perc_bell, rate: (rrand 0.125, 1.5)
  sleep rrand(0.2, 2)
end
```

## Limite ("cutoff") aléatoire

Un autre exemple sympathique de randomisation est de modifier la limite 
d'un son de synthé aléatoirement. Un synthé super pour essayer cela 
est l'émulateur du `:tb303` :

```
use_synth :tb303

loop do
  play 50, release: 0.1, cutoff: rrand(60, 120)
  sleep 0.125
end
```

##  Tête de série aléatoire

Alors que faire si vous n'aimez pas cette particulière séquence de 
nombres aléatoires que fournit Sonic Pi ? Eh bien, c'est tout à fait 
possible de choisir un point de départ via `use_random_seed`. Il est 
établi que la tête de série par défaut est 0, aussi choisissez une 
autre tête de série pour une expérience aléatoire différente !

Envisagez ce qui suit :

```
5.times do
  play rrand(50, 100)
  sleep 0.5
end
```

Chaque fois que vous exécuterez ce code, vous entendrez la même 
séquence de 5 notes. Pour obtenir une séquence différente, changez 
simplement la tête de série :

```
use_random_seed 40
5.times do
  play rrand(50, 100)
  sleep 0.5
end
```

Ceci va produire une séquence différente de 5 notes. En changeant le 
tête de série et en écoutant les résultats, vous pouvez trouver 
quelque chose que vous aimez - et quand vous le partagerez avec 
d'autres, ils entendront exactement ce que vous avez aussi écouté.

Jetons un œil sur d'autres fonctions de randomisation utiles.


## Choix ("choose")

Une chose très commune est de choisir un item aléatoirement dans une 
liste d'items connus. Par exemple, je peux vouloir jouer une note parmi 
les suivantes : 60, 65 ou 72. Je peux y arriver avec `choose` qui me 
choisit un item dans une liste. En premier, je dois mettre mes nombres 
dans une liste, ce qui est réalisé en les encadrant entre crochets et 
en les séparant avec des virgules : `[60, 65, 72]`. Ensuite, j'ai juste 
besoin de leur passer `choose` :

```
choose([60, 65, 72])
```

Écoutons comment cela sonne :

```
loop do
  play choose([60, 65, 72])
  sleep 1
end
```

## rrand

Nous avons déja vu `rrand`, mais examinons-le encore. Il retourne un 
nombre aléatoire entre 2 valeurs exclues. Cela signifie qu'il ne 
retournera jamais soit la borne basse, soit la borne haute - toujours 
quelque chose entre les deux. Le nombre sera toujours un nombre 
flottant - ce qui signifie que ce n'est pas un nombre entier, mais une 
fraction de nombre. Exemple de flottants retournés par `rrand(20, 110)`:


* 20.343235
* 42.324324
* 100.93423

## rrand_i

Occasionnellement, vous désirez un nombre entier aléatoire, pas un 
flottant. C'est là que `rrand_i` arrive à la rescousse. Il fonctionne 
de façon similaire à `rrand` sauf qu'il peut retourner potentiellement 
les valeurs minimales et maximales comme valeurs aléatoires (ce qui 
signifie que c'est inclusif plutôt qu'exclusif des bornes). Exemple de 
nombres retournés par `rrand_i(20, 110)` :

* 20
* 46
* 99

## rand

Cette fonction va retourner un flottant aléatoire entre 0 (inclus) et 
la valeur maximale que vous spécifiez (exclue). Par défaut, elle 
retourne une valeur entre 0 et 1. C'est par conséquent utile pour 
choisir une valeur de `amp:` aléatoire :

```
loop do
  play 60, amp: rand
  sleep 0.25
end
```

## rand_i

Similaire à la relation entre `rrand_i` et `rrand`, `rand_i` retournera 
un nombre entier compris entre 0 et la valeur maximale que vous 
spécifierez.

## dice (dé)

Quelquefois, vous souhaitez émuler un jet de dés - c'est un cas 
particulier de `rrand_i` où la valeur minimale est toujours 1. Un appel 
à `dice` nécessite que vous spécifiez le nombre de faces du dé. Un 
dé standard a 6 faces, donc `dice(6)` agira de manière très similaire -
retournant l'une des valeurs 1, 2, 3, 4, 5, ou 6. Cependant, juste 
comme dans des jeux de rôle fantaisistes, vous pourriez trouver des 
dés à 4 faces, ou à 12 faces, peut-être même à 120 faces !

## one_in

Finalement, vous pouvez souhaiter simuler la chance que vous avez de 
tomber sur le nombre le plus élevé d'un dé, soit 6 pour un dé standard. 
Ainsi `one_in` retourne vrai ("true") avec une probabilité de 1 sur le 
nombre de faces du dé. Par conséquent `one_in(6)` retournera vrai avec 
une probabilité de 1 sur 6 ou faux ("false") autrement. Les valeurs 
True et False sont très utiles pour les ordres `if` que nous couvrirons 
dans une section suivante de ce tutoriel.

Maintenant allez et introduisez de l'aléatoire dans votre code !
