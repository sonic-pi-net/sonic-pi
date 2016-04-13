A.10 Contrôle

# Contrôler votre son

Jusqu'ici dans cette série nous nous sommes concentrés sur le
déclenchement de sons. Nous avons découvert qu'on pouvait déclencher
les nombreux synthés présents dans Sonic Pi avec `play` ou `synth` et
comment déclencher des samples pré-enregistrés avec `sample`. Nous
avons aussi vu comment on pouvait entourer ces sons dans des effets
studio tels que la reverb et la distorsion en utilisant la commande
`with_fx`. En combinant cela avec le système de chronométrage
incroyablement précis de Sonic Pi on peut produire un vaste ensemble
de sons, rythmes et mélodies. Cependant, une fois qu'on a
soigneusement sélectionné les options d'un son en particulier et qu'on
l'a déclenché, on ne peut plus le modifier pendant qu'il est joué,
c'est ça ? Non ! Aujourd'hui vous allez apprendre quelque chose de
très puissant : comment contrôler des synthés qui sont en train d'être
joués.

## Un son de base

Créons un simple son agréable. Lancez Sonic Pi et tapez le code
suivant dans un buffer disponible :

```
synth :prophet, note: :e1, release: 8, cutoff: 100
```

Maintenant cliquez sur le bouton 'Run' en haut à gauche pour entendre
un beau son de synthé grondant. Allez-y, cliquez à nouveau dessus
quelques fois pour vous habituer. OK, fini? Commençons à le contrôler !

## Noeuds de synthé

Une fonctionnalité peu connue de Sonic Pi est que les fonctions
`play`, `synth` et `sample` retournent ce qu'on appelle un `SynthNode`
qui représente un son en train d'être joué. Vous pouvez capturer un de
ces `SynthNode`s en utilisant une variable standard et le *contrôler*
ensuite dans le futur. Par exemple, changeons la valeur de l'option
`cutoff:` après un battement :

```
sn = synth :prophet, note: :e1, release: 8, cutoff: 100
sleep 1
control sn, cutoff: 130
```

Regardons chaque ligne une par une :

On commence par déclencher le synthé `:prophet` en utilisant la
fonction `synth` habituelle. Cependant on capture aussi le résultat
dans une variable nommée `sn`. On aurait pu appeler cette variable
complètement différemment comme par exemple `synth_node` ou `jane` :
le nom n'a pas d'importance. Enfin il est important de choisir un nom
qui a du sens pour vous pour vos performances et pour les gens qui
lisent votre code. J'ai choisi `sn` parce que c'est un bon petit moyen
mnémotechnique pour 'synth node'.

A la ligne 2 on a une commande `sleep` standard. Ca ne fait rien de
spécial : ça demande juste à l'ordinateur d'attendre un battement
avant d'avancer à la ligne suivante.

C'est à la ligne 3 qu'on commence à s'amuser. Ici on utilise la
fonction `control` pour dire à notre `SynthNode` courant de changer la
valeur de coupure en `130`. Si vous cliquez sur le bouton 'Run', vous
entendrez le synthé `:prophet` commencer à jouer comme avant, mais
après un battement il changera et sonnera beaucoup plus clair.


** Breakout Box Start **
Options modulables

La plupart des options des synthés et effets de Sonic Pi peuvent être
modifiées après avoir été déclenchées. Cependant, ce n'est pas le cas
pour toutes. Par exemple, les options d'enveloppe `attack:`, `decay:`,
`sustain:` et `release:` ne peuvent être définies que quand on
déclenche le synthé. Pour savoir quelles options peuvent être
modifiées ou non, c'est simple : allez voir la documentation d'un
synthé ou effet et regardez la documentation des options individuelles
et cherchez les phrases "May be changed whilst playing" (Peut être
modifiée en jouant) ou "Can not be changed once set" (ne peut pas être
modifiée une fois définie). Par exemple, la documentation de l'option
`attack:` du synthé `:beep` indique clairement qu'on ne peut pas la
modifier ultérieurement :

* Valeur par défaut: 0 
* Doit être supérieure ou égal à 0
* Ne peut pas être modifiée après sa définition
* Mise à l'échelle avec la valeur courante de BPM
** Breakout Box End **

## Modifications multiples

Pendant qu'un synthé est joué vous n'êtes pas limités à ne le changer
qu'une fois : vous êtes libres de le modifier autant que vous le
souhaitez. Par exemple, on peut transformer notre `:prophet` en petit
arpégiateur avec le code suivant :

```
notes = (scale :e3, :minor_pentatonic)
sn = synth :prophet, note: :e1, release: 8, cutoff: 100
sleep 1
16.times do
  control sn, note: notes.tick
  sleep 0.125
end
```

Dans cet extrait de code nous avons juste introduit quelques choses en
plus. On commence par définir une nouvelle variable appelée `notes`
qui contient les notes qu'on aimerait parcourir en boucle (un
arpégiateur est juste un mot savant pour quelque chose qui parcourt en
boucle une liste de notes dans un ordre donné). Ensuite on déplace
notre appel à `control` dans une itération en l'appelant 16 fois. A
chaque appel à `control` on parcourt notre anneau de `notes` qui va se
répéter automatiquement quand on arrivera à sa fin (grâce à la
puissance fabuleuse des anneaux de Sonic Pi). Pour un peu de variété
essayez de remplacer `.tick` par `.choose` et tentez d'entendre la
différence.


Notez qu'on peut modifier plusieurs options en même temps. Essayez de
changer la ligne de contrôle en la suivante et écoutez la différence :

```
control sn, note: notes.tick, cutoff: rrand(70, 130)
```

## Transitions

Quand on contrôle un `SynthNode`, il répond exactement à ce moment-là
et change immédiatement la valeur de l'option comme si vous aviez
pressé un bouton ou actionné un interrupteur pour demander la
modification. Cela peut sonner rythmique et percussif, notamment si
l'option contrôle un aspect du timbre comme `cutoff:`. Cependant, on
n'a pas toujours envie que la modification arrive immédiatement. On a
parfois envie de se déplacer en douceur de la valeur courante à la
nouvelle, comme si on avait déplacé un curseur. Bien sûr, Sonic Pi
sait aussi faire cela en utilisant les options `_slide:`

Chaque option qui peut être modifiée a aussi une option spéciale
correspondante `_slide:` qui vous permet de définir un temps de
transition. Par exemple, `amp:` a `amp_slide:` et `cutoff:` a
`cutoff_slide:`. Ces options de transition marchent un peu
différemment de toutes les autres options parce qu'elles disent à la
note de synthé comment se comporter *la prochaine fois qu'elle seront
contrôlées*. Voyons ça :

```
sn = synth :prophet, note: :e1, release: 8, cutoff: 70, cutoff_slide: 2
sleep 1
control sn, cutoff: 130
```

Remarquez que cet exemple est exactement le même qu'avant sauf qu'on a
ajouté `cutoff_slide:`. Ceci indique que la prochaine fois que
l'option `cutoff:` de ce synthé sera contrôlée, il mettra deux
battements pour passer de sa valeur courante à la nouvelle valeur.
Par conséquent, quand on utilise `control` vous pouvez entendre la
coupure changer graduellement de 70 à 130. Cela crée une sensation
dynamique intéressante pour le son. Maintenant essayez de changer la
durée de `cutoff_slide:` en une valeur plus courte comme 0.5 ou une
valeur plus longue comme 4 pour voir comment ça change le son.
Souvenez-vous que vous pouvez transitionner chacune des options
modifiables de cette même manière, et chaque valeur de `_slide:` peut
être complètement différente donc vous pouvez avoir une transition
lente de la coupure, une transition rapide de l'amplitude, et une
transition de la stéréo un peu entre les deux si c'est ce que vous
avez envie de créer.


## Rassemblons tout

Regardons un court exemple qui montre la puissance du contrôle des
synthés après leur déclenchement. Remarquez que vous pouvez aussi
transitionner les effets comme les synthés, mais avec une syntaxe
légèrement différente. Regardez la section 7.2 du tutoriel inclus dans
Sonic Pi pour plus d'information sur le contrôle des effets.

Copiez le code dans un buffer de libre et écoutez. Ne vous arrêtez pas
là : jouez à modifier le code. Changez les durées de transition,
changez les notes, le synthé, l'effet et les durées d'attente et voyez
si vous pouvez le transformer en quelque chose de complètement
différent !


```
live_loop :moon_rise do
  with_fx :echo, mix: 0, mix_slide: 8 do |fx|
    control fx, mix: 1
    notes = (scale :e3, :minor_pentatonic, num_octaves: 2).shuffle
    sn = synth :prophet , sustain: 8, note: :e1, cutoff: 70, cutoff_slide: 8
    control sn, cutoff: 130
    sleep 2
    32.times do
      control sn, note: notes.tick, pan: rrand(-1, 1)
      sleep 0.125
    end
  end
end
```
