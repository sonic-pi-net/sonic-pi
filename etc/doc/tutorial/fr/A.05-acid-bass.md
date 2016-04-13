A.5 Basse acide

# Basse acide

Il est impossible de regarder l'histoire de la musique de danse
électronique sans voir l'énorme impact du petit synthétiseur Roland
TB-303. C'est la sauce secrète derrière le son original de basse
acide. On entend ces riffs de basse classiques crissants et
pataugeants depuis la jeune scène de house de Chicago jusqu'à des
artistes plus récents comme Plastikman, Squarepusher et Aphex Twin.

Il est intéressant de savoir que Roland n'avait pas l'intention que le
TB-303 soit utilisé dans de la musique pour danser. Il a été créé à
l'origine comme une aide de travail pour les guitaristes. Ils ont
imaginé que les gens les programmeraient pour jouer des lignes de
basse d'accompagnement. Malheureusement il y avait un certain nombre
de problèmes : ils étaient un peu compliqués à programmer, ne
sonnaient pas particulièrement bien comme remplacement de guitare
basse et ils étaient assez chers. Décidant d'arrêter les frais, Roland
a cessé d'en fabriquer après en avoir vendu 10'000 unités et après un
certain nombres d'années à rester sur les étagères des guitaristes,
ils se retrouvés dans les vitrines des magasins d'occasion. C'est
pauvres TB-303 rejetés attendaient d'être découverts par une nouvelle
génération d'expérimentateurs qui ont commencé à les utiliser dans des
manières que Roland n'avait pas imaginées pour créer des sons fous.
La house acide était née.


Même si se procurer un TB-303 original n'est pas si facile vous serez
heureux d'apprendre que vous pouvez transformer votre Raspberry Pi en
TB-303 en utilisant la puissance de Sonic Pi. Lancez Sonic Pi, copiez
ce code dans un buffer vide et cliquez sur 'Run' :

    use_synth :tb303
    play :e1
    
Instant acid bass! Let's play around...

# Presse cette basse

Commençons par construire un arpège interactif pour rendre les choses
amusantes. Dans le dernier tutoriel nous avons vu comment des mélodies
peuvent être juste un anneau de notes qu'on joue les unes après les
autres, en répétant quand on arrive à la fin. Créons une boucle
interactive qui fait cela :

    use_synth :tb303
    live_loop :squelch do
      n = (ring :e1, :e2, :e3).tick
      play n, release: 0.125, cutoff: 100, res: 0.8, wave: 0
      sleep 0.125
    end
    
Regardons chaque ligne.

1. A la première ligne choisit `tb303` comme synthé par défaut avec la
  fonction `use_synth`.

2. A la deuxième ligne on crée une boucle interactive du nom de
   `:squelch` qui va juste boucler encore et encore.

3. A la troisième ligne on crée notre mélodie : un anneau de notes (E
   ou Mi aux octaves 1, 2 et 3) et on le parcourt simplement avec
   `.tick`. On définit `n` pour représenter la note courante de la
   mélodie. Le signe égal veut juste dire qu'on affecte la valeur à
   droite au nom à gauche. Cette valeur sera différente à chaque
   boucle. La première fois `n` aura la valeur `:e1`. La deuxième fois
   ce sera `:e2`, puis `:e3`, puis de nouveau `:e1`, en bouclant à
   l'infini.
   
4. A la ligne quatre on joue notre synthé `:tb303`. On lui passe
   quelques options intéressantes : `release:`, `cutoff:`, `res:` et
   `wave:` que l'on décrira plus bas.

5. La ligne cinq est notre `sleep` : on demande à la boucle
   interactive de boucler toutes les `0.125`secondes ou 8 fois par
   seconde au BPM par défaut de 60
   
6. La ligne six est la fin de la boucle interactive. Le mot `end`
   indique juste à Sonic Pi où se termine la boucle interactive.


Alors que vous êtes encore en train de vous familiariser avec ce qui
se passe, tapez le code ci-dessous et cliquez sur le bouton 'Run'.
Vous devriez entendre le `:tb303` entrer en action. C'est ici le coeur
de l'action : commençons à programmer de manière interactive.

Pendant que la boucle tourne, changez l'option `cutoff:` en `110`.
Puis cliquez à nouveau sur le bouton 'Run'. Vous devriez entendre le
son devenir un peu plus dur et sec. Montez à `120` et cliquez sur
'Run'. Puis `130`. Ecoutez comme les valeurs hautes de coupure rendent
le son plus perçant et intense. Enfin, descendez à `80` quand vous
sentirez que vous voulez un peu de repos. Puis répétez ça autant que
vous voulez. Pas d'inquiétude, je serai toujours là...

Une autre option qui vaut le coup d'être utilisée est `res:`. Elle
contrôle le niveau de résonance du filtre. Une haute résonance est
caractéristique des sons de basse acide. Nous avons pour le moment une
`res:` de `0.8`. Essayez de la monter à `0.85`, puis `0.9`, et enfin
`0.95`. Un cutoff de `110` ou plus peut permettre d'entendre plus
facilement les différences. Enfin défoulez-vous et montez jusque
`0.999` pour avoir des sons déments. Avec une `res` aussi haute vous
entendez le filtre de coupure résonner tant qu'il commence à faire ses
propres sons.

Enfin pour avoir un grand impact sur le timbre essayez de mettre
l'option `wave:` à `1`. C'est le choix de l'oscillateur source. La
valeur par défaut est `0` qui est une onde en dents de scie.

Bien sûr, essayez différentes mélodies en changeant les notes dans
l'anneau ou même en choisissant des notes de gammes ou d'accords.
Amusez-vous bien avec votre premier synthé de basse acide.

# Déconstruisons le TB-303

Le design du TB-303 original était en fait assez simple. Comme vous
pouvez le voir sur le diagramme suivant il n'y a que quatre parties
principales.

![TB-303 Design](../images/tutorial/articles/A.05-acid-bass/tb303-design.png)

En premier on a l'onde oscillatoire : les ingrédients de base du son.
Dans ce cas nous avons une onde carrée. Ensuite on a l'enveloppe
d'amplitude de l'oscillateur qui contrôle l'amplitude de l'onde carrée
au cours du temps. On peut y accéder dans Sonic Pi avec les options 
`attack:`, `decay:`, `sustain:` et `release:` ainsi que leur niveau
correspondant. Pour plus d'informations lisez la Section 2.4 'Durée
avec enveloppes' dans le tutoriel inclus dans Sonic Pi. On passe
ensuite notre onde carrée enveloppée dans un filtre passe bas
résonant. Cela coupe les hautes fréquences et a un bel effet de
résonance. Et c'est ici que ça devient amusant. La valeur de coupure
de ce filtre est aussi contrôlée par sa propre enveloppe ! Cela veut
dire que nous avons un contrôle incroyable sur le timbre du son en
jouant avec ces deux enveloppes. Jetons-y un oeil :

  
    use_synth :tb303
    with_fx :reverb, room: 1 do
      live_loop :space_scanner do
        play :e1, cutoff: 100, release: 7, attack: 1, cutoff_attack: 4, cutoff_release: 4
        sleep 8
      end
    end
    
Pour chaque option standard d'enveloppe, il y a une option `cutoff_`
équivalente dans le synthé `:tb303`. Donc, pour changer le temps
d'attaque de la coupure on peut utiliser l'option `:cutoff_attack`.
Copiez le code ci-dessus dans un buffer vide et cliquez sur 'Run'.
Vous entendrez un son fou entrer et sortir. Maintenant essayez de
jouer avec. Essayez de changer la durée de `cutoff_attack:` en `1`
puis en `0.5`. Puis essayez `8`.

Remarquez que j'ai passé tout cela à travers un effet `:reverb` pour
plus d'atmosphère : essayez d'autres effets pour voir ce qui marche !

## Rassemblons tout

Enfin voici un morceau que j'ai composé en utilisant les idées de ce
tutoriel. Copiez le dans un buffer vide, écoutez un peu, puis
commencez à programmer interactivement vos changements. Voyez quels
sons fous vous pouvez faire avec ! A la prochaine...

    use_synth :tb303
    use_debug false
     
    with_fx :reverb, room: 0.8 do
      live_loop :space_scanner do
        with_fx :slicer, phase: 0.25, amp: 1.5 do
          co = (line 70, 130, steps: 8).tick
          play :e1, cutoff: co, release: 7, attack: 1, cutoff_attack: 4, cutoff_release: 4
          sleep 8
        end
      end
     
      live_loop :squelch do
        use_random_seed 3000
        16.times do
          n = (ring :e1, :e2, :e3).tick
          play n, release: 0.125, cutoff: rrand(70, 130), res: 0.9, wave: 1, amp: 0.8
          sleep 0.125
        end
      end
    end
