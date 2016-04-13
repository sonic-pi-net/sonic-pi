A.7 Battements de Bizet

# Battements de Bizet

Après notre briève excursion dans le monde fantastique de la
programmation Minecraft avec Sonic Pi le mois dernier, penchons nous
sur la musique à nouveau. Aujourd'hui nous allons amener un morceau
classique de danse d'opéra droit dans le 21e siècle en utilisant
la puissance fantastique du code.

# Scandaleux et perturbateur

Sautons dans une machine à remonter le temps jusqu'en 1875. Un
compositeur nommé Bizet avait juste terminé son dernier opéra :
Carmen. Malheureusement comme beaucoup de nouveaux morceaux excitants
et perturbateurs les gens ne l'aimaient pas du tout au début parce
qu'il était trop scandaleux et différent. Malheureusement Bizet est
mort dix ans avant que l'opéra ne connaisse un grand succès
international et ne devienne un des opéras les plus connus et les plus
fréquemment interprétés de tous les temps. Par sympathie pour cette
tragédie nous allons prendre un des thèmes principaux de Carmen et
nous allons le convertir dans un format moderne de musique qui est
aussi trop scandaleux et différent pour la plupart des gens de notre
époque : la musique codée interactive !

# La Habanera décryptée

Essayez de programmer un opéra entier de manière interactive serait un
peu ambitieux pour ce tutoriel, concentrons nous sur une des ses plus
célèbres parties : la ligne de basse de la Habanera :

![Habanera Riff](../images/tutorial/articles/A.07-bizet/habanera.png)

Cela peut vous sembler complètement illisible si vous n'avez pas
étudié la notation musicale. Cependant, en tant que programmeurs nous
voyons la notation musicale comme juste une autre forme de code, elle
représente juste des instructions pour un musicien au lieu d'un
ordinateur.

# Notes

Les notes sont arrangées de gauche à droite comme les mots dans ce
magasine mais elles ont aussi différentes hauteurs. *La hauteur sur la
partition représente la hauteur d'une note.* Plus une note est haut
sur la partition, plus sa hauteur est grande.

Dans Sonic Pi nous savons déjà comment changer la hauteur d'une note :
on peut utiliser des grands ou petits nombres comme `play 75` et
`play 80` ou on peut utiliser les noms des notes : `play :E` et
`play :F`. Heureusement chacune des positions verticales sur la
partition représente un nom de note en particulier. Jetez un oeil à
cette table de correspondance bien pratique :

![Notes](../images/tutorial/articles/A.07-bizet/notes.png)

# Silences

Les partitions sont une sorte de code extrêmement riche et expressif
capable de communiquer de nombreuses choses. Cela ne devrait donc pas
nous surprendre que les partitions peuvent non seulement nous dire
quelles notes jouer mais aussi quand *ne pas* jouer de note. En
programmation c'est à peu près l'équivalent de l'idée de `nil` ou
`null` : l'absence de valeur. En d'autres mots ne pas jouer une note
c'est comme une absence de note.

Si vous regardez de près la partition vous verrez que c'est en fait
une combinaison de ronds noirs avec des barres qui représentent les
notes à jouer et des choses ondulées qui représentent les silences.
Heureusement, Sonic Pi a une notation très pratique pour un silence :
`:r`, donc si on exécute `play :r` il jouera en fait un silence ! On
pourrait aussi écrire `play :rest`, `play nil` ou `play false` qui
sont autant de manières équivalentes de représenter un silence.

# Rythme

Enfin il y a une dernière chose à apprendre à décoder dans la
notation : la notion du temps. Dans la notation originale vous verrez
que les notes sont liées par des traits épais. La deuxième note a
deux de ces traits ce qui veut dire qu'elle dure un 16e de temps. Les
autres notes ont un seul trait ce qui veut dire qu'elles durent un 8e
de temps. Le silence a aussi deux traits ondulés ce qui veut dire
qu'il représente aussi un 16e de temps.

Quand on essaie de décoder et d'explorer de nouvelles choses un truc
très pratique est de rendre tout le plus semblable possible pour
essayer de voir des relations ou modèles. Par exemple quand on réécrit
notre notation uniquement en double-croches on peut voir que notre
notation devient une séquence agréable de notes et de silences.


![Habanera Riff 2](../images/tutorial/articles/A.07-bizet/habanera2.png)

# Re-programmer la Habanera

Nous sommes maintenant prêts a traduire cette ligne de basse dans
Sonic Pi. Encodons ces notes et silences dans un anneau :

    (ring :d, :r, :r, :a, :f5, :r, :a, :r)
    
Voyons ce que ça donne. Jetons ça dans une boucle interactive et
parcourons-là :

    live_loop :habanera do
      play (ring :d, :r, :r, :a, :f5, :r, :a, :r).tick
      sleep 0.25
    end
    
Fabuleux, cette mélodie qu'on reconnait immédiatement prend vie dans
vos haut-parleurs. On a fait des efforts pour en arriver là, mais ça
valait la peine, bravo !
    
# Synthés de mauvaise humeur

Maintenant qu'on a la ligne de basse, essayons de re-créer une partie
de l'ambiance de la scène d'opéra. Un synthé à essayer est `:blade`
qui est un synthé style années 80. Essayons le avec la note de départ
`:d` passée dans un slicer et de la reverb :

    live_loop :habanera do
      use_synth :fm
      use_transpose -12
      play (ring :d, :r, :r, :a, :f5, :r, :a, :r).tick
      sleep 0.25
    end

    with_fx :reverb do
      live_loop :space_light do
        with_fx :slicer, phase: 0.25 do
          synth :blade, note: :d, release: 8, cutoff: 100, amp: 2
        end
        sleep 8
      end
    end

Maintenant essayez les autres notes de la ligne de basse : `:a` et
`:f5`. Souvenez-vous que vous n'avez pas besoin de cliquer sur 'Stop',
vous pouvez juste modifier le code pendant que la musique tourne et
ensuite cliquer sur 'Run' à nouveau. Aussi essayez différentes valeurs
pour l'option `phase:` du slicer comme `0.5`, `0.75` et `1`.


## Rassemblons tout

Enfin, combinons toutes les idées vues jusqu'ici dans un nouveau remix
de la Habanera. Vous remarquerez peut-être que j'ai inclus une autre
partie de la ligne de basse en commentaire. Quand vous aurez tout tapé
dans un buffer de libre cliquez sur 'Run' pour entendre la
composition. Maintenant, sans cliquer sur 'Stop', *décommentez* la
seconde ligne en enlevant le `#` et cliquez sur 'Run' à nouveau :
c'est merveilleux, non ? Maintenant amusez-vous à le modifier vous-même.

    use_debug false
    bizet_bass = (ring :d, :r, :r, :a, :f5, :r, :a, :r)
    #bizet_bass = (ring :d, :r, :r, :Bb, :g5, :r, :Bb, :r)
     
    with_fx :reverb, room: 1, mix: 0.3 do
      live_loop :bizet do
        with_fx :slicer, phase: 0.125 do
          synth :blade, note: :d4, release: 8,
            cutoff: 100, amp: 1.5
        end
        16.times do
          tick
          play bizet_bass.look, release: 0.1
          play bizet_bass.look - 12, release: 0.3
          sleep 0.125
        end
      end
    end
     
    live_loop :ind do
      sample :loop_industrial, beat_stretch: 1,
        cutoff: 100, rate: 1
      sleep 1
    end
     
    live_loop :drums do
      sample :bd_haus, cutoff: 110
      synth :beep, note: 49, attack: 0,
        release: 0.1
      sleep 0.5
    end

