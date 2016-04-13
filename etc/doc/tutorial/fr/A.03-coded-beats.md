A.3 Rythmes codés

# Rythmes codés

Une des évolutions techniques les plus excitantes et disruptives dans
la musique moderne a été l'invention des samplers. C'étaient des
boîtes qui permettaient d'enregistrer n'importe quels sons dedans et
de les manipuler et jouer de nombreuses manières intéressantes. Par
exemple, on pouvait prendre un vieux disque, trouver un solo de
batterie (ou un break), l'enregistrer dans le sampler et ensuite le
jouer deux fois moins vite pour créer la fondation de vos derniers
rythmes. C'est ainsi que la musique hip-hop est née et aujourd'hui il
est presque impossible de trouver de la musique électronique qui
n'incorpore pas de samples. Utiliser des samples est une très bonne
manière d'introduire facilement des éléments nouveaux et intéressants
dans vos performances de live coding.

Comment pouvez-vous vous procurer un sampler ? Eh bien, vous en avez
déjà un : c'est votre Raspberry Pi ! L'application de live coding
Sonic Pi comprend un sampler très puissant. Jouons avec !

## Le break Amen

Un des samples de break de batterie les plus classiques et
reconnaissables s'appelle le break Amen. Il a été joué pour la
première fois en 1969 dans la chanson "Amen Brother" des Winstons dans
un break de batterie. Cependant c'est quand il a été découvert par
les premiers musiciens de hip-hop dans les années 80 et utilisé dans
des samplers qu'il a commencé à être très utilisé dans des styles
aussi variés que le drum and bass, le breakbeat, la techno hardcore et
le breakcore.


Je suis sûr que vous êtes ravis d'entendre qu'il est aussi inclus dans
Sonic Pi. Choisissez un buffer vide et copiez-y ce code :
 
    sample :loop_amen

Cliquez sur *Run* et boum ! Vous ententez l'un des breaks de batterie
les plus influents de l'histoire de la musique dance. Ceci dit, ce
sample n'est pas célèbre pour être joué juste une fois, il est fait
pour être joué en boucle.


## Etirer des rythmes

Jouons le break Amen en boucle en utilisant notre vieille amie la
`live_loop`, introduite dans le tutoriel du mois dernier :

    live_loop :amen_break do
      sample :loop_amen
      sleep 2
    end

OK, ça boucle, mais il y a une pause ennuyeuse à chaque fois. C'est
parce qu'on a demandé d'attendre `2` temps et avec le nombre par défaut
de 60 BPM (battements par minute), le sample `:loop_amen` ne dure que
`1.753` temps. Nous avons donc un silence de `2 - 1.753 = 0.247`
temps. Même s'il est court, c'est notable.

Pour corriger ce problème on peut utiliser l'option `beat_stretch`
pour demander à Sonic Pi d'étirer (ou rétrécir) le sample pour
correspondre au temps spécifié.

[Breakout box start] Les fonctions `sample` et `synth` de Sonic Pi
vous donnent beaucoup de contrôle via des paramètres optionnels comme
`amp:`, `cutoff:` et `release:`. Cela dit le terme `paramètre
optionnel` est très long à dire donc on les appelle juste *opts* pour
rester simple.
[Breakout box end]

    live_loop :amen_break do
      sample :loop_amen, beat_stretch: 2
      sleep 2
    end  

Maintenant on peut danser ! Quoique, peut-être qu'on veut l'accélérer
ou le ralentir en fonction de l'ambiance.

## Jouer avec le temps

OK, et si on voulait changer de style pour faire du hip hop old school
ou du breakcore ? Une manière simple de faire ça est de jouer avec le
temps : ou en d'autres mots jouer avec le tempo. C'est très facile
avec Sonic Pi : il suffit d'appeler `use_bpm` dans votre boucle
interactive :

    live_loop :amen_break do
      use_bpm 30
      sample :loop_amen, beat_stretch: 2
      sleep 2
    end 

Pendant que vous rappez sur ces rythmes lents, remarquez que nous
avons toujours un repos de 2 et que notre BPM vaut 30, mais tout est
bien synchronisé. L'option `beat_stretch` marche avec le BPM courant
pour s'assurer que tout fonctionne bien.

Maintenant voici la partie amusante. Alors que la boucle est en train
de tourner, changez le `30` dans la ligne `use_bpm 30` en `50`. Oh,
tout est devenu plus rapide, mais toujours *synchro* ! Essayez d'aller
plus vite : jusque 80, jusque 120, ou même, soyons fous, 200 !


## Filtrer

Maintenant qu'on peut jouer des samples en boucle de manière
interactive, regardons les options les plus amusantes proposées par le
synthé `sample`. La première est le `cutoff:` qui contrôle le filtre
de coupure du sampler. Par défaut il est désactivé mais on peut
facilement l'activer :


    live_loop :amen_break do
      use_bpm 50
      sample :loop_amen, beat_stretch: 2, cutoff: 70
      sleep 2
    end  


Allez-y, changez la valeur de l'option `cutoff:`. Par exemple
montez-la à 100, cliquez sur *Run* et attendez la prochaine boucle
pour entendre la différence dans le son. Remarquez que des valeurs
basses comme 50 sonnent plus doux et bas et que des hautes valeurs
comme 100 ou 120 sonnent plus plein et râpeux. C'est parce que
l'option `cutoff:` va couper les parties haute-fréquence du son tout
comme une tondeuse coupe le haut de la pelouse. L'option `cutoff:`
est comme le réglage de la longueur : cela détermine combien il reste
d'herbe.


## Couper

Un autre super outil avec lequel on peut jouer est l'effet slicer.
Il va couper le son en tranches. Entourez la ligne de `sample` avec le
code de l'effet ainsi :

    live_loop :amen_break do
      use_bpm 50
      with_fx :slicer, phase: 0.25, wave: 0, mix: 1 do
        sample :loop_amen, beat_stretch: 2, cutoff: 100
      end
      sleep 2
    end

Remarquez comme le son bondit un peu plus de haut en bas. (Vous pouvez
entendre le son original sans l'effet en changeant l'option `mix:` en
`0`.) Maintenant essayez de modifier la valeur de l'option `phase:`.
C'est la fréquence (en battements) de l'effet de coupe. Une plus
petite valeur comme `0.5` va couper plus rapidement et une plus grande
valeur comme `0.5` va couper plus lentement. Remarquez que si on
divise ou multiplie successivement par deux l'option `phase:` cela
sonne généralement bien. Enfin, choisissez une valeur pour l'option
`wave:` entre 0, 1 et 2 et écoutez comment le son change. Ce sont
différentes formes d'onde. 0 est une onde scie (elle commence fort et
finit en fondu), 1 est une onde carrée (commence et finit fort) et 2
est une onde en triangle (commence en fondu, finit en fondu).

## Rassemblons tout

Enfin, revenons dans le temps et revisitons la jeune scène de drum and
bass de Bristol avec l'exemple de ce mois. Ne vous inquiétez pas trop
de tout ce que cela signifie, tapez le, cliquez sur Run, puis
commencez à coder interactivement en changement les valeurs des
options et voyez où cela vous amène. Partagez ce que vous créez ! A la
prochaine...

    use_bpm 100
  
    live_loop :amen_break do
      p = [0.125, 0.25, 0.5].choose
      with_fx :slicer, phase: p, wave: 0, mix: rrand(0.7, 1) do
        r = [1, 1, 1, -1].choose
        sample :loop_amen, beat_stretch: 2, rate: r, amp: 2
      end
      sleep 2
    end
  
    live_loop :bass_drum do
      sample :bd_haus, cutoff: 70, amp: 1.5
      sleep 0.5
    end
  
    live_loop :landing do
      bass_line = (knit :e1, 3, [:c1, :c2].choose, 1)
      with_fx :slicer, phase: [0.25, 0.5].choose, invert_wave: 1, wave: 0 do
        s = synth :square, note: bass_line.tick, sustain: 4, cutoff: 60
        control s, cutoff_slide: 4, cutoff: 120
      end
      sleep 4
    end
