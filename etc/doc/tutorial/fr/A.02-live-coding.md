A.2 Live Coding

# Live Coding

Les rayons lasers perçaient à travers les bouffées de fumée et les
baffles transmettaient des basses profondes dans les corps de la foule.
L'atmosphère était remplie d'un mélange de synthés et de danse. Il y
avait pourtant quelque chose d'étrange dans cette boîte de nuit.
Du texte futuriste était projeté au dessus de la cabine du DJ,
bougeant, dansant, clignotant. Ce n'étaient pas de belles
visualisations, c'était juste une projection de Sonic Pi tournant sur
un Raspberry Pi. La personne dans la cabine de DJ n'était pas en train
de tourner des disques, elle écrivait, éditait et évaluait du code. En
direct. C'est ça le Live Coding.

![Sam Aaron Live Coding](../images/tutorial/articles/A.02-live-coding/sam-aaron-live-coding.png)

Cela peut sonner comme une histoire tirée par les cheveux dans une
boîte de nuit futuriste mais coder de la musique comme cela est une
tendance qui se développe et qu'on appelle souvent Live Coding
(programmation interactive) (http://toplap.org). Une des directions
récentes que cette approche de la musique a prise est l'Algorave
(http://algorave.com) : des événements où des artistes comme moi
codent de la musique pour que les gens dansent. Cependant vous n'avez
pas besoin d'une boîte de nuit pour coder en live : avec Sonic Pi
version 2.6 et plus vous pouvez le faire dans n'importe quel endroit
où vous pouvez prendre votre Raspberry Pi et un casque ou des
haut-parleurs. Quand vous aurez atteint la fin de cet article, vous
saurez programmez vos rythmes et les modifier en direct. Où vous irez
ensuite ne sera limité que par votre imagination.

## Boucle interactive

La clef de la programmation interactive est la maîtrise de la
`live_loop`. En voici une :

    live_loop :beats do
      sample :bd_haus
      sleep 0.5
    end

Il y a quatre ingrédients principaux dans une `live_loop`. La première
est son nom. Notre `live_loop` s'appelle ici `:beats`. Vous pouvez
donner le nom que vous voulez à votre `live_loop`. Défoulez-vous.
Soyez créatifs. J'utilise souvent des noms qui communiquent à
l'audience qu'est-ce que la musique va faire. Le deuxième ingrédient
est le mot `do` qui marque où la `live_loop` commence. Le troisième
est le mot `end` qui marque où la `live_loop` finit, et enfin il y a
le corps de la `live_loop` qui décrit ce que la boucle va répéter :
c'est la partie entre `do` et `end`. Dans ce cas on joue en boucle un
sample de tambour basse et on attend un demi temps. Cela produit un
beau rythme régulier de basse. Allez, copiez-le dans un buffer vide de
Sonic Pi et cliquez sur 'Run'. Boum, boum, boum !

## Redéfinir à la volée

OK, qu'est-ce qu'elle a de si spécial, cette `live_loop` ? Jusqu'ici
on dirait que c'est juste une boucle ! Eh bien, la beauté des
`live_loop`s c'est qu'on peut les redéfinir à la volée. Cela veut
dire que pendant qu'elles sont en train de tourner, on peut changer ce
qu'elles font. C'est le secret de la programmation interactive avec
Sonic Pi. Essayons :

    live_loop :choral_drone do
      sample :ambi_choir, rate: 0.4
      sleep 1
    end

Maintenant cliquez le bouton 'Run' ou tapez 'alt-r'. Vous entendez
maintenant de beaux sons de chorale. Ensuite, alors qu'il est encore
en train de jouer, changer la fréquence de `0.4` en `0.38`. Cliquez à
nouveau sur 'Run'. Ouaou ! Vous avez entendu le choeur changer de
note ? Ecrivez à nouveau `0.4` pour revenir comme avant. Puis `0.2`,
puis `0.19`, puis à nouveau `0.4`. Voyez-vous comment juste changer un
paramètre à la volée vous donne un réel contrôle sur la musique ?
Maintenant jouez vous-même avec la fréquence, choisissez vos propres
valeurs. Essayez des nombres négatifs, de très petits nombres et de
grands nombres. Amusez-vous !

## Il est important de dormir

Une des leçons les plus importantes avec les `live_loop` c'est
qu'elles ont besoin de se reposer. Prenons par exemple cette
`live_loop` :

    live_loop :infinite_impossibilities do
      sample :ambi_choir
    end

Si vous essayez d'exécuter ce code, vous verrez immédiatement que
Sonic Pi se plaint que la `live_loop` n'a pas dormi. C'est un
mécanisme de sécurité qui se met en place. Prenons un moment pour
penser à ce que ce code demande à l'ordinateur de faire. C'est cela,
on demande à l'ordinateur de jouer un nombre infini de samples de
chorale dans un temps nul. Sans le mécanisme de sécurité le pauvre
ordinateur essaierait de faire ça et exploserait. Souvenez-vous en
bien : vos `live_loop`s doivent contenir un appel à `sleep`.


## Combiner des sons

La musique est pleine de choses qui arrivent en même temps. La
batterie en même temps que la basse, en même temps que du chant, en
même temps que des guitares... En informatique on appelle ça la
concurrence et Sonic Pi nous donne une manière étonnamment simple de
jouer des choses en même temps. Il suffit d'utiliser plus qu'une
`live_loop` !

    live_loop :beats do
      sample :bd_tek
      with_fx :echo, phase: 0.125, mix: 0.4 do
        sample  :drum_cymbal_soft, sustain: 0, release: 0.1
        sleep 0.5
      end
    end
  
    live_loop :bass do
      use_synth :tb303
      synth :tb303, note: :e1, release: 4, cutoff: 120, cutoff_attack: 1
      sleep 4
    end

Ici nous avons deux `live_loop`s, une qui tourne rapidement pour faire
un rythme et une qui boucle lentement pour faire un son fou de basse.

Une des choses intéressantes quand on utilise plusieurs `live_loop`s
c'est que chacune gère son propre temps. Cela veut dire qu'il est très
facile de créer des structures polyrythmiques intéressantes et même de
jouer avec la phase dans le style de Steve Reich. Par exemple :

    # La phase piano de Steve Reich
  
    notes = (ring :E4, :Fs4, :B4, :Cs5, :D5, :Fs4, :E4, :Cs5, :B4, :Fs4, :D5, :Cs5)
  
    live_loop :slow do
      play notes.tick, release: 0.1
      sleep 0.3
    end
  
    live_loop :faster do
      play notes.tick, release: 0.1
      sleep 0.295
    end


## Rassemblons tout

Dans chacun de ces tutoriels, nous finirons avec un exemple qui montre
un nouveau morceau de musique qui utilise toutes les idées
introduites. Lisez ce code et essayez d'imaginer ce qu'il fait.
Ensuite, copiez-le dans un buffer frais de Sonic Pi et cliquez 'Run'
pour entendre comment il sonne. Enfin changez un des nombres ou
commentez / décommentez des parties. Voyez si vous pouvez prendre ça
comme point de départ d'une nouvelle performance, et surtout
amusez-vous ! A la prochaine...

    with_fx :reverb, room: 1 do
      live_loop :time do
        synth :prophet, release: 8, note: :e1, cutoff: 90, amp: 3
        sleep 8
      end
    end
  
    live_loop :machine do
      sample :loop_garzul, rate: 0.5, finish: 0.25
      sample :loop_industrial, beat_stretch: 4, amp: 1
      sleep 4
    end
  
    live_loop :kik do
      sample :bd_haus, amp: 2
      sleep 0.5
    end
  
    with_fx :echo do
      live_loop :vortex do
        # use_random_seed 800
        notes = (scale :e3, :minor_pentatonic, num_octaves: 3)
        16.times do
          play notes.choose, release: 0.1, amp: 1.5
          sleep 0.125
        end
      end
    end
