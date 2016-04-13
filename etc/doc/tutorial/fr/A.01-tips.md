A.1 Conseils pour Sonic Pi

# Les cinq meilleurs conseils

## 1. Il n'y a pas d'erreur

La plus importante leçon à apprendre avec Sonic Pi c'est qu'il n'y a
vraiment pas d'erreur. La meilleure façon d'apprendre c'est juste
d'essayer, essayer, et essayer. Essayez beaucoup de choses
différentes, ne vous inquiétez pas de savoir si votre code sonne bien
ou pas et commencez par expérimenter avec le plus de synthés, notes,
effets et options possibles. Vous découvrirez beaucoup de choses qui
vous feront rire parce qu'elles ne sonnent pas bien du tout et aussi
quelques joyaux qui sonnent magnifiquement bien. Débarrassez-vous
ensuite de ce que vous n'aimez pas et gardez les choses qui vous
plaisent. Plus vous vous permettrez de faire des erreurs et plus vous
apprendrez et découvrirez votre son de code personnel.

## 2. Utilisez les effets

Disons que vous maîtrisez déjà les bases de Sonic Pi pour créer des
sons avec `sample` et `play`. Qu'est-ce qui vient ensuite ? Savez-vous
que Sonic Pi supporte plus de 27 effets studio pour changer le son de
votre code ? Les effets sont comme des filtres pour images dans les
programmes de dessin, mais à la place de rendre l'image floue ou noir
et blanc, on peut ajouter de la reverb, de la distorsion ou de l'écho
au son. On peut voir ça comme brancher le câble d'une guitare dans une
pédale d'effet puis dans un ampli. Heureusement Sonic Pi rend
l'utilisation d'effets très simple et n'a pas besoin de câble. Tout
ce dont vous avez besoin c'est de choisir à quelle section de votre
code ajouter l'effet puis de l'entourer avec le code de l'effet.
Prenons un exemple. Disons que vous avez le code suivant :


    sample :loop_garzul
     
    16.times do
      sample :bd_haus
      sleep 0.5
    end


Si vous voulez ajouter un effet au sample `:loop_garzul`, il suffit de
le mettre dans un bloc `with_fx` comme ceci :


    with_fx :flanger do
      sample :loop_garzul
    end
     
    16.times do
      sample :bd_haus
      sleep 0.5
    end


Maintenant si voulez ajouter un effet au tambour basse, enveloppez-le
aussi dans un `with_fx` :


    with_fx :flanger do
      sample :loop_garzul
    end
     
    with_fx :echo do
      16.times do
        sample :bd_haus
        sleep 0.5
      end
    end


Rappelez-vous, vous pouvez entourer *n'importe quel* code dans
`with_fx` et tous les sons créés passeront dans cet effet.


## 3. Paramétrez vos synthés

Pour découvrir vraiment votre son de code vous voudrez savoir comment
modifier et contrôler les synthés et effets. Par exemple, vous voudrez
peut-être changer la durée d'une note, ajouter plus de reverb, ou
changer la durée entre échos. Heureusement, Sonic Pi vous donne un
niveau de contrôle incroyable pour faire cela avec des paramètres
optionnels ou opts pour faire court. Regardons ça rapidement. Copiez
ce code dans un buffer et exécutez-le :

    sample :guit_em9


Oh, un joli son de guitare ! Commençons à jouer un peu avec. Et si on
changeait sa fréquence ?

    sample :guit_em9, rate: 0.5


Hé, qu'est-ce que ce `rate: 0.5` que j'ai ajouté à la fin ? C'est ce
qu'on appelle une opt. Tous les synthés et samples de Sonic Pi les
supportent et il y en a beaucoup avec lesquels on peut jouer. Ils
sont disponibles pour les effets aussi. Essayez ceci :


    with_fx :flanger, feedback: 0.6 do
      sample :guit_em9
    end


Maintenant essayez d'augmenter ce feedback à 1 pour entendre des sons
fous ! Lisez la documentation pour des détails complets sur les
nombreuses opts disponibles.


## 5. Coder de manière interactive

La meilleure manière d'expérimenter rapidement et d'explorer Sonic Pi
est de coder de manière interactive. Cela vous permet de partir d'un
peu de code et de le changer de manière continue pendant qu'il est en
train de s'exécuter. Par exemple, si vous ne savez pas ce que le
paramètre cutoff fait à un sample, jouez avec. Essayons ! Copiez ce
code dans un de vos buffers Sonic Pi :


    live_loop :experiment do
      sample :loop_amen, cutoff: 70
      sleep 1.75
    end


Maintenant cliquez sur 'Run' et vous entendrez un rhythme de batterie
un peu étouffé. Maintenant changez la valeur de `cutoff:` en `80` et
cliquez à nouveau sur 'Run'. Entendez-vous la différence ? Essayez
`90`, `100`, `110`...

Quand vous aurez pris la main à utiliser des `live_loop`, vous ne
pourrez plus vous en passer. Quand je donne un concert de live coding
je m'appuie autant sur `live_loop` qu'un batteur sur ses baguettes.
Pour plus d'informations à propos du live coding regardez la section
9 du tutoriel inclus dans Sonic Pi.

## 5. Surfez sur les suites aléatoires

Enfin, une chose que j'adore faire est de tricher en faisant que Sonic
Pi compose des choses pour moi. Une manière géniale de faire ça est
d'utiliser l'aléatoire. Cela peut paraître compliqué mais ça ne l'est
pas du tout. Regardons. Copiez ceci dans un buffer :


    live_loop :rand_surfer do
      use_synth :dsaw
      notes = (scale :e2, :minor_pentatonic, num_octaves: 2)
      16.times do
        play notes.choose, release: 0.1, cutoff: rrand(70, 120)
        sleep 0.125
      end
    end


Maintenant, quand vous jouez cela, vous entendrez une suite continue
de notes aléatoires de la gamme `:e2 :minor_pentatonic` jouée avec le
synthé `:dsaw`. "Attendez, attendez ! Ce n'est pas une mélodie", vous
entends-je crier ! Eh bien, voici la première partie du tour de magie.
Chaque fois que l'on recommence le `live_loop` on peut dire à Sonic Pi
de fixer la suite aléatoire à un point connu. C'est un peu comme
voyager dans le temps. Essayons ceci : ajoutez la ligne
`use_random_seed 1` au `live_loop` :

    live_loop :rand_surfer do
      use_random_seed 1
      use_synth :dsaw
      notes = (scale :e2, :minor_pentatonic, num_octaves: 2)
      16.times do
        play notes.choose, release: 0.1, cutoff: rrand(70, 120)
        sleep 0.125
      end
    end
    
Maintenant, chaque fois que la `live_loop` boucle, la suite aléatoire
est réinitialisée. Cela veut dire qu'elle contient exactement les même
16 notes à chaque fois. Et voilà ! Une mélodie de composée. Et
maintenant voici la partie excitante. Changez la valeur de `1` en un
autre nombre. Par exemple `4923`. Ouaou, une autre mélodie ! Donc, en
changeant juste un nombre (la graine aléatoire), on peut explorer
autant de combinaisons mélodiques qu'on peut imaginer ! C'est ça la
magie du code.
