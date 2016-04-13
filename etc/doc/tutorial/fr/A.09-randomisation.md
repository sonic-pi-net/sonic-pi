A.9 L'aléatoire

# Surfer sur des flux aléatoires

Dans le quatrième épisode de cette série de tutoriels nous avons jeté
un oeil brièvement à l'aléatoire en codant des riffs grésillants de
synthé. Vu comme l'aléatoire est une partie très importante de mes
performances de live coding j'ai pensé qu'il serait utile d'en couvrir
les bases plus en détail. Mettez votre casquette et surfons sur des
flux aléatoires !

# Il n'y a pas d'aléatoire

La première chose à apprendre qui pourrait vraiment vous surprendre en
jouant avec les fonctions aléatoires de Sonic Pi c'est qu'elles ne
sont en fait pas vraiment aléatoires. Qu'est-ce que cela signifie ?
Eh bien, faisons quelques essais. Commencez par imaginer un nombre
dans votre tête entre 0 et 1. Gardez-le en tête et ne me le dites pas.
Maintenant, laissez-moi deviner... est-ce que c'était `0.321567` ?
Non ? Bah, je ne suis clairement pas bon à ce jeu. Essayons encore une
fois, mais demandons à Sonic Pi de choisir un nombre cette fois.
Lancez Sonic Pi version 2.7 ou plus et demandez lui un nombre
aléatoire mais de nouveau ne me dites pas le résultat.


    print rand
    
Et maintenant pour le truc... est-ce que c'était `0.75006103515625` ?
Oui ! Ha, je vois que vous êtes un peu sceptique. C'était peut-être
juste de la chance. Essayons encore. Cliquez sur 'Run' à nouveau et
regardons ce qu'on obtient... Quoi ? Encore `0.75006103515625` ? Ca ne
peut clairement pas être aléatoire ! Vous avez raison, ça ne l'est pas.

Qu'est-ce qui se passe ici ? Le mot savant d'informatique pour ceci
est le déterminisme. Cela veut juste dire que rien n'a lieu par hasard
et que tout a sa destinée. Votre version de Sonic Pi a le destin de
toujours retourner `0.75006103515625` dans le programme ci-dessus.
Cela peut sembler assez inutile, mais laissez moi vous assurer que
c'est une des parties les plus puissantes de Sonic Pi. Si vous
persévérez vous apprendrez comment compter sur la nature
déterministique de l'aléatoire dans Sonic Pi comme un bloc de
construction fondamental pour vos compositions et performances de live
coding.

# Une mélodie aléatoire

Quand Sonic Pi démarre il charge en fait en mémoire une séquence de
441 000 valeurs aléatoires pré-générées. Quand vous appelez une
fonction aléatoire comme `rand` ou `rrand`, ce flux aléatoire est
utilisé pour générer votre résultat. Chaque appel à une fonction
aléatoire consomme une valeur de ce flux. Ainsi le dixième appel à une
fonction aléatoire utilisera la dixième valeur du flux. Aussi, chaque
fois que vous cliquez sur le bouton 'Run', le flux est réinitialisé
pour cette exécution. C'est pour cela que j'ai pu prédire le résultat
de `rand` et pourquoi la mélodie 'aléatoire' était la même à chaque
fois. La version de Sonic Pi de chacun utilise exactement le même flux
aléatoire ce qui est très important quand on commence à partager nos
morceaux.

Utilisons cette connaissance pour générer une mélodie aléatoire
répétable :

    8.times do
     play rrand_i(50, 95)
     sleep 0.125
    end

Tapez ceci dans un buffer de libre et cliquez sur 'Run'. Vous
entendrez une mélodie de notes aléatoires entre 50 et 95. Quand elle
aura fini, cliquez sur 'Run' à nouveau pour entendre exactement la
même mélodie à nouveau.

*start breakout box*
## Des fonctions aléatoires pratiques

Sonic Pi contient un certain nombre de fonctions utiles pour
travailler avec le flux aléatoire. Voici une liste des plus utiles :

* `rand` : retourne simplement la prochaine valeur dans le flux aléatoire
* `rrand` : retourne une valeur aléatoire dans un intervalle
* `rrand_i` : retourne une valeur entière aléatoire dans un intervalle
* `one_in` : retourne vrai ou faux pour une probabilité donnée
* `dice` : imite un jet de dé et retourne une valeur entre 1 et 6
* `choose` : choisit une valeur aléatoire dans une liste

Regardez leur documentation dans le système d'aide pour des
informations détaillées et des exemples.

 *end breakout box*

# Réinitialiser le flux

Même si la capacité de répéter une séquence de notes choisies est
essentielle pour vous permettre de rejouer une mélodie sur la piste de
danse, ça pourrait ne pas être exactement la mélodie que vous
souhaitez. Ne serait-ce pas génial si on pouvait essayez différentes
mélodies et choisir celle qu'on préfère ? C'est ici que la vraie
magie commence.

On peut régler le flux manuellement avec la fonction
`use_random_seed`. En informatique, une graine aléatoire est le point
de départ à partir duquel un nouveau flux de valeurs aléatoires peut
fleurir. Essayons-le :


    use_random_seed 0
    3.times do
      play rrand_i(50, 95)
      sleep 0.125
    end
    
Super, on récupère les trois premières notes de notre mélodie
aléatoire ci-dessus : `84`, `83` et `71`. Cependant on peut
maintenant changer la graine. Par exemple :

    use_random_seed 1
    3.times do
      play rrand_i(50, 95)
      sleep 0.125
    end
    
    
Intéressant, on obtient `83`, `71` et `61`. Vous avez peut-être
remarqué que les deux premiers nombres ici sont les mêmes que les deux
derniers nombres d'avant : ce n'est pas une coïncidence.

Rappelez-vous que le flux aléatoire est juste une liste géante de
valeurs pré-choisies. Choisir une graine aléatoire nous déplace juste
en un point de la liste. Une autre manière de voir ça est d'imaginer
un énorme jeu de cartes pré-mélangées. Utiliser une graine aléatoire,
c'est couper le jeu en un point particulier. Ce qui est fabuleux avec
ça c'est qu'on peut se déplacer dans le flux aléatoire, ce qui nous
donne un énorme pouvoir quand on fait de la musique.

Revisitons notre mélodie aléatoire de huit notes avec cette nouvelle
capacité de réinitialiser le flux, et mettons là dans une boucle
interactive pour pouvoir expérimenter pendant qu'elle joue :

    live_loop :random_riff do    
      use_random_seed 0
      8.times do
        play rrand_i(50, 95), release: 0.1
        sleep 0.125
      end
    end

Maintenant, pendant qu'elle est en train de jouer, changez la valeur
de la graine de `0` en quelque chose d'autre. Essayez `100`, ou
pourquoi pas `999`. Essayez vos propres valeurs, expérimentez et
amusez-vous : voyez quelle graine génère la mélodie que vous préférez.

# Rassemblons tout

Le tutoriel de ce mois a été un plongeon bien technique dans les 
entrailles de la fonctionnalité aléatoire de Sonic Pi. J'espère que
cela vous a montré un peu comment elle marche et comment vous pouvez
commencer à utiliser de l'aléatoire de manière fiable pour créer des
motifs reproductibles dans votre musique. Il est important de souligner
qu'on peut utiliser de l'aléatoire reproductible *où* on veut. Par
exemple, on peut rendre aléatoire l'amplitude des notes, le mix d'un
effet, etc. Dans l'avenir nous regarderons de plus près certaines de
ces applications, mais pour le moment je vais vous laisser avec un
exemple court.

Tapez le code suivant dans un buffer disponible, cliquez sur 'Run' et
commencez à modifier les graines, cliquez sur 'Run' de nouveau
(pendant que le code tourne) et explorez les différents sons, rythmes
et mélodies que vous pouvez créer. Quand vous en trouvez une qui sonne
bien, notez la graine pour pouvoir y revenir plus tard. Enfin, quand
vous aurez trouvé quelques graines qui vous plaisent, donnez un
concert de live coding à vos amis en changeant simplement entre vos
graines préférées pour créer un morceau entier.

live_loop :random_riff do
  use_random_seed 10300
  use_synth :prophet
  s = [0.125, 0.25, 0.5].choose
  8.times do
    r = [0.125, 0.25, 1, 2].choose
    n = (scale :e3, :minor).choose
    co = rrand(30, 100)
    play n, release: r, cutoff: co
    sleep s
  end
end

live_loop :drums do
  use_random_seed 2001
  16.times do
    r = rrand(0.5, 10)
    sample :drum_bass_hard, rate: r, amp: rand
    sleep 0.125
  end
end


