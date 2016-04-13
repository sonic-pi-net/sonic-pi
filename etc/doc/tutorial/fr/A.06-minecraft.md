A.6 Minecraft musical

# Minecraft musical



Bonjour et bienvenue à nouveau ! Dans les tutoriels précédents nous
nous somes concentrés exclusivement sur les possibilités mnusicales de
Sonic Pi, en transformant votre Raspberry Pi en instrument de musique
prêt à l'emploi. Jusqu'ici nous avons appris comment :

* Programmer de manière interactive, en changeant les sons à la volée,
* Coder des rythmes imposants,
* Générer des mélodies de synthé puissantes
* recréer le fameux son TB-303 de basse acide

Il y a tellement plus à vous montrer, et nous l'explorerons dans de
futures éditions. Cependant, ce mois-ci, nous allons regarder une
chose que Sonic Pi sait faire et que vous n'avez probablement pas
remarquée : contrôler Minecraft.


# Bonjour, monde de Minecraft

OK, commençons. Démarrez votre Raspberry Pi, lancez Minecraft Pi et
créez un nouveau monde. Maintenant démarrez Sonic Pi et déplacez vos
fenêtres de manière à pouvoir voir Sonic Pi et Minecraft Pi en même
temps.

Dans un buffer disponible tapez ce qui suit :

    mc_message "Hello Minecraft from Sonic Pi!"
    
Maintenant cliquez sur 'Run'. Boum ! Votre message est apparu dans
Minecraft ! C'était facile, non ? Maintenant arrêtez un moment de lire
ceci et jouez un peu avec vos propres messages. Amusez-vous !

![Screen 0](../images/tutorial/articles/A.06-minecraft/Musical-Minecraft-0-small.png)

# Téléportation sonique

Explorons un peu. L'option standard est de saisir la souris et le
clavier et de commencer à se promener. Ca marche, mais c'est assez
lent et ennuyeux. Ce serait beaucoup mieux si on avait une sorte de
machine de téléportation. Eh bien, grâce à Sonic Pi, on en a une.
Essayez ceci :

    mc_teleport 80, 40, 100
    
Oh ! On est monté bien haut. Si vous n'étiez pas en mode volant vous
avez du retomber jusqu'au sol. Si vous double-tapez Espace pour entrer
en mode volant et vous téléportez à nouveau, vous resterez en l'air
à l'endroit où vous vous êtes déplacés.

Maintenant, qu'est-ce que ces nombres signifient ? Nous avons trois
nombres qui décrivent les coordonnées de l'endroit du monde où vous
voulez vous déplacer. On donne à chacun de ces nombres un nom : x, y
et z.

* x - à quelle distance vers la gauche ou la droite (80 dans notre exemple)
* y - à quelle hauteur on veut être (40 dans notre exemple)
* z - à quelle distance vers l'avant ou l'arrière (100 dans notre exemple)

En choisissant différentes valeurs pour x, y, et z, on peut se
téléporter *n'importe où* dans notre monde. Essayez ! Choisissez
différents nombres et regardez où vous vous retrouvez. Si l'écran
devient noir c'est que vous vous êtes téléportés sous terre ou dans
une montagne. Choisissez juste une valeur de y plus grande pour vous
retrouver au-dessus de la terre. Continuez à explorer jusqu'à ce que
vous trouviez un endroit qui vous plaise...

En utilisant les idées vues jusqu'ici, construisons un téléporteur
sonique qui fera un son amusant de téléportation quand il nous fera
voyager à travers le monde de Minecraft :

    mc_message "Preparing to teleport...."
    sample :ambi_lunar_land, rate: -1
    sleep 1
    mc_message "3"
    sleep 1
    mc_message "2"
    sleep 1
    mc_message "1"
    sleep 1
    mc_teleport 90, 20, 10
    mc_message "Whoooosh!"
    
![Screen 1](../images/tutorial/articles/A.06-minecraft/Musical-Minecraft-1-small.png)

# Blocs magiques

Maintenant que vous avez trouvé un endroit sympathique, commençons à
construire. Vous pourriez faire comme vous en avez l'habitude et
commencer à cliquer furieusement la souris pour placer des blocs sous
le curseur. Ou vous pourriez utiliser la magie de Sonic Pi. Essayez
ceci :


    x, y, z = mc_location
    mc_set_block :melon, x, y + 5, z

Maintenant regardez vers le haut ! Il y a un melon dans le ciel !
Prenez un moment pour regarder le code. Qu'est-ce qu'on a fait ? Sur
la première ligne on a attrapé l'emplacement actuel de Steve dans les
variables x, y et z. Elles correspondent aux coordonnées décrites
ci-dessus. Nous utilisons ces coordonnées dans la fonction
`mc_set_block` qui va placer le bloc de votre choix aux coordonnées
spécifiées. Pour placer quelque chose plus haut dans le ciel on doit
juste augmenter la valeur de y, c'est pour ça qu'on lui ajoute 5.
Faisons un chemin de melons :

    live_loop :melon_trail do
      x, y, z = mc_location
      mc_set_block :melon, x, y-1, z
      sleep 0.125
    end

Maintenant sautez dans Minecraft, assurez-vous d'être en mode volant
(tapez deux fois sur Espace sinon) et volez autour du monde. Regardez
derrière vous pour voir un beau chemin de blocs de melon ! Voyez quels
dessins tordus vous pouvez faire dans le ciel.

# Programmer Minecraft de manière interactive

Ceux d'entre vous qui ont suivi ce tutoriel pendant les derniers mois
doivent être bien étonnés. Le chemin de melons est assez cool, mais la
partie la plus excitante de l'exemple précédent est qu'on peut
utiliser la `live_loop` avec Minecraft ! Pour ceux qui ne savent pas,
la `live_loop` est la faculté magique et spéciale de Sonic Pi qu'aucun
autre langage de programmation ne possède. Elle vous permet d'exécuter
des boucles multiples en même temps et vous permet de les modifier
pendant qu'elles tournent. Elles sont incroyablement puissantes et
amusantes. J'utilise les `live_loop`s pour jouer de la musique dans
des boîtes de nuit avec Sonic Pi : les DJs utilisent des disques et
moi j'utilise des `live_loop`s :-) Cependant aujourd'hui nous allons
programmer de manière interactive de la musique et Minecraft.



Commençons. Exécutez le code ci-dessus et commencez à faire votre
chemin de melons de nouveau. Maintenant, sans arrêter le code, changez
simplement `:melon` en `:brick` et cliquez sur `Run`. Eh voilà, vous
construisez maintenant un chemin de briques. C'était simple, non ?
Un peu de musique pour accompagner ça ? Facile. Essayez ceci :

    live_loop :bass_trail do
      tick
      x, y, z = mc_location
      b = (ring :melon, :brick, :glass).look
      mc_set_block b, x, y -1, z
      note = (ring :e1, :e2, :e3).look
      use_synth :tb303
      play note, release: 0.1, cutoff: 70
      sleep 0.125
    end
    
Maintenant pendant que ça joue commencez à modifier le code. Changez
les types de bloc : essayez `:water`, `:grass` ou votre type de bloc
préféré. Aussi, essayez de changer la valeur de coupure de `70` à `80`
puis jusque `100`. N'est-ce pas amusant ?

## Rassemblons tout

![Screen 2](../images/tutorial/articles/A.06-minecraft/Musical-Minecraft-2-small.png)

Rassemblons tout ce que nous avons vu jusqu'ici avec un peu de magie
en plus. Combinons notre faculté de téléportation avec placement de
blocs et la musique pour faire une vidéo de musique Minecraft. Ne vous
inquiétez pas si vous ne comprenez pas tout, tapez juste le code et
jouez ensuite à modifier quelques-unes des valeurs pendant qu'il
s'exécute. Amusez-vous bien et à la prochaine...
    

    live_loop :note_blocks do
      mc_message "This is Sonic Minecraft"
      with_fx :reverb do
        with_fx :echo, phase: 0.125, reps: 32 do
          tick
          x = (range 30, 90, step: 0.1).look
          y = 20
          z = -10
          mc_teleport x, y, z
          ns = (scale :e3, :minor_pentatonic)
          n = ns.shuffle.choose
          bs = (knit :glass, 3, :sand, 1)
          b = bs.look
          synth :beep, note: n, release: 0.1
          mc_set_block b, x+20, n-60+y, z+10
          mc_set_block b, x+20, n-60+y, z-10
          sleep 0.25
        end
      end
    end
    
    live_loop :beats do
      sample :bd_haus, cutoff: 100
      sleep 0.5
    end
          




