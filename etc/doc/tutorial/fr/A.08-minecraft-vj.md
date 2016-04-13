A.8 Devenez un VJ Minecraft

# Devenez un VJ Minecraft

![Screen 0](../images/tutorial/articles/A.08-minecraft-vj/minecraft-vj-0-small.png)

Tout le monde a joué à Minecraft. Vous aurez tous construit des
structures incroyables, conçu des pièges rusés et même créé des lignes
de chariots raffinées contrôlées par des interrupteurs de pierre
rouge. Mais qui parmi vous s'est produit avec Minecraft ? On parie que
vous ne saviez pas que vous pouvez utiliser Minecraft pour créer des
animations visuelles incroyables tout comme un VJ professionnel.

Si votre seule possibilité de modifier Minecraft était d'utiliser la
souris, vous auriez du mal à changer les choses suffisamment vite.
Heureusement pour vous votre Raspberry Pi a une version de Minecraft
qui peut être contrôlée avec du code. Il contient aussi une application
nommée Sonic Pi qui rend la programmation Minecraft non seulement facile
mais aussi incroyablement amusante.

Dans l'article d'aujourd'hui nous allons vous montrer quelques trucs
et astuces que nous avons utilisés pour créer des spectacles dans des
boîtes de nuit et salles de concert autour du monde.

Commençons...

# Commencer

Commençons avec un simple exercice d'échauffement pour nous rappeler
les bases. Commencez par allumer votre Raspberry Pi et lancer
Minecraft et Sonic Pi. Dans Minecraft, créez un nouveau monde, et dans
Sonic Pi choisissez un buffer de libre et écrivez-y ce code :

    mc_message "Let's get started..."
    
Cliquez sur le bouton 'Run' et vous verrez le message dans la fenêtre
Minecraft. OK, on est prêt à démarrer, on va s'amuser...

# Tempêtes de sable

Quand on utilise Minecraft pour créer des visuels on essaie de penser
à des choses qui auront l'air intéressantes et qui seront aussi
faciles à générer avec du code. Un truc sympa est de créer une tempête
de sable en laissant tomber des bloc de sable du ciel. Pour cela on a
juste besoin de quelques fonctions simples :

* `sleep` : pour insérer un délai entre des actions
* `mc_location` : pour trouver notre emplacement courant
* `mc_set_block` : pour positionner des blocs de sable à un endroit particulier
* `rrand` : pour nous permetre de générer des valeurs aléatoires dans un intervalle
* `live_loop` : pour nous permettre de faire pleuvoir du sable en continu

<!-- Breakout box start --> 

Si vous ne connaissez pas une des ces fonctions comme `rrand`, vous
pouvez juste taper le mot dans votre buffer, cliquer dessus, puis
taper `Control-i` sur le clavier pour ouvrir la documentation. Vous
pouvez aussi aller dans l'onglet *lang* du système d'aide puis
y chercher directement les fonctions et toutes sortes d'autres choses
excitantes que vous pouvez faire.


<!-- Breakout box end -->

Commençons par faire tomber un peu de pluie avant de laisser la
tempête complète éclater. Récupérez votre emplacement courant et
utilisez-la pour créer quelques blocs de sable dans le ciel pas loin :

    x, y, z = mc_location
    mc_set_block :sand, x, y + 20, z + 5
    sleep 2
    mc_set_block :sand, x, y + 20, z + 6
    sleep 2
    mc_set_block :sand, x, y + 20, z + 7
    sleep 2
    mc_set_block :sand, x, y + 20, z + 8
    
Après avoir cliqué sur 'Run', vous devrez peut-être un peu regarder
autour de vous car les blocs peuvent commencer par tomber derrière
vous, suivant dans quelle direction vous êtes pour le moment. N'ayez
pas peur, si vous les avez raté, cliquez à nouveau sur 'Run' pour
créer encore un peu de pluie de sable, assurez-vous juste de regarder
dans la bonne direction.

Parcourons rapidement ce qui se passe ici. Sur la première ligne nous
avons récupéré l'emplacement Steve en coordonnées avec la fonction
`mc_location` et les avons placées dans les variables `x`, `y` et `z`.
Puis sur les lignes suivantes nous avons utilisé la fonction
`mc_set_block` pour placer un peu de sable aux mêmes coordonnées que
Steve mais avec quelques modifications. On a utilisé la même
coordonnée x, une coordonnée y 20 blocs plus haut et des coordonnées z
successivement plus larges pour que le sable tombe dans une ligne en
s'éloignant de Steve.

Et si vous preniez ce code et commenciez à jouer avec ? Essayez
d'ajouter plus de lignes, de changer la durée d'attente, essayez de
mélanger du `:sand` avec du `:gravel` et choisissez différentes
coordonnées. Expérimentez et amusez-vous !

# Boucles interactives déchaînées

OK, il est l'heure de lancer la tempête en déchaînant la puissance
complète de la `live_loop` : la capacité magique de Sonic Pi qui
montre la puissance entière de la programmation interactive : changer
le code à la volée pendant qu'il est en train de s'exécuter !

    live_loop :sand_storm do
      x, y, z = mc_location
      xd = rrand(-10, 10)
      zd = rrand(-10, 10)
      co = rrand(70, 130)
      synth :cnoise, attack: 0, release: 0.125, cutoff: co
      mc_set_block :sand, x + xd, y+20, z+zd
      sleep 0.125
    end
    
Qu'est-ce que c'est amusant ! On boucle assez vite (8 fois par
seconde) et pendant chaque boucle on trouve l'emplacement de Steve
comme avant mais on génère ensuite trois valeurs aléatoires :

* `xd` : la différence pour x qui sera entre -10 et 10
* `zd` : la différence pour z, aussi entre -10 et 10
* `co` : une valeur de coupure pour le filtre passe-bas, entre 70 et 130

On utilise ensuite ces valeurs aléatoires dans les fonctions `synth`
et `mc_set_block` ce qui nous donne du sable qui tombe dans des
endroits aléatoires autour de Steve ainsi qu'un son percussif
semblable à de la pluie, joué le synthé `:cnoise`.

Pour ceux d'entre vous qui ne connaissaient pas les boucles
interactives : c'est là qu'on commence vraiment à s'amuser avec Sonic
Pi. Pendant que le code tourne et que le sable pleut, essayez de
changer une des valeurs, peut-être la valeur d'attente à `0.25` ou
le type de bloc `:sand` en `:gravel`. Ensuite cliquez sur 'Run' à
nouveau. Et voilà ! Les choses changent sans que le code ne s'arrête.
C'est votre passerelle pour vous produire comme un vrai VJ. Continuez
à vous exercer à changer des choses. Jusqu'où arrivez-vous à modifier
les visuels sans arrêter le code ?


# Des formes de blocs épiques

![Screen 1](../images/tutorial/articles/A.08-minecraft-vj/minecraft-vj-1-small.png)

Enfin une autre super façon de générer des visuels intéressants est de
générer des énormes murs de motifs vers lesquels voler. Pour cet effet
nous allons devoir passer d'un placement aléatoire de blocs à un
placement de manière ordonnée. On peut faire cela en imbriquant deux
itérations (cliquez sur le bouton 'Aide' et allez dans la section 5.2
du tutoriel "Itération et boucles" pour plus d'informations sur
l'itération). L'étrange `|xd|` après le `do` veut dire que `xd`
prendra une valeur à chaque boucle de l'itération. La première fois il
vaudra `0`, puis `1`, puis `2`, etc. En imbriquant deux itérations
comme cela on peut générer toutes les coordonnées d'un carré. On peut
ensuite choisir aléatoirement des types de blocs d'un anneau de blocs
pour obtenir un effet intéressant :

    x, y, z = mc_location
    bs = (ring :gold, :diamond, :glass)
    10.times do |xd|
      10.times do |yd|
        mc_set_block bs.choose, x + xd, y + yd, z
      end
    end

Pas mal. Pendant qu'on s'amuse ici, essayez de changer `bs.choose` en
`bs.tick` pour passer d'un motif aléatoire à un motif plus régulier.
Essayez de changer les types de blocs et les plus aventureux parmi
vous voudrons peut-être mettre ce code dans une `live_loop` pour que
les modifs continuent à changer automatiquement.

Enfin, pour la fin du set du VJ, changez les deux `10.times` en
`100.times` et cliquez sur 'Run'. Boum ! Un énorme mur géant de
briques aléatoires. Imaginez combien de temps ça vous aurait pris de
construire ça avec votre souris ! Double-tapez la touche Espace pour
entrer en mode volant et commencez à planer pour obtenir de super
effets visuels. Ne vous arrêtez pas là, utilisez votre imagination 
pour trouver des idées sympa et utilisez ensuite la puissance de
programmation de Sonic Pi pour le réaliser. Quand vous vous serez
suffisamment exercés, baissez la lumière et donnez un spectacle de VJ
pour vos amis !
