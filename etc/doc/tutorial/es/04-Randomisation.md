4 Aleatoriedad

# Aleatoriedad

Una manera de añadir interés a tu música es usando números aleatorios.
Sonic Pi tiene gran funcionalidad añadiendo aleatoriedad a la música,
pero antes de comenzar, debemos aprender una verdad chocante: en Sonic
Pi *no existe aleatoriedad real*. ¿Qué significa eso?


## Repetibilidad


Una función realmente útil es `rrand` la cual te da un valor aleatorio 
entre dos números - un *min* y un *max*. (`rrand` es la abreviatura 
para ranged random). Intentemos tocar una nota al azar:

```
play rrand(50, 95)
```

¡Ooh, tocó una nota al azar! Tocó la nota `83.7527`. Una nota entre 50 y
95 Woah, espera, predije el número correcto que obtuviste? ¡Algo raro
sucede! Intenta con el código de nuevo. ¿qué? Otra vez `83.7527` ? 
¡Eso no es aleatorio!

La respuesta es que no es realmente aleatorio, es pseudo-aleatorio. 
Sonic Pi te dará números como al azar en una manera repetitiva, lo
cual es muy útil para asegurar que la música que creas en tú máquina
suene idéntica en cualquier otra máquina - aunque utilices números al
azar en tú composición.

Claro que en una pieza musical donde quieras colocar notas al "azar" 
y siempre eliga `83.7527` no sería ni aleatorio ni interesante. Sin
embargo, no es así. Intenta lo siguiente:


```
loop do
  play rrand(50, 95)
  sleep 0.5
end 
```

¡Sí! Finalmente suena aleatorio. Dentro de una misma *corrida* las
subsecuentes llamadas a funciones aleatorias dan valores..aleatorios.
Sin embargo, la próxima corrida volverá a dar la misma secuencia de
valores "aleatorios" y sonar exactamente igual. Es como si Sonic Pi
volvió en el tiempo al mismo punto cuando se corrió *run* la primera
vez. ¡Es el Groundhog Day de la síntesis musical!


## Haunted Bells

Una buena ilustración de aleatoriedad en acción es el ejemplo haunted
bells que buclee el sampleo de `:perc_bell` con velocidad de Aleatoriedad
y tiempo de pausa entre los sonidos:

```
loop do
  sample :perc_bell, rate: (rrand 0.125, 1.5)
  sleep rrand(0.2, 2)
end
```

## Corte aleatorio

Otro buen ejemplo de aleatoriedad es modificar el momento de corte 
de un sintetizador, en forma aleatoria. Un gran synth en el que 
hacerlo es el emulador `:tb303`

```
use_synth :tb303

loop do
  play 50, release: 0.1, cutoff: rrand(60, 120)
  sleep 0.125
end
```

## Semillas aleatorias

¿Entonces qué, no te gusta la particular secuencia de números al azar
que provee Sonic Pi? Bueno, es totalmente posible elegir un punto de 
comienzo via `use_random_seed`. Predeterminado está a 0, así que:
¡elige una semilla diferente para una aleatoriedad diferente!

Considera el siguiente:

```
5.times do
  play rrand(50, 100)
  sleep 0.5
end
```

Cada vez que corras ese código escucharás la misma secuencia de 5
notas. Para cambiar la secuencia, simplemente cambia el valor de 
seed_:

```
use_random_seed 40
5.times do
  play rrand(50, 100)
  sleep 0.5
end
```

Esto producirá una diferente secuencia de 5 notas. Al cambiar el valor
de seed y escuchar los resultados, puedes encontrar algo que te guste
y cuando lo compartas con otros, escucharán exactamente lo mismo.

Veamos otras útiles funciones de aleatoriedad.


## Elegir (choose)

Una cuestión común es elegir un ítem desde una lista de ítems conocidos
Por ejemplo, quiero tocar una nota de entre las siguientes: 60, 65 or 72.
Puedo lograr eso con `choose` pues me deja elegir un ítem de esa lista.
Primero necesito confeccionar mi listado entre paréntesis cuadrados y 
separándolos por comas: `[60, 65, 72]`. Después sólo necesito pasarlos
por `choose`:

```
choose([60, 65, 72])
```

Escuchemos a lo que suena:

```
loop do
  play choose([60, 65, 72])
  sleep 1
end
```

## Rango aleatorio (rrand)

Ya hemos visto la función `rrand`, pero adentrémonos más en ella.
Nos proporciona un número aleatorio entre dos valores. Eso significa
que nunca nos dará los números extremos del rango, sólo entre ellos.
Ese número será un 'flotante', lo que significa que no es un número
entero, sino una fracción de número. He aquí ejemplos de flotantes:

`rrand(20, 110)`:

* 87.5054931640625
* 86.05255126953125
* 61.77825927734375

## Rango aleatorio ínclusivo ( rrand_i)

Ocasionalmente querrás un número entero aleatorio, no un flotante.
Es entonces que `rrand_i` viene al rescate. Funcional similarmente
a `rrand` excepto que incluye los valores mínimo y máximo como 
potenciales aleatorios (lo que significa que es inclusiva y no 
excluyente de los valores extremos del rango). Ejemplos de números
con `rrand_i(20, 110)` son:

* 88
* 86
* 62

## rand

Esta función te regresará un número aleatorio flotante incluyendo
los valores 0 y el máximo especificado. Predeterminado está el 
rango entre 0 y 1. Por ello es útil  para elegir valores de `amp:`
aleatorios:

```
loop do
  play 60, amp: rand
  sleep 0.25
end
```

## rand_i


De maneara similar a `rrand_i` y `rrand`, `rand_i` nos dará un
número leatorio ENTERO entre 0 y el máximo especificado.


## Dado (dice)


Alguna vez querrás emular el tiro de un dado. Para ello tenemos un 
caso especial de `rrand_i` donde el valor inferior será siempre 1.
El uso de `dice` requiere que especifiques el número de lados del 
dado. Un dado estándard tiene 6 lados, así `dice(6)` actuará
similarmente, dando valores de1, 2, 3, 4, 5, or 6. Sin embargo,
como en los juegos de role-play encontrarás necesidad de un dado de
4, 12, 20 o inclusive de 120 caras..

## one_in

Finalmente cuando quieras emular tirar el máximo puntaje con un dado,
como 6 en un dado estándard `one_in` te dará eso con una posibilidad
de uno en número de lados del dado. Por lo tanto, `one_in(6)` nos dará
verdad con una probabilidad de 1 en 6 o falso. Verdadero y falso son
valores muy útiles para `if` el cual cubriremos en la siguiente sección
de este tutorial.

¡Ahora ve a jugar con códigos y algo de aleatoriedad!
