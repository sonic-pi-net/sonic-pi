11 Minecraft Pi

# Minecraft Pi

Sonic Pi soporta una simple API para interactuar con Minecraft Pi -
la edición especial de Minecraft instalada en el sistema operativo
basado en Linux que trae la Raspberry Pi, elRaspbian.

## Sin necesidad de importar librerías

La integración de Minecraft Pi  está diseñada para ser extremadamente
fácil de usar. Sólo necesitas lanzar Minecraft Pi y crear un mundo. De
ahí tienes libertad de usar `mc_*` fns tanto como utilizas`play` y `synth`. 
No hay necesidad de importar algo o instalar librerías. Todo está listo
para funcionar.

## Conexión automática

El API de Minecraft Pi se encarga de manejar tu conexión a la aplicación
Minecraft Pi. Esto significa que no tienes algo de lo que preocuparte.
Si intentas utilizar el API de Minecraft Pi cuando Minecraft Pi no está 
en funcionamiento, Sonic Pi te lo dirá. Similarmente, si cierras Minecraft Pi
mientras se ejecuta un `live_loop` que utiliza la API, el bucle se detendrá
y te dirá que no se puede conectar. Para reconectar, simplemente lanza Minecraft Pi
de nuevo y Sonic Pi detectará automáticamente  y recreará la conexión para tí.

## Diseñado para ser Live Coded

La API de Minecraft fue diseñada para trabajar dentro de `live_loop`s. Esto significa
que es posible sincronizar modificaciones en tus mundos de Minecraft Pi con modificaciones
en tu Sonic Pi. Videos instantáneos de música basada en Minecraft! Nota que Minecraft Pi
es software alfa y conocido por algunos problemillas. Si encuentras algunos problemas, 
reinicia Minecraft Pi y continúa como anteriormente. La funcionalidad de conexión automática
de Sonic Pi se encargará por tí.

## Requiere una Raspberry Pi 2.0

Es recomendable usar una Raspberry Pi 2 si pretendes correr al mismo tiempo Sonic Pi y
Minecraft, especialmente si deseas utilizar las capacidades de Sonic Pi.

## Soporte API

Por ahora, Sonic Pi soporta las manipulaciones básicas de bloque y ejcutante, que fueron
detalladas en la sección 11.1. En futuras versiones se pretende soportar llamadas de eventos
disparadas por interacciones de ejecutantes en el mundo.
