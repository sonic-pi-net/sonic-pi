11 Minecraft Pi

# Minecraft Pi

Raspbian, das Linux-Betriebssystem für Raspberry Pi, bringt eine
spezielle Version von Minecraft als Teil der Vorinstallation mit.
Und mit Sonic Pi kannst Du über eine einfache API jetzt auch
Minecraft Pi steuern.

## Keine Bibliotheken notwendig

Die Verbindung zwischen Minecraft Pi und Sonic Pi ist so einfach zu
nutzen, dass Du nichts dafür vorbereiten musst. Starte einfach Minecraft
Pi und baue eine Welt. Du kannst sofort die `mc_*`-Funktionen in Deinem
Code genau wie `play` oder `synth` einsetzen. Es gibt nichts zu
installieren und keine Bibliotheken zu importieren - alles ist fix und
fertig und sofort startklar für Dich.

## Automatische Verbindung

Die Minecraft-Pi-API kümmert sich automatisch um die Verbindung zur
laufenden Minecraft-Pi-Applikation. Du musst also nichts dafür tun.
Sonic Pi wird sich allerdings freundlich beschweren, wenn Du die
API benutzen möchtest und Minecraft Pi nicht gleichzeitig läuft.
Und falls Du die Minecraft-Pi-Applikation beendest, während Dein Code
die API in einem `live_loop` ansprechen will, wird der Loop beendet und
Du erhältst eine entsprechende Fehlermeldung. Starte dann Minecraft Pi
wieder neu, die API wird sich von selbst damit verbinden und Dein Code
funktioniert wieder.

## Gemacht für Live-Coding

Die Minecraft-Pi-API ist dazu da, dass sie problemlos in in einem
`live_loop` funktioniert. Das heißt, Du kannst damit Veränderungen in
Deiner Minecraft-Welt mit Veränderungen in Deinen Sonic-Pi-Sounds
synchronisieren. Zack, Minecraft-basierte Musik-Videos! Bedenke aber,
dass Minecraft Pi eine Alpha-Version ist und die Software ein paar
kleinere Bugs hat. Wenn ein solches Problem mit Minecraft Pi auftritt,
starte es einfach neu. Sonic Pi nimmt dann automatisch eine neue
Verbindung über seine API auf.

## Raspberry Pi 2 empfohlen

Minecraft Pi und Sonic Pi mit seinen Sound-Möglichkeiten sind beides
sehr rechenintensive Programme. Der alte Raspberry ist davon schnell
überfordert und ein Raspberry Pi 2 mit seinem schnelleren Prozessor
deshalb sehr zu empfehlen!

## API-Support

Zur Zeit kann Sonic Pi einige Grundfunktionen zur Manipulation von
Minecraft-Blöcken und dem Spieler ausführen. Details dazu findest Du
im nächsten Abschnitt 11.1. Für ein späteres Release plane
ich, auch Event-Callbacks aus Minecraft Pi zu unterstützen, so dass der
Code in Sonic Pi von Ereignissen erfahren kann, die der Spieler in
Minecraft Pi ausgelöst hat.
