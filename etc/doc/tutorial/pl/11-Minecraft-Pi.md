11 Minecraft Pi

# Minecraft Pi

Sonic Pi wspiera proste API umożliwia interakcję z Minecraft Pi - 
specjalną edycją gry Minecraft, która jest domyślnie preinstalowana 
w systemie Raspbian na Raspberry Pi (system operacyjny bazujący na 
Linuksie).

## Brak konieczności importowania bibliotek

Integracja z Minecraft Pi została zaprojektowana aby być szalenie 
łatwa w użyciu. Wszystko co potrzebujesz zrobić to włączyć Minecraft Pi
i stworzyć nowy świat. Gdy już to zrobić możesz dowolnie używać funkcji 
`mc_*` tak samo jak używałeś do tej pory poleceń `play` i `synth`. Nie 
ma konieczności importowania czegokolwiek lub instalacji dodatkowych 
bibliotek - wszystko jest gotowe i działa po wyjęciu z pudełka. 

## Automatyczne Połączenie

API serwowane przez Minecraft Pi zajmuje się zarządzaniem twoim połączeniem 
z aplikacją Minecraft Pi. Oznacza to, że nie musisz się przejmować o nic. 
Jeśli spróbujesz skorzystać z API oferowanego przez Minecraft Pi kiedy 
gra nie jest włączona, Sonic Pi uprzejmie Cię o tym poinformuje. Podobnie, 
jeśli zamkniesz Minecraft Pi w momencie kiedy wciąż masz uruchomioną 
żywą pętlę `live_loop`, która korzysta z API, żywa pętla zatrzyma sie 
i uprzejmie poinformuje Cię, że nie może się połączyć. Aby wznowić 
połączenie wystarczy, że ponownie włączysz Minecraft Pi a Sonic Pi 
automatycznie wykryje i ponownie utworzy połączenie za Ciebie. 

## Zaprojektowanie do Kodowania Na Żywo

API oferowane przez Minecraft Pi zostało zaprojektowane w taki sposób, 
aby pracować bez żadnych problemów o obrębie żywych pętli tworzonych 
z wykorzystaniem polecenia `live_loop`. Oznacza to, że jest możliw a
synchronizacja zmian w twoim świecie Minecraft Pi ze zmianami dokonywanymi 
w dźwiękach brzmiących w twoim Sonic Pi. Błyskawiczne nagrania wideo 
z muzyką bazujące na grze Minecraft! Jeśli jednak zdarzy się, że natrafisz 
na jakieś problemy, po prostu uruchom ponownie Minecraft Pi i kontynuuj 
wcześniejszą zabawę. Funkcjonalność automatycznego połączenia Sonic Pi 
zajmie się wszystkim za Ciebie. 

## Wymaga Raspberry Pi 2.0

Zalecane jest abyś używał Raspberry Pi 2 jeśli chcesz jednocześnie 
uruchomić Sonic Pi i Minecraft - zwłaszcza jeśli chcesz wykorzystać 
możliwości dźwiękowe Sonic Pi.

## Wspierane API

Na ten moment, Sonic Pi wspiera podstawowe bloki oraz manipulacje 
graczy, które sa opisane w rozdziale 11.1. Wsparcie dla zdarzeń zwrotnych 
wywoływanych przez interakcje gracza w świecie jest planowane na 
przyszłe wydanie. 
