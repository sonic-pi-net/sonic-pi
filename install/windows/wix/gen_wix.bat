cd %~dp0
cd ..

@echo "Generating gui, server, etc wix files"
heat dir etc -gg -g1 -f -sfrag -dr SonicPi -cg ETC -out etc.wxs
heat dir app\gui -gg -g1 -v -t "wix\filter.xslt" -sreg -sfrag -dr AppDir -cg GUI -out gui.wxs
heat dir app\server -gg -g1 -sreg -sfrag -dr AppDir -cg SERVER -out server.wxs

