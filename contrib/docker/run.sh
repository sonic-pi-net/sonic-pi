#!/bin/sh
#
# Run sonic-pi in the docker container
pasuspender -- docker run -it --privileged -v /dev/snd:/dev/snd:rw -e DISPLAY=$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix debian:sonic-pi /home/sonic/start-sonic.sh
