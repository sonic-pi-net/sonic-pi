#!/bin/sh
#
# Start sonic-pi up in the container
#

# first the jackd server
jackd -r -P 95 -d alsa -n 2 -r 48000 &

# then sonic-pi
sonic-pi
