#!/bin/bash

sudo apt update
sudo apt install snapd
sudo snap install cmake --classic
sudo apt remove cmake
echo "Remember to reboot to get the new cmake!"
