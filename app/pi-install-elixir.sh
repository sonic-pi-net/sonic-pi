#!/bin/bash

if [ ! -d ~/.asdf ]; then
  echo "Installing asdf"

  git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.1

  echo ". $HOME/.asdf/asdf.sh" >> ~/.bashrc
  echo ". $HOME/.asdf/completions/asdf.bash" >> ~/.bashrc
fi

source ~/.asdf/asdf.sh
asdf update
asdf plugin-update --all

echo "Installing Erlang via asdf"
asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang.git
asdf install erlang latest
asdf global erlang latest

echo "Installing Elixir via asdf"
asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git
asdf install elixir latest
asdf global elixir latest
