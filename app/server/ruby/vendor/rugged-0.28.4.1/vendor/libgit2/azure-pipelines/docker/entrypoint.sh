#!/bin/bash
useradd --shell /bin/bash --create-home libgit2
chown -R $(id -u libgit2) /home/libgit2
exec gosu libgit2 "$@"
