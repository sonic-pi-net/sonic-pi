#!/bin/sh

if [ -n "$COVERITY" ];
then
	./script/coverity.sh;
	exit $?;
fi

# Create a test repo which we can use for the online::push tests
mkdir $HOME/_temp
git init --bare $HOME/_temp/test.git
git daemon --listen=localhost --export-all --enable=receive-pack --base-path=$HOME/_temp $HOME/_temp 2>/dev/null &
export GITTEST_REMOTE_URL="git://localhost/test.git"

mkdir _build
cd _build
cmake .. -DCMAKE_INSTALL_PREFIX=../_install $OPTIONS || exit $?
cmake --build . --target install || exit $?
ctest -V . || exit $?

# Now that we've tested the raw git protocol, let's set up ssh to we
# can do the push tests over it

killall git-daemon
sudo start ssh
ssh-keygen -t rsa -f ~/.ssh/id_rsa -N "" -q
cat ~/.ssh/id_rsa.pub >>~/.ssh/authorized_keys
ssh-keyscan -t rsa localhost >>~/.ssh/known_hosts

export GITTEST_REMOTE_URL="ssh://localhost/$HOME/_temp/test.git"
export GITTEST_REMOTE_USER=$USER
export GITTEST_REMOTE_SSH_KEY="$HOME/.ssh/id_rsa"
export GITTEST_REMOTE_SSH_PUBKEY="$HOME/.ssh/id_rsa.pub"
export GITTEST_REMOTE_SSH_PASSPHRASE=""

if [ -e ./libgit2_clar ]; then
    ./libgit2_clar -sonline::push -sonline::clone::cred_callback_failure
fi
