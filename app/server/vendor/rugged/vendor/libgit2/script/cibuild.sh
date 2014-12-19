#!/bin/sh

if [ -n "$COVERITY" ];
then
	./script/coverity.sh;
	exit $?;
fi

# Create a test repo which we can use for the online::push tests
mkdir "$HOME"/_temp
git init --bare "$HOME"/_temp/test.git
git daemon --listen=localhost --export-all --enable=receive-pack --base-path="$HOME"/_temp "$HOME"/_temp 2>/dev/null &
export GITTEST_REMOTE_URL="git://localhost/test.git"

mkdir _build
cd _build
# shellcheck disable=SC2086
cmake .. -DCMAKE_INSTALL_PREFIX=../_install $OPTIONS || exit $?
make -j2 install || exit $?
ctest -V . || exit $?

# Now that we've tested the raw git protocol, let's set up ssh to we
# can do the push tests over it

killall git-daemon

if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    echo 'PasswordAuthentication yes' | sudo tee -a /etc/sshd_config
else
    sudo start ssh
fi

ssh-keygen -t rsa -f ~/.ssh/id_rsa -N "" -q
cat ~/.ssh/id_rsa.pub >>~/.ssh/authorized_keys
ssh-keyscan -t rsa localhost >>~/.ssh/known_hosts

# Get the fingerprint for localhost and remove the colons so we can parse it as a hex number
export GITTEST_REMOTE_SSH_FINGERPRINT=$(ssh-keygen -F localhost -l | tail -n 1 | cut -d ' ' -f 2 | tr -d ':')

export GITTEST_REMOTE_URL="ssh://localhost/$HOME/_temp/test.git"
export GITTEST_REMOTE_USER=$USER
export GITTEST_REMOTE_SSH_KEY="$HOME/.ssh/id_rsa"
export GITTEST_REMOTE_SSH_PUBKEY="$HOME/.ssh/id_rsa.pub"
export GITTEST_REMOTE_SSH_PASSPHRASE=""

if [ -e ./libgit2_clar ]; then
    ./libgit2_clar -sonline::push -sonline::clone::ssh_cert &&
    ./libgit2_clar -sonline::clone::ssh_with_paths
    if [ "$TRAVIS_OS_NAME" = "linux" ]; then
        ./libgit2_clar -sonline::clone::cred_callback
    fi
fi
