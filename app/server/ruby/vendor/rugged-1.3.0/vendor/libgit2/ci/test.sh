#!/usr/bin/env bash

set -e

if [ -n "$SKIP_TESTS" ]; then
	exit 0
fi

# Windows doesn't run the NTLM tests properly (yet)
if [[ "$(uname -s)" == MINGW* ]]; then
        SKIP_NTLM_TESTS=1
fi

SOURCE_DIR=${SOURCE_DIR:-$( cd "$( dirname "${BASH_SOURCE[0]}" )" && dirname $( pwd ) )}
BUILD_DIR=$(pwd)
TMPDIR=${TMPDIR:-/tmp}
USER=${USER:-$(whoami)}

SUCCESS=1
CONTINUE_ON_FAILURE=0

cleanup() {
	echo "Cleaning up..."

	if [ ! -z "$GITDAEMON_PID" ]; then
		echo "Stopping git daemon..."
		kill $GITDAEMON_PID
	fi

	if [ ! -z "$SSHD_DIR" -a -f "${SSHD_DIR}/pid" ]; then
		echo "Stopping SSH..."
		kill $(cat "${SSHD_DIR}/pid")
	fi

	echo "Done."
}

run_test() {
	if [[ "$GITTEST_FLAKY_RETRY" > 0 ]]; then
		ATTEMPTS_REMAIN=$GITTEST_FLAKY_RETRY
	else
		ATTEMPTS_REMAIN=1
	fi

	FAILED=0
	while [[ "$ATTEMPTS_REMAIN" > 0 ]]; do
		if [ "$FAILED" -eq 1 ]; then
			echo ""
			echo "Re-running flaky ${1} tests..."
			echo ""
		fi

		RETURN_CODE=0

		CLAR_SUMMARY="${BUILD_DIR}/results_${1}.xml" ctest -V -R "^${1}$" || RETURN_CODE=$? && true

		if [ "$RETURN_CODE" -eq 0 ]; then
			FAILED=0
			break
		fi

		echo "Test exited with code: $RETURN_CODE"
		ATTEMPTS_REMAIN="$(($ATTEMPTS_REMAIN-1))"
		FAILED=1
	done

	if [ "$FAILED" -ne 0 ]; then
		if [ "$CONTINUE_ON_FAILURE" -ne 1 ]; then
			exit 1
		fi

		SUCCESS=0
	fi
}

# Configure the test environment; run them early so that we're certain
# that they're started by the time we need them.

echo "##############################################################################"
echo "## Configuring test environment"
echo "##############################################################################"

if [ -z "$SKIP_GITDAEMON_TESTS" ]; then
	echo "Starting git daemon..."
	GITDAEMON_DIR=`mktemp -d ${TMPDIR}/gitdaemon.XXXXXXXX`
	git init --bare "${GITDAEMON_DIR}/test.git"
	git daemon --listen=localhost --export-all --enable=receive-pack --base-path="${GITDAEMON_DIR}" "${GITDAEMON_DIR}" 2>/dev/null &
	GITDAEMON_PID=$!
	disown $GITDAEMON_PID
fi

if [ -z "$SKIP_PROXY_TESTS" ]; then
	curl --location --silent --show-error https://github.com/ethomson/poxyproxy/releases/download/v0.7.0/poxyproxy-0.7.0.jar >poxyproxy.jar

	echo ""
	echo "Starting HTTP proxy (Basic)..."
	java -jar poxyproxy.jar --address 127.0.0.1 --port 8080 --credentials foo:bar --auth-type basic --quiet &

	echo ""
	echo "Starting HTTP proxy (NTLM)..."
	java -jar poxyproxy.jar --address 127.0.0.1 --port 8090 --credentials foo:bar --auth-type ntlm --quiet &
fi

if [ -z "$SKIP_NTLM_TESTS" ]; then
	curl --location --silent --show-error https://github.com/ethomson/poxygit/releases/download/v0.4.0/poxygit-0.4.0.jar >poxygit.jar

	echo ""
	echo "Starting HTTP server..."
	NTLM_DIR=`mktemp -d ${TMPDIR}/ntlm.XXXXXXXX`
	git init --bare "${NTLM_DIR}/test.git"
	java -jar poxygit.jar --address 127.0.0.1 --port 9000 --credentials foo:baz --quiet "${NTLM_DIR}" &
fi

if [ -z "$SKIP_SSH_TESTS" ]; then
	echo "Starting ssh daemon..."
	HOME=`mktemp -d ${TMPDIR}/home.XXXXXXXX`
	SSHD_DIR=`mktemp -d ${TMPDIR}/sshd.XXXXXXXX`
	git init --bare "${SSHD_DIR}/test.git"
	cat >"${SSHD_DIR}/sshd_config" <<-EOF
	Port 2222
	ListenAddress 0.0.0.0
	Protocol 2
	HostKey ${SSHD_DIR}/id_rsa
	PidFile ${SSHD_DIR}/pid
	AuthorizedKeysFile ${HOME}/.ssh/authorized_keys
	LogLevel DEBUG
	RSAAuthentication yes
	PasswordAuthentication yes
	PubkeyAuthentication yes
	ChallengeResponseAuthentication no
	StrictModes no
	# Required here as sshd will simply close connection otherwise
	UsePAM no
	EOF
	ssh-keygen -t rsa -f "${SSHD_DIR}/id_rsa" -N "" -q
	/usr/sbin/sshd -f "${SSHD_DIR}/sshd_config" -E "${SSHD_DIR}/log"

	# Set up keys
	mkdir "${HOME}/.ssh"
	ssh-keygen -t rsa -f "${HOME}/.ssh/id_rsa" -N "" -q
	cat "${HOME}/.ssh/id_rsa.pub" >>"${HOME}/.ssh/authorized_keys"
	while read algorithm key comment; do
		echo "[localhost]:2222 $algorithm $key" >>"${HOME}/.ssh/known_hosts"
	done <"${SSHD_DIR}/id_rsa.pub"

	# Get the fingerprint for localhost and remove the colons so we can
	# parse it as a hex number. Older versions have a different output
	# format.
	if [[ $(ssh -V 2>&1) == OpenSSH_6* ]]; then
		SSH_FINGERPRINT=$(ssh-keygen -F '[localhost]:2222' -f "${HOME}/.ssh/known_hosts" -l | tail -n 1 | cut -d ' ' -f 2 | tr -d ':')
	else
		SSH_FINGERPRINT=$(ssh-keygen -E md5 -F '[localhost]:2222' -f "${HOME}/.ssh/known_hosts" -l | tail -n 1 | cut -d ' ' -f 3 | cut -d : -f2- | tr -d :)
	fi
fi

# Run the tests that do not require network connectivity.

if [ -z "$SKIP_OFFLINE_TESTS" ]; then
	echo ""
	echo "##############################################################################"
	echo "## Running (offline) tests"
	echo "##############################################################################"

	run_test offline
fi

if [ -n "$RUN_INVASIVE_TESTS" ]; then
	echo ""
	echo "Running invasive tests"
	echo ""

	export GITTEST_INVASIVE_FS_SIZE=1
	export GITTEST_INVASIVE_MEMORY=1
	export GITTEST_INVASIVE_SPEED=1
	run_test invasive
	unset GITTEST_INVASIVE_FS_SIZE
	unset GITTEST_INVASIVE_MEMORY
	unset GITTEST_INVASIVE_SPEED
fi

if [ -z "$SKIP_ONLINE_TESTS" ]; then
	# Run the online tests.  The "online" test suite only includes the
	# default online tests that do not require additional configuration.
	# The "proxy" and "ssh" test suites require further setup.

	echo ""
	echo "##############################################################################"
	echo "## Running (online) tests"
	echo "##############################################################################"

	export GITTEST_FLAKY_RETRY=5
	run_test online
	unset GITTEST_FLAKY_RETRY

	# Run the online tests that immutably change global state separately
	# to avoid polluting the test environment.
	echo ""
	echo "##############################################################################"
	echo "## Running (online_customcert) tests"
	echo "##############################################################################"
	run_test online_customcert
fi

if [ -z "$SKIP_GITDAEMON_TESTS" ]; then
	echo ""
	echo "Running gitdaemon tests"
	echo ""

	export GITTEST_REMOTE_URL="git://localhost/test.git"
	run_test gitdaemon
	unset GITTEST_REMOTE_URL
fi

if [ -z "$SKIP_PROXY_TESTS" ]; then
	echo ""
	echo "Running proxy tests (Basic authentication)"
	echo ""

	export GITTEST_REMOTE_PROXY_HOST="localhost:8080"
	export GITTEST_REMOTE_PROXY_USER="foo"
	export GITTEST_REMOTE_PROXY_PASS="bar"
	run_test proxy
	unset GITTEST_REMOTE_PROXY_HOST
	unset GITTEST_REMOTE_PROXY_USER
	unset GITTEST_REMOTE_PROXY_PASS

	echo ""
	echo "Running proxy tests (NTLM authentication)"
	echo ""

	export GITTEST_REMOTE_PROXY_HOST="localhost:8090"
	export GITTEST_REMOTE_PROXY_USER="foo"
	export GITTEST_REMOTE_PROXY_PASS="bar"
	export GITTEST_FLAKY_RETRY=5
	run_test proxy
	unset GITTEST_FLAKY_RETRY
	unset GITTEST_REMOTE_PROXY_HOST
	unset GITTEST_REMOTE_PROXY_USER
	unset GITTEST_REMOTE_PROXY_PASS
fi

if [ -z "$SKIP_NTLM_TESTS" ]; then
	echo ""
	echo "Running NTLM tests (IIS emulation)"
	echo ""

	export GITTEST_REMOTE_URL="http://localhost:9000/ntlm/test.git"
	export GITTEST_REMOTE_USER="foo"
	export GITTEST_REMOTE_PASS="baz"
	run_test auth_clone_and_push
	unset GITTEST_REMOTE_URL
	unset GITTEST_REMOTE_USER
	unset GITTEST_REMOTE_PASS

	echo ""
	echo "Running NTLM tests (Apache emulation)"
	echo ""

	export GITTEST_REMOTE_URL="http://localhost:9000/broken-ntlm/test.git"
	export GITTEST_REMOTE_USER="foo"
	export GITTEST_REMOTE_PASS="baz"
	run_test auth_clone_and_push
	unset GITTEST_REMOTE_URL
	unset GITTEST_REMOTE_USER
	unset GITTEST_REMOTE_PASS
fi

if [ -z "$SKIP_NEGOTIATE_TESTS" -a -n "$GITTEST_NEGOTIATE_PASSWORD" ]; then
	echo ""
	echo "Running SPNEGO tests"
	echo ""

	if [ "$(uname -s)" = "Darwin" ]; then
		KINIT_FLAGS="--password-file=STDIN"
	fi

	echo $GITTEST_NEGOTIATE_PASSWORD | kinit $KINIT_FLAGS test@LIBGIT2.ORG
	klist -5f

	export GITTEST_REMOTE_URL="https://test.libgit2.org/kerberos/empty.git"
	export GITTEST_REMOTE_DEFAULT="true"
	run_test auth_clone
	unset GITTEST_REMOTE_URL
	unset GITTEST_REMOTE_DEFAULT

	echo ""
	echo "Running SPNEGO tests (expect/continue)"
	echo ""

	export GITTEST_REMOTE_URL="https://test.libgit2.org/kerberos/empty.git"
	export GITTEST_REMOTE_DEFAULT="true"
	export GITTEST_REMOTE_EXPECTCONTINUE="true"
	run_test auth_clone
	unset GITTEST_REMOTE_URL
	unset GITTEST_REMOTE_DEFAULT
	unset GITTEST_REMOTE_EXPECTCONTINUE

	kdestroy -A
fi

if [ -z "$SKIP_SSH_TESTS" ]; then
	echo ""
	echo "Running ssh tests"
	echo ""

	export GITTEST_REMOTE_URL="ssh://localhost:2222/$SSHD_DIR/test.git"
	export GITTEST_REMOTE_USER=$USER
	export GITTEST_REMOTE_SSH_KEY="${HOME}/.ssh/id_rsa"
	export GITTEST_REMOTE_SSH_PUBKEY="${HOME}/.ssh/id_rsa.pub"
	export GITTEST_REMOTE_SSH_PASSPHRASE=""
	export GITTEST_REMOTE_SSH_FINGERPRINT="${SSH_FINGERPRINT}"
	run_test ssh
	unset GITTEST_REMOTE_URL
	unset GITTEST_REMOTE_USER
	unset GITTEST_REMOTE_SSH_KEY
	unset GITTEST_REMOTE_SSH_PUBKEY
	unset GITTEST_REMOTE_SSH_PASSPHRASE
	unset GITTEST_REMOTE_SSH_FINGERPRINT
fi

if [ -z "$SKIP_FUZZERS" ]; then
	echo ""
	echo "##############################################################################"
	echo "## Running fuzzers"
	echo "##############################################################################"

	ctest -V -R 'fuzzer'
fi

cleanup

if [ "$SUCCESS" -ne 1 ]; then
	echo "Some tests failed."
	exit 1
fi

echo "Success."
exit 0
