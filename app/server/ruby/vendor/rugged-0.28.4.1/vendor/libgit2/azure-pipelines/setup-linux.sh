#!/bin/sh

set -e
set -x

TMPDIR=${TMPDIR:-/tmp}

if [ -z "$SKIP_APT" ]; then
	apt-get update
	apt-get -y install build-essential pkg-config clang cmake openssl libssl-dev libssh2-1-dev libcurl4-gnutls-dev openssh-server
fi

mkdir -p /var/run/sshd

if [ "$MBEDTLS" ]; then
	MBEDTLS_DIR=${MBEDTLS_DIR:-$(mktemp -d ${TMPDIR}/mbedtls.XXXXXXXX)}

	git clone --depth 10 --single-branch --branch mbedtls-2.6.1 https://github.com/ARMmbed/mbedtls.git ${MBEDTLS_DIR}
	cd ${MBEDTLS_DIR}

	CFLAGS=-fPIC cmake -DENABLE_PROGRAMS=OFF -DENABLE_TESTING=OFF -DUSE_SHARED_MBEDTLS_LIBRARY=OFF -DUSE_STATIC_MBEDTLS_LIBRARY=ON .
	cmake --build .

	if [ -z "$SKIP_MBEDTLS_INSTALL" ]; then
		make install
	fi
fi
