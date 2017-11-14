#!/bin/bash

THIS_FILE="$(readlink -f "$0")"
ROOT="$(dirname "$(dirname "$(dirname "$THIS_FILE")")")"
PROGRAM="$ROOT"/examples/rev-list
LIBDIR="$ROOT"/build
REPO="$ROOT"/tests/resources/testrepo.git

cd "$REPO"

run () {
    LD_LIBRARY_PATH="$LIBDIR" "$PROGRAM" "$@"
}

diff -u - <(run --date-order a4a7dce) <<EOF
a4a7dce85cf63874e984719f4fdd239f5145052f
c47800c7266a2be04c571c04d5a6614691ea99bd
9fd738e8f7967c078dceed8190330fc8648ee56a
4a202b346bb0fb0db7eff3cffeb3c70babbd2045
5b5b025afb0b4c913b4c338a42934a3863bf3644
8496071c1b46c854b31185ea97743be6a8774479
EOF

out="$(run --topo-order a4a7dce)"
diff -q - <(echo -n "$out") <<EOF >/dev/null ||
a4a7dce85cf63874e984719f4fdd239f5145052f
c47800c7266a2be04c571c04d5a6614691ea99bd
9fd738e8f7967c078dceed8190330fc8648ee56a
4a202b346bb0fb0db7eff3cffeb3c70babbd2045
5b5b025afb0b4c913b4c338a42934a3863bf3644
8496071c1b46c854b31185ea97743be6a8774479
EOF
diff -u - <(echo "$out") <<EOF
a4a7dce85cf63874e984719f4fdd239f5145052f
9fd738e8f7967c078dceed8190330fc8648ee56a
4a202b346bb0fb0db7eff3cffeb3c70babbd2045
c47800c7266a2be04c571c04d5a6614691ea99bd
5b5b025afb0b4c913b4c338a42934a3863bf3644
8496071c1b46c854b31185ea97743be6a8774479
EOF

diff -u - <(run --date-order --reverse a4a7dce) <<EOF
8496071c1b46c854b31185ea97743be6a8774479
5b5b025afb0b4c913b4c338a42934a3863bf3644
4a202b346bb0fb0db7eff3cffeb3c70babbd2045
9fd738e8f7967c078dceed8190330fc8648ee56a
c47800c7266a2be04c571c04d5a6614691ea99bd
a4a7dce85cf63874e984719f4fdd239f5145052f
EOF

out=$(run --topo-order --reverse a4a7dce)
diff -q - <(echo -n "$out") <<EOF >/dev/null ||
8496071c1b46c854b31185ea97743be6a8774479
5b5b025afb0b4c913b4c338a42934a3863bf3644
4a202b346bb0fb0db7eff3cffeb3c70babbd2045
9fd738e8f7967c078dceed8190330fc8648ee56a
c47800c7266a2be04c571c04d5a6614691ea99bd
a4a7dce85cf63874e984719f4fdd239f5145052f
EOF
diff -u - <(echo "$out") <<EOF
8496071c1b46c854b31185ea97743be6a8774479
5b5b025afb0b4c913b4c338a42934a3863bf3644
c47800c7266a2be04c571c04d5a6614691ea99bd
4a202b346bb0fb0db7eff3cffeb3c70babbd2045
9fd738e8f7967c078dceed8190330fc8648ee56a
a4a7dce85cf63874e984719f4fdd239f5145052f
EOF

out="$(run --date-order --topo-order --reverse --reverse a4a7dce)"
diff -q - <(echo -n "$out") <<EOF >/dev/null ||
a4a7dce85cf63874e984719f4fdd239f5145052f
c47800c7266a2be04c571c04d5a6614691ea99bd
9fd738e8f7967c078dceed8190330fc8648ee56a
4a202b346bb0fb0db7eff3cffeb3c70babbd2045
5b5b025afb0b4c913b4c338a42934a3863bf3644
8496071c1b46c854b31185ea97743be6a8774479
EOF
diff -u - <(echo "$out") <<EOF
a4a7dce85cf63874e984719f4fdd239f5145052f
9fd738e8f7967c078dceed8190330fc8648ee56a
4a202b346bb0fb0db7eff3cffeb3c70babbd2045
c47800c7266a2be04c571c04d5a6614691ea99bd
5b5b025afb0b4c913b4c338a42934a3863bf3644
8496071c1b46c854b31185ea97743be6a8774479
EOF

diff -u - <(run ^9fd738e~2 9fd738e) <<EOF
9fd738e8f7967c078dceed8190330fc8648ee56a
4a202b346bb0fb0db7eff3cffeb3c70babbd2045
EOF

diff -u - <(run --not 9fd738e..9fd738e~2) <<EOF
9fd738e8f7967c078dceed8190330fc8648ee56a
4a202b346bb0fb0db7eff3cffeb3c70babbd2045
EOF
