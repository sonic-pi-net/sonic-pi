# variables that benchmark tests can set
#

set -eo pipefail

#
# command-line parsing
#

usage() { echo "usage: $(basename "$0") [--cli <path>] [--baseline-cli <path>] [--output-style <style>] [--json <path>]"; }

NEXT=
BASELINE_CLI=
TEST_CLI="git"
JSON=
SHOW_OUTPUT=

if [ "$CI" != "" ]; then
	OUTPUT_STYLE="color"
else
	OUTPUT_STYLE="auto"
fi

#
# parse the arguments to the outer script that's including us; these are arguments that
# the `benchmark.sh` passes (or that a user could specify when running an individual test)
#

for a in "$@"; do
	if [ "${NEXT}" = "cli" ]; then
		TEST_CLI="${a}"
		NEXT=
	elif [ "${NEXT}" = "baseline-cli" ]; then
		BASELINE_CLI="${a}"
		NEXT=
	elif [ "${NEXT}" = "output-style" ]; then
		OUTPUT_STYLE="${a}"
		NEXT=
	elif [ "${NEXT}" = "json" ]; then
		JSON="${a}"
		NEXT=
	elif [ "${a}" = "-c" ] || [ "${a}" = "--cli" ]; then
		NEXT="cli"
	elif [[ "${a}" == "-c"* ]]; then
		TEST_CLI="${a/-c/}"
	elif [ "${a}" = "-b" ] || [ "${a}" = "--baseline-cli" ]; then
		NEXT="baseline-cli"
	elif [[ "${a}" == "-b"* ]]; then
		BASELINE_CLI="${a/-b/}"
	elif [ "${a}" == "--output-style" ]; then
		NEXT="output-style"
	elif [ "${a}" = "-j" ] || [ "${a}" = "--json" ]; then
		NEXT="json"
	elif [[ "${a}" == "-j"* ]]; then
		JSON="${a}"
	elif [ "${a}" = "--show-output" ]; then
		SHOW_OUTPUT=1
		OUTPUT_STYLE=
	else
                echo "$(basename "$0"): unknown option: ${a}" 1>&2
		usage 1>&2
		exit 1
	fi
done

if [ "${NEXT}" != "" ]; then
	echo "$(basename "$0"): option requires a value: --${NEXT}" 1>&2
        usage 1>&2
        exit 1
fi

fullpath() {
	FULLPATH="${1}"
	if [[ "$(uname -s)" == "MINGW"* ]]; then FULLPATH="$(cygpath -u "${1}")"; fi

	if [[ "${FULLPATH}" != *"/"* ]]; then
                FULLPATH="$(which "${FULLPATH}")"
                if [ "$?" != "0" ]; then exit 1; fi
	else
		FULLPATH="$(cd "$(dirname "${FULLPATH}")" && pwd)/$(basename "${FULLPATH}")"
	fi

	if [[ "$(uname -s)" == "MINGW"* ]]; then FULLPATH="$(cygpath -w "${FULLPATH}")"; fi
	echo "${FULLPATH}"
}

resources_dir() {
	cd "$(dirname "$0")/../resources" && pwd
}

temp_dir() {
	if [ "$(uname -s)" == "Darwin" ]; then
		mktemp -dt libgit2_bench
	else
		mktemp -dt libgit2_bench.XXXXXXX
	fi
}

create_preparescript() {
	# add some functions for users to use in preparation
	cat >> "${SANDBOX_DIR}/prepare.sh" << EOF
	set -e

	SANDBOX_DIR="${SANDBOX_DIR}"
	RESOURCES_DIR="$(resources_dir)"

	create_text_file() {
		FILENAME="\${1}"
		SIZE="\${2}"

		if [ "\${FILENAME}" = "" ]; then
			echo "usage: create_text_file <name> [size]" 1>&2
			exit 1
		fi

		if [ "\${SIZE}" = "" ]; then
			SIZE="1024"
		fi

		if [[ "\$(uname -s)" == "MINGW"* ]]; then
			EOL="\r\n"
			EOL_LEN="2"
			CONTENTS="This is a reproducible text file. (With Unix line endings.)\n"
			CONTENTS_LEN="60"
		else
			EOL="\n"
			EOL_LEN="1"
			CONTENTS="This is a reproducible text file. (With DOS line endings.)\r\n"
			CONTENTS_LEN="60"
		fi

		rm -f "\${FILENAME:?}"
		touch "\${FILENAME}"

		if [ "\${SIZE}" -ge "\$((\${CONTENTS_LEN} + \${EOL_LEN}))" ]; then
			SIZE="\$((\${SIZE} - \${CONTENTS_LEN}))"
			COUNT="\$(((\${SIZE} - \${EOL_LEN}) / \${CONTENTS_LEN}))"

			if [ "\${SIZE}" -gt "\${EOL_LEN}" ]; then
				dd if="\${FILENAME}" of="\${FILENAME}" bs="\${CONTENTS_LEN}" seek=1 count="\${COUNT}" 2>/dev/null
			fi

			SIZE="\$((\${SIZE} - (\${COUNT} * \${CONTENTS_LEN})))"
                fi

		while [ "\${SIZE}" -gt "\${EOL_LEN}" ]; do
			echo -ne "." >> "\${FILENAME}"
			SIZE="\$((\${SIZE} - 1))"
		done

		if [ "\${SIZE}" = "\${EOL_LEN}" ]; then
			echo -ne "\${EOL}" >> "\${FILENAME}"
			SIZE="\$((\${SIZE} - \${EOL_LEN}))"
		else
			while [ "\${SIZE}" -gt "0" ]; do
				echo -ne "." >> "\${FILENAME}"
				SIZE="\$((\${SIZE} - 1))"
			done
		fi
	}

	create_random_file() {
		FILENAME="\${1}"
		SIZE="\${2}"

		if [ "\${FILENAME}" = "" ]; then
			echo "usage: create_random_file <name> [size]" 1>&2
			exit 1
		fi

		if [ "\${SIZE}" = "" ]; then
			SIZE="1024"
		fi

		dd if="/dev/urandom" of="\${FILENAME}" bs="\${SIZE}" count=1 2>/dev/null
	}

	flush_disk_cache() {
		if [ "\$(uname -s)" = "Darwin" ]; then
			sync && sudo purge
		elif [ "\$(uname -s)" = "Linux" ]; then
			sync && echo 3 | sudo tee /proc/sys/vm/drop_caches >/dev/null
		elif [[ "\$(uname -s)" == "MINGW"* ]]; then
			PurgeStandbyList
		fi
	}

	sandbox() {
		RESOURCE="\${1}"

		if [ "\${RESOURCE}" = "" ]; then
			echo "usage: sandbox <path>" 1>&2
			exit 1
		fi

		if [ ! -d "\${RESOURCES_DIR}/\${RESOURCE}" ]; then
			echo "sandbox: the resource \"\${RESOURCE}\" does not exist"
			exit 1
		fi

		rm -rf "\${SANDBOX_DIR:?}/\${RESOURCE}"
		cp -R "\${RESOURCES_DIR}/\${RESOURCE}" "\${SANDBOX_DIR}/"
	}

	sandbox_repo() {
		RESOURCE="\${1}"

		sandbox "\${RESOURCE}"

		if [ -d "\${SANDBOX_DIR}/\${RESOURCE}/.gitted" ]; then
			mv "\${SANDBOX_DIR}/\${RESOURCE}/.gitted" "\${SANDBOX_DIR}/\${RESOURCE}/.git";
		fi
		if [ -f "\${SANDBOX_DIR}/\${RESOURCE}/gitattributes" ]; then
			mv "\${SANDBOX_DIR}/\${RESOURCE}/gitattributes" "\${SANDBOX_DIR}/\${RESOURCE}/.gitattributes";
		fi
		if [ -f "\${SANDBOX_DIR}/\${RESOURCE}/gitignore" ]; then
			mv "\${SANDBOX_DIR}/\${RESOURCE}/gitignore" "\${SANDBOX_DIR}/\${RESOURCE}/.gitignore";
		fi
	}

	cd "\${SANDBOX_DIR}"
EOF

	if [ "${PREPARE}" != "" ]; then
		echo "" >> "${SANDBOX_DIR}/prepare.sh"
		echo "${PREPARE}" >> "${SANDBOX_DIR}/prepare.sh"
	fi

	echo "${SANDBOX_DIR}/prepare.sh"
}

create_runscript() {
	SCRIPT_NAME="${1}"; shift
	CLI_PATH="${1}"; shift

	if [[ "${CHDIR}" = "/"* ]]; then
		START_DIR="${CHDIR}"
	elif [ "${CHDIR}" != "" ]; then
		START_DIR="${SANDBOX_DIR}/${CHDIR}"
	else
		START_DIR="${SANDBOX_DIR}"
	fi

	# our run script starts by chdir'ing to the sandbox or repository directory
	echo -n "cd \"${START_DIR}\" && \"${CLI_PATH}\"" >> "${SANDBOX_DIR}/${SCRIPT_NAME}.sh"

	for a in "$@"; do
		echo -n " \"${a}\"" >> "${SANDBOX_DIR}/${SCRIPT_NAME}.sh"
	done

	echo "${SANDBOX_DIR}/${SCRIPT_NAME}.sh"
}

gitbench_usage() { echo "usage: gitbench command..."; }

#
# this is the function that the outer script calls to actually do the sandboxing and
# invocation of hyperfine.
#
gitbench() {
	NEXT=

	# this test should run the given command in preparation of the tests
	# this preparation script will be run _after_ repository creation and
	# _before_ flushing the disk cache
	PREPARE=

	# this test should run within the given directory; this is a
	# relative path beneath the sandbox directory.
	CHDIR=

	# this test should run `n` warmups
	WARMUP=0

	if [ "$*" = "" ]; then
		gitbench_usage 1>&2
		exit 1
	fi

	for a in "$@"; do
		if [ "${NEXT}" = "warmup" ]; then
			WARMUP="${a}"
			NEXT=
		elif [ "${NEXT}" = "prepare" ]; then
			PREPARE="${a}"
			NEXT=
		elif [ "${NEXT}" = "chdir" ]; then
			CHDIR="${a}"
			NEXT=
		elif [ "${a}" = "--warmup" ]; then
			NEXT="warmup"
		elif [ "${a}" = "--prepare" ]; then
			NEXT="prepare"
		elif [ "${a}" = "--chdir" ]; then
			NEXT="chdir"
		elif [[ "${a}" == "--"* ]]; then
			echo "unknown argument: \"${a}\"" 1>&2
			gitbench_usage 1>&2
			exit 1
		else
			break
		fi

		shift
	done

	if [ "${NEXT}" != "" ]; then
		echo "$(basename "$0"): option requires a value: --${NEXT}" 1>&2
		gitbench_usage 1>&2
		exit 1
	fi

	# sanity check

	for a in "${SANDBOX[@]}"; do
		if [ ! -d "$(resources_dir)/${a}" ]; then
			echo "$0: no resource '${a}' found" 1>&2
			exit 1
		fi
	done

	if [ "$REPOSITORY" != "" ]; then
		if [ ! -d "$(resources_dir)/${REPOSITORY}" ]; then
			echo "$0: no repository resource '${REPOSITORY}' found" 1>&2
			exit 1
		fi
	fi

	# set up our sandboxing

	SANDBOX_DIR="$(temp_dir)"

	if [ "${BASELINE_CLI}" != "" ]; then
		BASELINE_CLI_PATH=$(fullpath "${BASELINE_CLI}")
		BASELINE_RUN_SCRIPT=$(create_runscript "baseline" "${BASELINE_CLI_PATH}" "$@")
	fi
	TEST_CLI_PATH=$(fullpath "${TEST_CLI}")
	TEST_RUN_SCRIPT=$(create_runscript "test" "${TEST_CLI_PATH}" "$@")

	PREPARE_SCRIPT="$(create_preparescript)"
	ARGUMENTS=("--prepare" "bash ${PREPARE_SCRIPT}" "--warmup" "${WARMUP}")

	if [ "${OUTPUT_STYLE}" != "" ]; then
		ARGUMENTS+=("--style" "${OUTPUT_STYLE}")
	fi

	if [ "${SHOW_OUTPUT}" != "" ]; then
		ARGUMENTS+=("--show-output")
	fi

	if [ "$JSON" != "" ]; then
		ARGUMENTS+=("--export-json" "${JSON}")
	fi

	if [ "${BASELINE_CLI}" != "" ]; then
		ARGUMENTS+=("-n" "${BASELINE_CLI} $*" "bash ${BASELINE_RUN_SCRIPT}")
	fi

	ARGUMENTS+=("-n" "${TEST_CLI} $*" "bash ${TEST_RUN_SCRIPT}")

	hyperfine "${ARGUMENTS[@]}"
	rm -rf "${SANDBOX_DIR:?}"
}
