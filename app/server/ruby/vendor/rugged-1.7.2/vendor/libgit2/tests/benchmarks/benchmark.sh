#!/bin/bash

set -eo pipefail

#
# parse the command line
#

usage() { echo "usage: $(basename "$0") [--cli <path>] [--baseline-cli <path>] [--suite <suite>] [--json <path>] [--zip <path>] [--verbose] [--debug]"; }

TEST_CLI="git"
BASELINE_CLI=
SUITE=
JSON_RESULT=
ZIP_RESULT=
OUTPUT_DIR=
VERBOSE=
DEBUG=
NEXT=

for a in "$@"; do
	if [ "${NEXT}" = "cli" ]; then
		TEST_CLI="${a}"
		NEXT=
	elif [ "${NEXT}" = "baseline-cli" ]; then
		BASELINE_CLI="${a}"
		NEXT=
	elif [ "${NEXT}" = "suite" ]; then
		SUITE="${a}"
		NEXT=
	elif [ "${NEXT}" = "json" ]; then
		JSON_RESULT="${a}"
		NEXT=
	elif [ "${NEXT}" = "zip" ]; then
		ZIP_RESULT="${a}"
		NEXT=
	elif [ "${NEXT}" = "output-dir" ]; then
		OUTPUT_DIR="${a}"
		NEXT=
	elif [ "${a}" = "c" ] || [ "${a}" = "--cli" ]; then
		NEXT="cli"
	elif [[ "${a}" == "-c"* ]]; then
		TEST_CLI="${a/-c/}"
	elif [ "${a}" = "b" ] || [ "${a}" = "--baseline-cli" ]; then
		NEXT="baseline-cli"
	elif [[ "${a}" == "-b"* ]]; then
		BASELINE_CLI="${a/-b/}"
	elif [ "${a}" = "-s" ] || [ "${a}" = "--suite" ]; then
		NEXT="suite"
	elif [[ "${a}" == "-s"* ]]; then
		SUITE="${a/-s/}"
	elif [ "${a}" = "-v" ] || [ "${a}" == "--verbose" ]; then
		VERBOSE=1
	elif [ "${a}" == "--debug" ]; then
		VERBOSE=1
		DEBUG=1
	elif [ "${a}" = "-j" ] || [ "${a}" == "--json" ]; then
		NEXT="json"
	elif [[ "${a}" == "-j"* ]]; then
		JSON_RESULT="${a/-j/}"
	elif [ "${a}" = "-z" ] || [ "${a}" == "--zip" ]; then
		NEXT="zip"
	elif [[ "${a}" == "-z"* ]]; then
		ZIP_RESULT="${a/-z/}"
	elif [ "${a}" = "--output-dir" ]; then
		NEXT="output-dir"
	else
		echo "$(basename "$0"): unknown option: ${a}" 1>&2
		usage 1>&2
		exit 1
	fi
done

if [ "${NEXT}" != "" ]; then
	usage 1>&2
	exit 1
fi

if [ "${OUTPUT_DIR}" = "" ]; then
	OUTPUT_DIR=${OUTPUT_DIR:="$(mktemp -d)"}
	CLEANUP_DIR=1
fi

#
# collect some information about the test environment
#

SYSTEM_OS=$(uname -s)
if [ "${SYSTEM_OS}" = "Darwin" ]; then SYSTEM_OS="macOS"; fi

SYSTEM_KERNEL=$(uname -v)

fullpath() {
	if [[ "$(uname -s)" == "MINGW"* && $(cygpath -u "${TEST_CLI}") == "/"* ]]; then
		echo "${TEST_CLI}"
	elif [[ "${TEST_CLI}" == "/"* ]]; then
		echo "${TEST_CLI}"
	else
		which "${TEST_CLI}"
	fi
}

cli_version() {
	if [[ "$(uname -s)" == "MINGW"* ]]; then
		$(cygpath -u "$1") --version
	else
		"$1" --version
	fi
}

TEST_CLI_NAME=$(basename "${TEST_CLI}")
TEST_CLI_PATH=$(fullpath "${TEST_CLI}")
TEST_CLI_VERSION=$(cli_version "${TEST_CLI}")

if [ "${BASELINE_CLI}" != "" ]; then
	if [[ "${BASELINE_CLI}" == "/"* ]]; then
		BASELINE_CLI_PATH="${BASELINE_CLI}"
	else
		BASELINE_CLI_PATH=$(which "${BASELINE_CLI}")
	fi

	BASELINE_CLI_NAME=$(basename "${BASELINE_CLI}")
	BASELINE_CLI_PATH=$(fullpath "${BASELINE_CLI}")
	BASELINE_CLI_VERSION=$(cli_version "${BASELINE_CLI}")
fi

#
# run the benchmarks
#

echo "##############################################################################"
if [ "${SUITE}" != "" ]; then
	SUITE_PREFIX="${SUITE/::/__}"
	echo "## Running ${SUITE} benchmarks"
else
	echo "## Running all benchmarks"
fi
echo "##############################################################################"
echo ""

if [ "${BASELINE_CLI}" != "" ]; then
	echo "# Baseline CLI: ${BASELINE_CLI} (${BASELINE_CLI_VERSION})"
fi
echo "# Test CLI: ${TEST_CLI} (${TEST_CLI_VERSION})"
echo ""

BENCHMARK_DIR=${BENCHMARK_DIR:=$(dirname "$0")}
ANY_FOUND=
ANY_FAILED=

indent() { sed "s/^/  /"; }
time_in_ms() { if [ "$(uname -s)" = "Darwin" ]; then date "+%s000"; else date "+%s%N" ; fi; }
humanize_secs() {
	units=('s' 'ms' 'us' 'ns')
	unit=0
	time="${1}"

	if [ "${time}" = "" ]; then
		echo ""
		return
	fi

	# bash doesn't do floating point arithmetic.  ick.
	while [[ "${time}" == "0."* ]] && [ "$((unit+1))" != "${#units[*]}" ]; do
		time="$(echo | awk "{ print ${time} * 1000 }")"
		unit=$((unit+1))
	done

	echo "${time} ${units[$unit]}"
}

TIME_START=$(time_in_ms)

for TEST_PATH in "${BENCHMARK_DIR}"/*; do
	TEST_FILE=$(basename "${TEST_PATH}")

	if [ ! -f "${TEST_PATH}" ] || [ ! -x "${TEST_PATH}" ]; then
		continue
	fi

	if [[ "${TEST_FILE}" != *"__"* ]]; then
		continue
	fi

	if [[ "${TEST_FILE}" != "${SUITE_PREFIX}"* ]]; then
		continue
	fi

	ANY_FOUND=1
	TEST_NAME="${TEST_FILE/__/::}"

	echo -n "${TEST_NAME}:"
	if [ "${VERBOSE}" = "1" ]; then
		echo ""
	else
		echo -n "  "
	fi

	if [ "${DEBUG}" = "1" ]; then
		SHOW_OUTPUT="--show-output"
	fi

	OUTPUT_FILE="${OUTPUT_DIR}/${TEST_FILE}.out"
	JSON_FILE="${OUTPUT_DIR}/${TEST_FILE}.json"
	ERROR_FILE="${OUTPUT_DIR}/${TEST_FILE}.err"

	FAILED=
	${TEST_PATH} --cli "${TEST_CLI}" --baseline-cli "${BASELINE_CLI}" --json "${JSON_FILE}" ${SHOW_OUTPUT} >"${OUTPUT_FILE}" 2>"${ERROR_FILE}" || FAILED=1

	if [ "${FAILED}" = "1" ]; then
		if [ "${VERBOSE}" != "1" ]; then
			echo "failed!"
		fi

		indent < "${ERROR_FILE}"
		ANY_FAILED=1
		continue
	fi

	# in verbose mode, just print the hyperfine results; otherwise,
	# pull the useful information out of its json and summarize it
	if [ "${VERBOSE}" = "1" ]; then
		indent < "${OUTPUT_FILE}"
	else
		jq -r '[ .results[0].mean, .results[0].stddev, .results[1].mean, .results[1].stddev ] | @tsv' < "${JSON_FILE}" | while IFS=$'\t' read -r one_mean one_stddev two_mean two_stddev; do
			one_mean=$(humanize_secs "${one_mean}")
			one_stddev=$(humanize_secs "${one_stddev}")

			if [ "${two_mean}" != "" ]; then
				two_mean=$(humanize_secs "${two_mean}")
				two_stddev=$(humanize_secs "${two_stddev}")

				echo "${one_mean} ± ${one_stddev}  vs  ${two_mean} ± ${two_stddev}"
			else
				echo "${one_mean} ± ${one_stddev}"
			fi
		done
	fi

	# add our metadata to the hyperfine json result
	jq ". |= { \"name\": \"${TEST_NAME}\" } + ." < "${JSON_FILE}" > "${JSON_FILE}.new" && mv "${JSON_FILE}.new" "${JSON_FILE}"
done

TIME_END=$(time_in_ms)

if [ "$ANY_FOUND" != "1" ]; then
	echo ""
	echo "error: no benchmark suite \"${SUITE}\"."
	echo ""
	exit 1
fi

escape() {
	echo "${1//\\/\\\\}"
}

# combine all the individual benchmark results into a single json file
if [ "${JSON_RESULT}" != "" ]; then
	if [ "${VERBOSE}" = "1" ]; then
		echo ""
		echo "# Writing JSON results: ${JSON_RESULT}"
	fi

	SYSTEM_JSON="{ \"os\": \"${SYSTEM_OS}\",  \"kernel\": \"${SYSTEM_KERNEL}\" }"
	TIME_JSON="{ \"start\": ${TIME_START}, \"end\": ${TIME_END} }"
	TEST_CLI_JSON="{ \"name\": \"${TEST_CLI_NAME}\", \"path\": \"$(escape "${TEST_CLI_PATH}")\", \"version\": \"${TEST_CLI_VERSION}\" }"
	BASELINE_CLI_JSON="{ \"name\": \"${BASELINE_CLI_NAME}\", \"path\": \"$(escape "${BASELINE_CLI_PATH}")\", \"version\": \"${BASELINE_CLI_VERSION}\" }"

	if [ "${BASELINE_CLI}" != "" ]; then
		EXECUTOR_JSON="{ \"baseline\": ${BASELINE_CLI_JSON}, \"cli\": ${TEST_CLI_JSON} }"
	else
		EXECUTOR_JSON="{ \"cli\": ${TEST_CLI_JSON} }"
	fi

	# add our metadata to all the test results
	jq -n "{ \"system\": ${SYSTEM_JSON}, \"time\": ${TIME_JSON}, \"executor\": ${EXECUTOR_JSON}, \"tests\": [inputs] }" "${OUTPUT_DIR}"/*.json > "${JSON_RESULT}"
fi

# combine all the data into a zip if requested
if [ "${ZIP_RESULT}" != "" ]; then
	if [ "${VERBOSE}" = "1" ]; then
		if [ "${JSON_RESULT}" = "" ]; then echo ""; fi
		echo "# Writing ZIP results: ${ZIP_RESULT}"
	fi

	zip -jr "${ZIP_RESULT}" "${OUTPUT_DIR}" >/dev/null
fi

if [ "$CLEANUP_DIR" = "1" ]; then
	rm -f "${OUTPUT_DIR}"/*.out
	rm -f "${OUTPUT_DIR}"/*.err
	rm -f "${OUTPUT_DIR}"/*.json
	rmdir "${OUTPUT_DIR}"
fi

if [ "$ANY_FAILED" = "1" ]; then
	exit 1
fi
