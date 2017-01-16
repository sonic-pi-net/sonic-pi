#!/usr/bin/env bash

set -e

if [ "$1" == "" -o "$2" == "" ]; then
	echo "usage: $0 crlfrepo directory [tempdir]"
	exit 1
fi

input=$1
output=$2
tempdir=$3

set -u

create_repo() {
	local input=$1
	local output=$2
	local tempdir=$3
	local systype=$4
	local autocrlf=$5
	local attr=$6

	local worktree="${output}/${systype}/autocrlf_${autocrlf}"

	if [ "$attr" != "" ]; then
		local attrdir=`echo $attr | sed -e "s/ /,/g" | sed -e "s/=/_/g"`
		worktree="${worktree},${attrdir}"
	fi

	if [ "$tempdir" = "" ]; then
		local gitdir="${worktree}/.git"
	else
		local gitdir="${tempdir}/generate_crlf_${RANDOM}"
	fi

	echo "Creating ${worktree}"
	mkdir -p "${worktree}"

	git clone --no-checkout --quiet --bare "${input}/.gitted" "${gitdir}"
	git --work-tree="${worktree}" --git-dir="${gitdir}" config core.autocrlf ${autocrlf}

	if [ "$attr" != "" ]; then
		echo "* ${attr}" >> "${worktree}/.gitattributes"
	fi

	git --work-tree="${worktree}" --git-dir="${gitdir}" checkout HEAD

	if [ "$attr" != "" ]; then
		rm "${worktree}/.gitattributes"
	fi

	if [ "$tempdir" != "" ]; then
		rm -rf "${gitdir}"
	fi
}

if [[ `uname -s` == MINGW* ]]; then
	systype="windows"
else
	systype="posix"
fi

for autocrlf in true false input; do
	for attr in "" text text=auto -text crlf -crlf eol=lf eol=crlf \
		"text eol=lf" "text eol=crlf" \
		"text=auto eol=lf" "text=auto eol=crlf"; do

		create_repo "${input}" "${output}" "${tempdir}" \
			"${systype}" "${autocrlf}" "${attr}"
	done
done

