#!/usr/bin/env bash
#
# This script will generate the test corpus for CR/LF data using git;
# we create files with all possible line ending varieties (all LF, all
# CRLF, mixed, etc) on all the possible line ending configurations
# (`core.autocrlf=true`, `text=auto` in gitattributes, etc).  This
# allows us to validate that our configuration will match byte-for-byte
# the configuration that git produces.
#
# To update the test resource data, from the test resource directory:
#     git rm -r ./crlf_data/{posix,windows}
#     sh ./generate_crlf.sh ./crlf ./crlf_data /tmp/crlf_gitdirs
#     git add ./crlf_data/{posix,windows}

set -e

if [ "$1" == "" -o "$2" == "" ]; then
	echo "usage: $0 crlfrepo directory [tempdir]"
	exit 1
fi

input=$1
output=$2
tempdir=$3

set -u

create_to_workdir_data() {
	local input=$1
	local output=$2
	local tempdir=$3
	local systype=$4
	local autocrlf=$5
	local attr=$6

	local worktree="${output}/${systype}_to_workdir/autocrlf_${autocrlf}"

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

create_to_odb_data() {
	local input=$1
	local output=$2
	local tempdir=$3
	local systype=$4
	local autocrlf=$5
	local safecrlf=$6
	local attr=$7

	local destdir="${output}/${systype}_to_odb/autocrlf_${autocrlf},safecrlf_${safecrlf}"

	if [ "$attr" != "" ]; then
		local attrdir=`echo $attr | sed -e "s/ /,/g" | sed -e "s/=/_/g"`
		destdir="${destdir},${attrdir}"
	fi

	if [ "$tempdir" = "" ]; then
		local workdir="${destdir}/_workdir"
	else
		local workdir="${tempdir}/generate_crlf_${RANDOM}"
	fi

	echo "Creating ${destdir}"
	mkdir -p "${destdir}"

	git init "${workdir}" >/dev/null
	git --work-tree="${workdir}" --git-dir="${workdir}/.git" config core.autocrlf "${autocrlf}"
	git --work-tree="${workdir}" --git-dir="${workdir}/.git" config core.safecrlf "${safecrlf}"

	if [ "$attr" != "" ]; then
		echo "* ${attr}" > "${workdir}/.gitattributes"
	fi

	cp ${input}/* ${workdir}

	for path in ${workdir}/*; do
		filename=$(basename $path)
		failed=""
		output=$(git --work-tree="${workdir}" --git-dir="${workdir}/.git" add ${filename} 2>&1) || failed=1

		if [ ! -z "${failed}" -a "${output:0:35}" == "fatal: LF would be replaced by CRLF" ]; then
			echo "LF would be replaced by CRLF in '${filename}'" > "${destdir}/${filename}.fail"
		elif [ ! -z "${failed}" -a "${output:0:35}" == "fatal: CRLF would be replaced by LF" ]; then
			echo "CRLF would be replaced by LF in '${filename}'" > "${destdir}/${filename}.fail"
		elif [ ! -z "${failed}" ]; then
			echo "failed to add ${filename}: ${output}" 1>&2
			exit 1
		else
			git --work-tree="${workdir}" --git-dir="${workdir}/.git" cat-file blob ":${filename}" > "${destdir}/${filename}"
		fi
	done

	if [ "$tempdir" != "" ]; then
		rm -rf "${workdir}"
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

		create_to_workdir_data "${input}" "${output}" "${tempdir}" \
			"${systype}" "${autocrlf}" "${attr}"

		for safecrlf in true false warn; do
			create_to_odb_data "${input}" "${output}" "${tempdir}" \
				"${systype}" "${autocrlf}" "${safecrlf}" "${attr}"
		done
	done
done

