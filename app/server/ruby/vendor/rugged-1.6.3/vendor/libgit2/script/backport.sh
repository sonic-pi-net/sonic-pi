#!/bin/sh

if test $# -eq 0
then
    echo "USAGE: $0 <#PR> [<#PR>...]"
    exit
fi

commits=

for pr in $*
do
    mergecommit=$(git rev-parse ":/Merge pull request #$pr" || exit 1)
    mergebase=$(git merge-base "$mergecommit"^1 "$mergecommit"^2 || exit 1)

    commits="$commits $(git rev-list --reverse "$mergecommit"^2 ^"$mergebase")"
done

echo "Cherry-picking the following commits:"
git rev-list --no-walk --oneline $commits
echo

git cherry-pick $commits
