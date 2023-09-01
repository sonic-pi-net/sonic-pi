#!/bin/sh
#creates push_src repo for libgit2 push tests.
set -eu

#Create src repo for push
mkdir push_src
pushd push_src
  git init

  echo a > a.txt
  git add .
  git commit -m 'added a.txt'

  mkdir fold
  echo b > fold/b.txt
  git add .
  git commit -m 'added fold and fold/b.txt'

  git branch b1 #b1 and b2 are the same
  git branch b2

  git checkout -b b3
  echo edit >> a.txt
  git add .
  git commit -m 'edited a.txt'

  git checkout -b b4 master
  echo edit >> fold\b.txt
  git add .
  git commit -m 'edited fold\b.txt'

  git checkout -b b5 master
  git submodule add ../testrepo.git submodule
  git commit -m "added submodule named 'submodule' pointing to '../testrepo.git'"

  git checkout master
  git merge -m "merge b3, b4, and b5 to master" b3 b4 b5

  #Log commits to include in testcase
  git log --format=oneline --decorate --graph
  #*-.   951bbbb90e2259a4c8950db78946784fb53fcbce (HEAD, master) merge b3, b4, and b5 to master
  #|\ \
  #| | * fa38b91f199934685819bea316186d8b008c52a2 (b5) added submodule named 'submodule' pointing to '../testrepo.git'
  #| * | 27b7ce66243eb1403862d05f958c002312df173d (b4) edited fold\b.txt
  #| |/
  #* | d9b63a88223d8367516f50bd131a5f7349b7f3e4 (b3) edited a.txt
  #|/
  #* a78705c3b2725f931d3ee05348d83cc26700f247 (b2, b1) added fold and fold/b.txt
  #* 5c0bb3d1b9449d1cc69d7519fd05166f01840915 added a.txt

  #fix paths so that we can add repo folders under libgit2 repo
  #rename .git to .gitted
  find . -name .git -exec mv -i '{}' '{}ted' \;
  mv -i .gitmodules gitmodules
popd
