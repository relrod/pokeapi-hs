#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone "git@github.com:relrod/pokeapi-hs.git" "$f/pokeapi-hs.git"
cabal haddock
pushd "$f/pokeapi-hs.git"
  git checkout gh-pages && git rm -rf *
popd
mv dist/doc/html/pokeapi-hs/* "$f/pokeapi-hs.git/"
pushd "$f/pokeapi-hs.git"
  git add -A
  git commit -m "Manual docs deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: http://relrod.github.io/pokeapi-hs/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
