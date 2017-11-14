#!/usr/bin/env bash
set -e

git diff --quiet --ignore-submodules HEAD &>/dev/null || DIRTY=1

cargo >/dev/null 2>&1 || (echo "cargo / rust is not installed!" && exit 1)
mdbook --help >/dev/null 2>&1 || cargo install mdbook
mdbook build
[ $DIRTY ] && git stash
git checkout gh-pages
mv book .book
mv haskell-through-diagrams .haskell-through-diagrams
rm -rf -- *
mv .book/* .
rmdir .book
git add -- * || :
mv .haskell-through-diagrams haskell-through-diagrams
git commit -m "update published book pages" || :
git push || :
git checkout master
[ $DIRTY ] && git stash pop
