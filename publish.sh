#!/usr/bin/env bash
set -e

cargo >/dev/null 2>&1 || (echo "cargo / rust is not installed!" && exit 1)
mdbook --help >/dev/null 2>&1 || cargo install mdbook
mdbook build
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
