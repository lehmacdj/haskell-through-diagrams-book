#!/usr/bin/env bash
set -e

mdbook build
git checkout gh-pages
mv book .book
rm -rf -- *
mv .book/* .
rmdir .book
git add . || :
git commit -m "update published book pages" || :
git push || :
git checkout master
