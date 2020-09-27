#!/bin/sh

set -eu

branch=$(git rev-parse --abbrev-ref HEAD)

if [ "$branch" != "main" ]; then
  echo "Cannot deploy from branch $branch; switch to main first"
  exit 2
fi

if [ ! -d "./dist" ]; then
  echo "dist directory doesn't exist; run yarn deploy or yarn bundle"
  exit 2
fi

git branch -D gh-pages 2>/dev/null || true
git branch -D draft 2>/dev/null || true
git checkout -b draft
cp ./static/CNAME ./dist/CNAME
git add -f ./dist
git commit -am "Deploy to gh-pages on $(date)"
git subtree split --prefix dist -b gh-pages
git push --force origin gh-pages:gh-pages
git checkout main
