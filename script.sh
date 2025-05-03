#!/bin/sh

set -exu

git clone https://github.com/CopticScriptorium/corpora.git /tmp/corpora
nix run . -- /tmp/corpora/
rsync -avmP --include='*/' --include='*.org' --exclude='*' /tmp/corpora/ site/content

cd site
nix develop ..#hugo -c hugo --gc --minify --baseURL $BASE_URL build
