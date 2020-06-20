#!/bin/sh

chmod -R +w gen-cached
rm -rf gen-cached && mkdir gen-cached &&
sed "s;/nix/store;gen-cached;g" result/bin/site-gen > gen-cached/site-gen &&
chmod +x gen-cached/site-gen &&
cp -r `grep -aoe "/nix/store/[^/]*" result/bin/site-gen` gen-cached
