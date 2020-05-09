# Build

**Dependencies:**

- yarn
- cabal

```
$ cabal build
$ cabal exec site rebuild
```

## Nix

```
$ nix-shell -p cabal2nix --run "cabal2nix . > hormone-therapy-info.nix"
$ nix-build
$ nix-shell --run "result/bin/site rebuild"
```
