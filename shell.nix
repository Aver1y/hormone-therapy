{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc883" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
.overrideAttrs(attrs: {buildInputs = attrs.buildInputs ++ [ nixpkgs.yarn ];})
