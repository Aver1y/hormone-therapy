{ pkgs ? import ./nixpkgs.nix
, compiler ? "ghc8101" }:

pkgs.haskell.packages.${compiler}.developPackage {
  root = ./site-gen;
  source-overrides = {
    pandoc-url2cite-hs = pkgs.fetchFromGitHub {
      owner = "Aver1y";
      repo = "pandoc-url2cite-hs";
      rev = "41e685d1f1788d3c152853bc72262ccbf02b5a07";
      sha256 = "1bx9a3fb0xcnjs0fl9k43qak61ag2kmdc6d2056ia421c1r0dq2f";
    };
  };
  overrides = self: super: {
    pandoc-citeproc = self.pandoc-citeproc_0_17;
    hakyll = pkgs.haskell.lib.doJailbreak super.hakyll;
  };
}
