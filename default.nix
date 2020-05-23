{ nixpkgs ? import ./nixpkgs.nix
, compiler ? "ghc8101" }:

with nixpkgs;
with haskell.packages.${compiler};

developPackage {
  root = ./site-gen;
  source-overrides = {
    pandoc-url2cite-hs = fetchFromGitHub {
      owner = "Aver1y";
      repo = "pandoc-url2cite-hs";
      rev = "41e685d1f1788d3c152853bc72262ccbf02b5a07";
      sha256 = "1bx9a3fb0xcnjs0fl9k43qak61ag2kmdc6d2056ia421c1r0dq2f";
    };
    hakyll = fetchFromGitHub {
      owner = "Aver1y";
      repo = "hakyll";
      rev = "72385bea24fc918ee7cb0664555055023f831008";
      sha256 = "1sqdm0scjxjni3s4m7vy5by6b1gjjx6cxyfpspplkxdv4v5c8pk2";
    };
  };
  overrides = self: super: {
    pandoc-citeproc = self.pandoc-citeproc_0_17;
  };
}
