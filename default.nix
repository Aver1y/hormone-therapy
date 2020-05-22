{ nixpkgs ? import ./nixpkgs.nix
, compiler ? "ghc8101" }:

with nixpkgs;
with haskell.packages.${compiler};

developPackage {
  root = ./.;
  source-overrides = {
    pandoc-url2cite-hs = fetchFromGitHub {
      owner = "Aver1y";
      repo = "pandoc-url2cite-hs";
      rev = "d6fac9756f0954c4b9911113f9639dcbba5d8a0f";
      sha256 = "0ksgpghsy064bn7rajlxk5a8a1b4shd20wp2l8pyifx04za9j1z6";
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
