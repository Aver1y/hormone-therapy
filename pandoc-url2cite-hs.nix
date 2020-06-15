{ mkDerivation, aeson, base, containers, data-default, directory
, fetchgit, filepath, lens, modern-uri, pandoc-citeproc
, pandoc-types, relude, req, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "pandoc-url2cite-hs";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/Aver1y/pandoc-url2cite-hs";
    sha256 = "1wasf2lw1nimli57jzpbdqjq5n4lyz2nzqfbpqbxacc2j94ax72m";
    rev = "5e16501451cf232e71e7b8ecd4d880ef0a86f18f";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers data-default directory filepath lens
    modern-uri pandoc-citeproc pandoc-types relude req text
    unordered-containers
  ];
  executableHaskellDepends = [ base lens pandoc-types relude ];
  description = "Pandoc filter that fetches bibliography entires with only a url given";
  license = stdenv.lib.licenses.agpl3Plus;
}
