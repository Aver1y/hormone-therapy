{ mkDerivation, base, filepath, hakyll, hakyll-sass, pandoc
, pandoc-citeproc, pandoc-crossref, pandoc-types
, pandoc-url2cite-hs, process, regex-tdfa, stdenv
}:
mkDerivation {
  pname = "site-gen";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base filepath hakyll hakyll-sass pandoc pandoc-citeproc
    pandoc-crossref pandoc-types pandoc-url2cite-hs process regex-tdfa
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
