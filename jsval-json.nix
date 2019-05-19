{ stdenv
, mkDerivation
, ghcjs-base
, mtl
, either
, transformers
, vector
, unordered-containers
, containers
, text
, hashable
, template-haskell
, aeson
, base
, bytestring
, generic-random
, hspec
, QuickCheck
, quickcheck-instances
, time
}:

mkDerivation {
  pname = "jsval-json";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base ghcjs-base mtl either transformers vector unordered-containers
    containers text hashable template-haskell
  ];
  testHaskellDepends = [
    aeson base bytestring containers generic-random ghcjs-base hashable
    hspec QuickCheck quickcheck-instances template-haskell text
    unordered-containers vector time
  ];
  homepage = "https://github.com/bitonic/jsval-json#readme";
  license = stdenv.lib.licenses.bsd3;
}
