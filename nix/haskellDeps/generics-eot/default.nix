{ mkDerivation, base, callPackage, directory, doctest, filepath
, hspec, interpolate, lib, markdown-unlit, mockery, QuickCheck
, shake, silently
}:
mkDerivation {
  pname = "generics-eot";
  version = "0.4.0.1";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base directory doctest filepath hspec interpolate markdown-unlit
    mockery QuickCheck shake silently
  ];
  testToolDepends = [ markdown-unlit ];
  homepage = "https://generics-eot.readthedocs.io/";
  description = "A library for generic programming that aims to be easy to understand";
  license = lib.licenses.bsd3;
}
