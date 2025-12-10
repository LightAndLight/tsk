{ mkDerivation, attoparsec, barbies, base, binary, bytestring
, cassava, containers, cryptohash-md5, directory, dlist, entropy
, generics-eot, hashable, hedgehog, hspec, hspec-hedgehog, lib, mtl
, optparse-applicative, process, text, time, uuid
}:
mkDerivation {
  pname = "tsk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    barbies base binary bytestring containers cryptohash-md5 directory
    dlist entropy generics-eot hashable mtl text time uuid
  ];
  executableHaskellDepends = [
    attoparsec barbies base bytestring cassava containers directory mtl
    optparse-applicative process text time
  ];
  testHaskellDepends = [
    base containers hashable hedgehog hspec hspec-hedgehog uuid
  ];
  license = lib.licenses.gpl3Only;
}
