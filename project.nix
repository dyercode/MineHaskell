{ mkDerivation, base, either, hedgehog, lib, tasty, tasty-hedgehog
, tasty-hunit
}:
mkDerivation {
  pname = "MineHaskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base either ];
  executableHaskellDepends = [ base either ];
  testHaskellDepends = [
    base either hedgehog tasty tasty-hedgehog tasty-hunit
  ];
  license = "unknown";
  mainProgram = "MineHaskellExe";
}
