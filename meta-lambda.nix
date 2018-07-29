{ mkDerivation, base, hedgehog, lens, megaparsec, stdenv }:
mkDerivation {
  pname = "meta-lambda";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens megaparsec ];
  testHaskellDepends = [ base hedgehog lens ];
  license = stdenv.lib.licenses.bsd3;
}
