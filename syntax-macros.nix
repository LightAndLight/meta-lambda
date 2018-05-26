{ mkDerivation, base, lens, megaparsec, stdenv }:
mkDerivation {
  pname = "syntax-macros";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens megaparsec ];
  license = stdenv.lib.licenses.bsd3;
}
