{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "hrecursion-schemes";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  description = "Higher order recursion schemes";
  license = stdenv.lib.licenses.mit;
}
