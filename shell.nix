{ nixpkgs ? (import <nixpkgs> {}) }:
let
  overrides = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      hrecursion-schemes = self.callPackage ./. {};
    };
  };
  drv = overrides.hrecursion-schemes;
in
if nixpkgs.lib.inNixShell then
  drv.env
else
  drv
