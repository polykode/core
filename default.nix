{ nixpkgs ? import <nixpkgs> {}, haskellPackages ? nixpkgs.haskellPackages }:
let
  inherit (nixpkgs) pkgs;
  systemPackages = with pkgs; [
    haskellPackages.haskell-language-server
    haskellPackages.cabal-install
    lxd
  ];
in pkgs.stdenv.mkDerivation {
  name = "lxc-env";
  buildInputs = systemPackages;
}
