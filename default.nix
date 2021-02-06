{ nixpkgs ? import <nixpkgs> {}, haskellPackages ? nixpkgs.haskellPackages }:
let
  inherit (nixpkgs) pkgs;
  systemPackages = with pkgs; [
    haskellPackages.haskell-language-server
    haskellPackages.cabal-install
    haskellPackages.hsc2hs
    lxd
    zlib
  ];
in pkgs.stdenv.mkDerivation {
  name = "lxc-env";
  buildInputs = systemPackages;
}
