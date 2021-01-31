{ nixpkgs ? import <nixpkgs> {}, haskellPackages ? nixpkgs.haskellPackages, compiler ? "default", doBenchmark ? false }:
let
  inherit (nixpkgs) pkgs;
  systemPackages = with pkgs; [
    haskellPackages.haskell-language-server
    haskellPackages.cabal-install
    docker-compose
    docker
    entr

    #lxc
    #gnupg
    #dnsmasq
    #iptables
    #bridge-utils
  ];
in pkgs.stdenv.mkDerivation {
  name = "lxc-env";
  buildInputs = systemPackages;
}
