{ nixpkgs ? import <nixpkgs> {}, haskellPackages ? nixpkgs.haskellPackages, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  systemPackages = [
    haskellPackages.haskell-language-server
    haskellPackages.cabal-install
    pkgs.entr # Re-run on file change
    pkgs.lxc
    #pkgs.lxd
    pkgs.gnupg
  ];

  commonHsPackages = with haskellPackages; [
    #base
    #cmark
    #raw-strings-qq
    #transformers
    #text
    #lxc
    #lxd-client
    #strict
    #unix
  ];
in
  with haskellPackages; mkDerivation {
    pname = "simlisp";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = commonHsPackages;
    executableHaskellDepends = commonHsPackages;
    executableSystemDepends = systemPackages;
    testHaskellDepends = commonHsPackages ++ [ hspec ];
    license = "MIT";
    hydraPlatforms = stdenv.lib.platforms.none;
  }
