{ nixpkgs ? import <nixpkgs> {} }:

# TODO: Add lxc -> libexec in path
let
  inherit (nixpkgs) pkgs;
  systemPackages = with pkgs; [
    lxc
    gnupg
    iptables
    iproute
    bridge-utils
    dnsmasq
    libpam-wrapper
    shadow

    # Work
    man
    vim
    #git
  ];
in pkgs.stdenv.mkDerivation {
  name = "lxc-env";
  buildInputs = systemPackages;
}
