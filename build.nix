{ ghcjsVersion ? "ghcjsHEAD" }:
let
  pkgs = import (import ./nixpkgs.nix) {};
  haskellPackages = pkgs.haskell.packages.${ghcjsVersion};
  jsval-json = haskellPackages.callPackage ./jsval-json.nix {};
in jsval-json
