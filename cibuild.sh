#!/usr/bin/env bash

# I don't think there is a way for me to have the nix environment globally on a
# debian system
export NIX_REMOTE=daemon
source /usr/local/etc/profile.d/nix.sh

set -e

# nix-shell -p 'haskellPackages.hpack' --run 'hpack'
nix-shell -p 'haskellPackages.cabal2nix' --run 'cabal2nix ./. > default.nix'
nix-build shell.nix
# nix-shell -p 'haskellPackages.cabal-install' --run 'cabal --enable-nix build'
# nix-shell -p 'haskellPackages.cabal-install' --run 'cabal --enable-nix test'
# nix-shell -p 'haskellPackages.hlint' --run 'hlint src'

