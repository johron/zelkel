{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  # List of packages to include in the shell environment
  buildInputs = [
    pkgs.libgcc
    pkgs.cargo
    pkgs.rustc
    pkgs.rustfmt
    pkgs.rustPlatform.rustLibSrc
    pkgs.rust-analyzer
  ];
}
