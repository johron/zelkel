{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.libgcc
    cargo
    rustc
    rustfmt
    rustPlatform.rustLibSrc
    rust-analyzer
  ];
}
