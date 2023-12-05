{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [ 
    pkgs.bats
    pkgs.erlang
    pkgs.entr
  ];
}
