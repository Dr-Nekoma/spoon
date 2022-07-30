{ pkgs ? import <nixpkgs> {} }:
let
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    rebar3
    erlang
    erlang-ls
  ];
  shellHook = ''
  '';
}
