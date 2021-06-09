let
  nixpkgs = import ./nixpkgs.nix;
  packages = import ./packages.nix;
in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; packages;
}
