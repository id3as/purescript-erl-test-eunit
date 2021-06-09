let
  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "e5f945b13b3f6a39ec9fbb66c9794b277dc32aa1";
    };
  nixpkgs = import pinnedNix { overlays = []; };

  packages = import ./packages.nix;
in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; packages;
}
