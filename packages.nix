let
  nixpkgs = import ./nixpkgs.nix;

  erlangChannel = nixpkgs.nixerl.erlang-23-2-1.overrideScope' (self: super: {
    erlang = super.erlang.override {
      wxSupport = false;
    };
  });

in

with nixpkgs;

let
    inherit (stdenv.lib) optionals;
in

with pkgs; [

    erlangChannel.erlang
    erlangChannel.rebar3
    erlangChannel.erlang-ls

    purerl-support.purescript-0-14-1
    purerl-support.spago-0-20-3

    # Purerl backend for purescript
    purerl.purerl-0-0-9
  ]

