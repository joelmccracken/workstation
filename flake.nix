{
  description = "Joel's systems";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin-nixpkgs = {
      url = "github:nixos/nixpkgs/nixpkgs-22.11-darwin";
    };
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "darwin-nixpkgs";
    darwin-home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "darwin-nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-doom-emacs = {
      url = "github:nix-community/nix-doom-emacs";
      # inputs.nixpkgs.follows = "nixpkgs";
    };

    # emacs-overlay = {
    #   url = "github:nix-community/emacs-overlay/master";
    #   # inputs.nixpkgs.follows = "darwin-nixpkgs";
    # };

    # doom-emacs = { url = "github:hlissner/doom-emacs/develop"; flake = false; };

    # darwin-nix-doom-emacs = {
    #   url = "github:nix-community/nix-doom-emacs";
    #   # inputs = {
    #   #   nixpkgs.follows = "darwin-nixpkgs";
    #   #   emacs-overlay.follows = "emacs-overlay";
    #   #   doom-emacs.follows = "doom-emacs";
    #   # };
    # };
  };

  outputs = inputs@{ self, darwin, nixpkgs, darwin-nixpkgs, home-manager, darwin-home-manager, nix-doom-emacs, # darwin-nix-doom-emacs,
    ... }:
    let
      darwin-home-config = let
          system = "x86_64-darwin";
          pkgs = darwin-nixpkgs.legacyPackages.${system};
        in darwin-home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          modules = [
            nix-doom-emacs.hmModule
            ./home.nix
          ]
;
        };
      linux-home-config =
        let
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.${system};
        in home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          modules = [
            nix-doom-emacs.hmModule
            ./home.nix
          ];

        };
    in
    {
      darwinConfigurations."glamdring" = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [ ./darwin-configuration.nix ];
      };

      homeConfigurations.glamdring.joel = darwin-home-config;
      homeConfigurations."ci-macos".runner = darwin-home-config;
      homeConfigurations."ci-ubuntu".runner = linux-home-config;
    };
}
