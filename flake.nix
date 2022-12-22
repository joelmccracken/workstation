{
  description = "Joel's darwin system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, darwin, nixpkgs, home-manager }:
    {
      darwinConfigurations."glamdring" = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [ ./darwin-configuration.nix ];
      };

      homeConfigurations.joel =
        let
          pkgs = nixpkgs.legacyPackages."x86_64-darwin";
        in home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [
            ./home.nix
          ];

          # Optionally use extraSpecialArgs
          # to pass through arguments to home.nix
      };
    };
}
