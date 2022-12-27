{
  description = "Joel's darwin system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin-nixpkgs = {
      url = "github:nixos/nixpkgs/nixpkgs-22.05-darwin";
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
  };

  outputs = { self, darwin, nixpkgs, home-manager }:
    let
      darwin-home-config = let
          system = "x86_64-darwin";
          pkgs = nixpkgs.legacyPackages.${system};
        in home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          modules = [
            ./home.nix
          ];
      };
      linux-home-config =
        let
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.${system};
        in home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          modules = [
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
      homeConfigurations."ci-macos".joel = darwin-home-config;
      homeConfigurations."ci-ubuntu".joel = linux-home-config;
    };
}
