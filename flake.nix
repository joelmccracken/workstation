{
  description = "Joel's darwin system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs }: {
    darwinConfigurations."glamdring" = darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      modules = [ ./darwin-configuration.nix ];
    };

    # darwinPackages = self.darwinConfigurations."glamdring".pkgs;
  };
}
