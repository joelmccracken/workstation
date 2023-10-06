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
  };

  outputs = inputs@{ self, darwin, nixpkgs, darwin-nixpkgs, home-manager, darwin-home-manager,
    ... }:
    let
      home-config = settings@{system, user, home, ...}:
        let
          pkgs = settings.pkgs.legacyPackages.${settings.system};

          home-manager-config =
            { config, pkgs, lib, ... }:
              {
                # Home Manager needs a bit of information about you and the
                # paths it should manage.
                home.username = user;
                home.homeDirectory = home;

                # This value determines the Home Manager release that your
                # configuration is compatible with. This helps avoid breakage
                # when a new Home Manager release introduces backwards
                # incompatible changes.
                #
                # You can update Home Manager without changing this value. See
                # the Home Manager release notes for a list of state version
                # changes in each release.
                home.stateVersion = "22.11";

                # Let Home Manager install and manage itself.
                programs.home-manager.enable = true;
                home.packages = [
                  pkgs.git
                  pkgs.ripgrep
                  pkgs.jq
                  pkgs.jl
                  pkgs.fd
                  pkgs.ispell
                  pkgs.bitwarden-cli
                  pkgs.direnv
                  pkgs.mr  # myrepos https://myrepos.branchable.com/install/
                  pkgs.graphviz
                  pkgs.cmake
                  pkgs.coreutils
                  pkgs.wget
                ];

                home.sessionPath = [
                  "~/.nix-profile/bin/"
                ];

                programs.emacs = {
                  enable = true;
                  extraPackages = epkgs: [ epkgs.vterm epkgs.sqlite];
                };

                # programs.doom-emacs = {
                #   enable = true;
                #   doomPrivateDir = ./dotfiles/doom.d;
                #   extraConfig = ''
                #     (add-to-list 'exec-path "~/.nix-profile/bin/")
                #   '';
                # };

                # workaround; see https://github.com/nix-community/home-manager/issues/3342#issuecomment-1283158398
                manual.manpages.enable = false;
              };
          in settings.hmModule.lib.homeManagerConfiguration {
              inherit pkgs;
              modules = [home-manager-config];
            };

      nix-darwin-config = {hostname, user, ...}:
        { config, pkgs, ... }:
          {
            networking.hostName = hostname;
            networking.localHostName = hostname;

            # List packages installed in system profile. To search by name, run:
            # $ nix-env -qaP | grep wget
            environment.systemPackages =
              [ pkgs.vim
              ];

            # Auto upgrade nix package and the daemon service.
            services.nix-daemon.enable = true;
            nix.package = pkgs.nix;

            # Create /etc/bashrc that loads the nix-darwin environment.
            programs.zsh.enable = true;  # default shell on catalina
            # programs.fish.enable = true;

            # Used for backwards compatibility, please read the changelog before changing.
            # $ darwin-rebuild changelog
            system.stateVersion = 4;
          };

      darwinConfig = user: hostname:
        darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          modules = [
            (nix-darwin-config { user = "joel"; hostname = "glamdring"; })
          ];
        };
    in
    {
      darwinConfigurations."glamdring" = darwinConfig "joel" "glamdring";
      darwinConfigurations."ci-macos" =  darwinConfig "runner" "ci-macos";

      homeConfigurations.glamdring.joel = home-config {
        user = "joel"; home = "/Users/joel"; system = "x86_64-darwin";
        hmModule = darwin-home-manager; pkgs = darwin-nixpkgs;
      };

      homeConfigurations."ci-macos".runner = home-config {
        user = "runner"; home = "/Users/runner"; system = "x86_64-darwin";
        hmModule = darwin-home-manager; pkgs = darwin-nixpkgs;
      };

      homeConfigurations."ci-ubuntu".runner = home-config {
        user = "runner"; home = "/home/runner"; system = "x86_64-linux";
        hmModule = home-manager;  pkgs = nixpkgs;
      };
    };
}
