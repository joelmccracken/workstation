{
  description = "Joel's systems";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-updated.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin-nixpkgs = {
      url = "github:nixos/nixpkgs/nixpkgs-22.11-darwin";
    };
    darwin-nixpkgs-updated = {
      url = "github:nixos/nixpkgs/nixpkgs-unstable";
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

  outputs = inputs@{ self, darwin, nixpkgs, nixpkgs-updated, darwin-nixpkgs,
                     darwin-nixpkgs-updated, home-manager, darwin-home-manager,
                     ... }:
    let
      home-config = settings@{system, user, home, newer-pkgs, ...}:
        let
          newer-pkgs = settings.newer-pkgs.legacyPackages.${settings.system};
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
                  newer-pkgs.bitwarden-cli
                  pkgs.direnv
                  pkgs.mr  # myrepos https://myrepos.branchable.com/install/
                  pkgs.graphviz
                  pkgs.cmake
                  pkgs.coreutils
                  pkgs.wget
                  newer-pkgs.racket
                ];

                home.sessionPath = [
                  "~/.nix-profile/bin/"
                ];

                programs.emacs = {
                  enable = true;
                  extraPackages = epkgs: [ epkgs.vterm epkgs.sqlite];
                };

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

      darwinConfig = settings:
        darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          modules = [
            (nix-darwin-config settings)
          ];
        };

      macConfig = settings:
        {
          darwinConfigurations.${settings.hostname} = darwinConfig settings;

          homeConfigurations.${settings.hostname}.${settings.user} = home-config (
            settings // { hmModule = darwin-home-manager; pkgs = darwin-nixpkgs;
                          newer-pkgs = darwin-nixpkgs-updated; }
          );
        };

      linuxConfig = settings:
        {
          homeConfigurations.${settings.hostname}.${settings.user} = home-config (
            settings // { hmModule = home-manager;  pkgs = nixpkgs; newer-pkgs = nixpkgs-updated; }
          );
        };

      mergeDefs = m1: m2: {
        darwinConfigurations = (m1.darwinConfigurations or {}) // (m2.darwinConfigurations or {});
        homeConfigurations = (m1.homeConfigurations or {}) // (m2.homeConfigurations or {});
      };

      machineDefs = machines: builtins.foldl' mergeDefs {} machines;
    in
      machineDefs [
        (macConfig {
          user = "joelmccracken"; hostname = "glamdring"; system = "x86_64-darwin"; home = "/Users/joelmccracken";
        })

        (macConfig {
          user = "runner"; hostname = "ci-macos"; system = "x86_64-darwin"; home = "/Users/runner";
        })

        (linuxConfig {
          user = "joel"; hostname = "belthronding"; system = "x86_64-linux"; home = "/home/joel";
        })

        (linuxConfig {
          user = "runner"; hostname = "ci-ubuntu"; system = "x86_64-linux"; home = "/home/runner";
        })
      ];
}
