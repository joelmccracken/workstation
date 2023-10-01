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
    # nix-doom-emacs = {
    #   url = "github:nix-community/nix-doom-emacs";
    # };
  };

  outputs = inputs@{ self, darwin, nixpkgs, darwin-nixpkgs, home-manager, darwin-home-manager, # nix-doom-emacs,
                     # darwin-nix-doom-emacs,
    ... }:

    let
      home-manager-config = { user, home }:
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


      darwin-home-config = user-config:
        let
          system = "x86_64-darwin";
          pkgs = darwin-nixpkgs.legacyPackages.${system};
        in darwin-home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          modules = [
            # nix-doom-emacs.hmModule
            (home-manager-config user-config)
          ];
        };
      linux-home-config = user-config:
        let
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.${system};
        in home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          modules = [
            # nix-doom-emacs.hmModule
            (home-manager-config user-config)
          ];
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

      homeConfigurations.glamdring.joel = darwin-home-config {
        user = "joel"; home = "/Users/joel";
      };

      homeConfigurations."ci-macos".runner = darwin-home-config {
        user = "runner"; home = "/Users/runner";
      };
      homeConfigurations."ci-ubuntu".runner = linux-home-config{
        user = "runner"; home = "/home/runner";
      };


      packages.x86_64-darwin.homeConfigurations.runner = darwin-home-config {
        user = "runner"; home = "/Users/runner";
      };
      packages.x86_64-linux.homeConfigurations.runner = linux-home-config{
        user = "runner"; home = "/home/runner";
      };


    };
}
