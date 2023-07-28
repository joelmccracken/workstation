{
  description = "Joel's systems";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs.url = "git+file:///Users/joel/Projects/nixpkgs";
    darwin-nixpkgs = {
      url = "git+file:///Users/joel/Projects/nixpkgs";
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
      url = "git+file:///Users/joel/Projects/nix-doom-emacs";
      inputs = {
        nixpkgs.follows = "nixpkgs";


        doom-emacs.url = "git+file:///Users/joel/Projects/doomemacs";
        nix-straight.url = "git+file:///Users/joel/Projects/nix-straight.el";
      };
    };
  };

  outputs = inputs@{ self, darwin, nixpkgs, darwin-nixpkgs, home-manager, darwin-home-manager, nix-doom-emacs,
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

            programs.doom-emacs = {
              enable = true;
              doomPrivateDir = ./dotfiles/doom.d;
              extraConfig = ''
                (add-to-list 'exec-path "~/.nix-profile/bin/")
              '';

              # emacsPackagesOverlay = self: super: {
              #   embark = self.trivialBuild {
              #     pname = "embark";
              #     ename = "embark";
              #     version = "5d0459d27aa7cf738b5af36cf862723a62bef955";
              #     packageRequires = [ super.compat super.emacs ];
              #     src = pkgs.fetchFromGitHub {
              #       owner = "oantolin";
              #       repo = "embark";
              #       rev = "5d0459d27aa7cf738b5af36cf862723a62bef955";
              #       sha256 = "sha256-7U94GRmUA+UdqvwSBSEGSwHSpfqaaiKghqg4P4gn85c=";
              #     };
              #     meta = {
              #       homepage = "https://elpa.gnu.org/packages/embark.html";
              #       license = lib.licenses.free;
              #     };

              #   };
              #   # https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/modules/completion/vertico/packages.el#L4C18-L7
              #   vertico = self.trivialBuild {
              #     pname = "vertico";
              #     ename = "vertico";
              #     version = "2ad46196653b8a873adf11aee949d621af8ff6bc";
              #     packageRequires = [ super.compat super.emacs ];
              #     src = pkgs.fetchFromGitHub {
              #       owner = "minad";
              #       repo = "vertico";
              #       rev = "2ad46196653b8a873adf11aee949d621af8ff6bc";
              #       sha256 = "sha256-8vsNZKSWY6AcLs/a8/b9tjmkF2LEeRSAOvsdiWq+cAc=";
              #     };
              #     meta = {
              #       homepage = "https://elpa.gnu.org/packages/vertico.html";
              #       license = lib.licenses.free;
              #     };

              #   };

              #   # vertico = self.melpaBuild {
              #   #   pname = "vertico";

              #   #   # inherit (haskellPackages.ghc-mod) version src;
              #   #   version = "2ad46196653b8a873adf11aee949d621af8ff6bc";

              #   #   src = pkgs.fetchFromGitHub {
              #   #     owner = "minad";
              #   #     repo = "vertico";
              #   #     rev = "2ad46196653b8a873adf11aee949d621af8ff6bc";
              #   #     sha256 = "sha256-8vsNZKSWY6AcLs/a8/b9tjmkF2LEeRSAOvsdiWq+cAc=";
              #   #   };

              #   #   packageRequires = [ super.compat super.emacs ];

              #   #   # propagatedUserEnvPkgs = [ haskellPackages.ghc-mod ];

              #   #   recipe = pkgs.writeText "recipe" ''
              #   #       (vertico :repo "minad/vertico" :fetcher github :files ("*.el" "extensions/*.el"))
              #   #   '';

              #   #   fileSpecs = [ "*.el" "extensions/*.el" ];

              #   #   meta = {
              #   #     homepage = "https://elpa.gnu.org/packages/vertico.html";
              #   #     license = lib.licenses.free;
              #   #   };
              #   # };
              # }
              # ;
            };

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
            nix-doom-emacs.hmModule
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
            nix-doom-emacs.hmModule
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
    in
    {
      darwinConfigurations."glamdring" = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          (nix-darwin-config { user = "joel"; hostname = "glamdring"; })
        ];
      };
      darwinConfigurations."ci-macos" = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          (nix-darwin-config { user = "runner"; hostname = "ci-macos"; })
        ];
      };
      homeConfigurations.glamdring.joel = darwin-home-config {
        user = "joel"; home = "/Users/joel";
      };
      homeConfigurations."ci-macos".runner = darwin-home-config {
        user = "runner"; home = "/Users/runner";
      };
      homeConfigurations."ci-ubuntu".runner = linux-home-config{
        user = "runner"; home = "/home/runner";

      };
    };
}
