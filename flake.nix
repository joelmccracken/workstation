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
    };
  };

  outputs = inputs@{ self, darwin, nixpkgs, darwin-nixpkgs, home-manager, darwin-home-manager, nix-doom-emacs,
    ... }:
    let
      emacsPackagesOverlay = self: super: {

        transient = self.callPackage({melpaBuild, fetchFromGitHub, lib, emacs, compat, pkgs, dash, git-commit, transient, with-editor}:
          melpaBuild rec {
            pname = "transient";
            version = "0.3.6";
            src = fetchFromGitHub {
              owner = "magit";
              repo = "transient";
              rev = "51e833e5152e9fdcdc1dbbf34ad2d4905bde1f69";
              sha256 = "sha256-fsNYL6N1ZaTzbsWOlEqClXpVhy21UzaZyyAaj/RvaYI=";
            };
            commit = "51e833e5152e9fdcdc1dbbf34ad2d4905bde1f69";
            # elisp dependencies
            packageRequires = [
              emacs
            ];
            recipe = pkgs.writeText "recipe" ''
              (transient
              :repo "magit/transient"
              :fetcher github
              :files
              ("lisp/*.el"))
            '';
            meta = {
              homepage = "https://elpa.gnu.org/packages/transient.html";
              license = lib.licenses.free;
            };
          }
        ) {};

        magit = self.callPackage({melpaBuild, fetchFromGitHub, lib, emacs, compat, pkgs, dash, git-commit, transient, with-editor}:
          melpaBuild rec {
            pname = "magit";
            version = "3.3.0";
            src = fetchFromGitHub {
              owner = "magit";
              repo = "magit";
              rev = "c1fb53d3de6390961ccd8dfb1cc135383508d0fc";
              sha256 = "sha256-I1k8Lsczs1RuGou0qqrAIQuAXiUyIuQVWHwCIjf4EjI=";
            };
            commit = "c1fb53d3de6390961ccd8dfb1cc135383508d0fc";
            # elisp dependencies
            packageRequires = [
              emacs compat dash git-commit transient with-editor
            ];
            recipe = pkgs.writeText "recipe" ''
              (magit
              :repo "magit/magit"
              :fetcher github
              :files
              ("lisp/*.el"))
            '';
            meta = {
              homepage = "https://elpa.gnu.org/packages/magit.html";
              license = lib.licenses.free;
            };
          }
        ) {};

        compat = self.callPackage({melpaBuild, fetchFromGitHub, lib, emacs, seq, pkgs}:
          melpaBuild rec {
            pname = "compat";
            version = "28.1.2.0";
            src = fetchFromGitHub {
              owner = "emacs-straight";
              repo = "compat";
              rev = "cc1924fd8b3f9b75b26bf93f084ea938c06f9615";
              sha256 = "sha256-ZEQI/qvnUc4GEdPlbRMLDn7Oh4j53Gxy3wEXZFX8Aq4=";
            };
            commit = "cc1924fd8b3f9b75b26bf93f084ea938c06f9615";
            # elisp dependencies
            packageRequires = [
              emacs seq
            ];
            recipe = pkgs.writeText "recipe" ''
              (compat
              :repo "emacs-compat/compat"
              :fetcher github
              :files
              ("*.el"))
            '';
            meta = {
              homepage = "https://elpa.gnu.org/packages/compat.html";
              license = lib.licenses.free;
            };
          }
        ) {};

        consult = self.callPackage({melpaBuild, fetchFromGitHub, lib, emacs, compat, pkgs}:
          melpaBuild rec {
            pname = "consult";
            version = "0.18";
            src = fetchFromGitHub {
              owner = "minad";
              repo = "consult";
              rev = "6319aec3513cd587a8817269bc32c8283d419710";
              sha256 = "sha256-0ppRvCvk93wzKMU6814LSGV5PlR84zeIvSUcvg5+TLY=";
            };
            commit = "6319aec3513cd587a8817269bc32c8283d419710";
            # elisp dependencies
            packageRequires = [
              emacs compat
            ];
            recipe = pkgs.writeText "recipe" ''
              (consult
              :repo "minad/consult"
              :fetcher github
              :files
              ("*.el"))
            '';
            meta = {
              homepage = "https://elpa.gnu.org/packages/consult.html";
              license = lib.licenses.free;
            };
          }
        ) {};

        vertico = self.callPackage({melpaBuild, fetchFromGitHub, lib, emacs, compat, pkgs}:
          melpaBuild rec {
            pname = "vertico";
            version = "0.25";
            src = fetchFromGitHub {
              owner = "minad";
              repo = "vertico";
              rev = "2ad46196653b8a873adf11aee949d621af8ff6bc";
              sha256 = "sha256-R7MYPn9Co+MT5CQ8+D5F5JBe92GjxEY0v14aPgBLFlg=";
            };
            commit = "2ad46196653b8a873adf11aee949d621af8ff6bc";

            # elisp dependencies
            packageRequires = [
              emacs compat
            ];
            recipe = pkgs.writeText "recipe" ''
              (vertico
              :repo "minad/vertico"
              :fetcher github
              :files
              ("*.el"
              "extensions/*.el"))
            '';
            meta = {
              homepage = "https://elpa.gnu.org/packages/vertico.html";
              license = lib.licenses.free;
            };
          }
        ) {};

        embark = self.callPackage({melpaBuild, fetchFromGitHub, lib, emacs, compat, pkgs}:
          melpaBuild rec {
            pname = "embark";
            version = "0.17";
            src = pkgs.fetchFromGitHub {
              owner = "oantolin";
              repo = "embark";
              rev = "5d0459d27aa7cf738b5af36cf862723a62bef955";
              sha256 = "sha256-7U94GRmUA+UdqvwSBSEGSwHSpfqaaiKghqg4P4gn85c=";
            };
            commit = "5d0459d27aa7cf738b5af36cf862723a62bef955";
            packageRequires = [
              emacs compat
            ];
            recipe = pkgs.writeText "recipe" ''
              (embark
              :repo "oantolin/embark"
              :fetcher github
              :files ("embark.el" "embark-org.el" "embark.texi"))
            '';
            meta = {
              homepage = "https://elpa.gnu.org/packages/embark.html";
              license = lib.licenses.free;
            };
          }) {};
        embark-consult = self.callPackage({melpaBuild, fetchFromGitHub, lib, emacs, compat, pkgs}:
          melpaBuild rec {
            pname = "embark-consult";
            version = "0.17";
            src = pkgs.fetchFromGitHub {
              owner = "oantolin";
              repo = "embark";
              rev = "5d0459d27aa7cf738b5af36cf862723a62bef955";
              sha256 = "sha256-7U94GRmUA+UdqvwSBSEGSwHSpfqaaiKghqg4P4gn85c=";
            };
            commit = "5d0459d27aa7cf738b5af36cf862723a62bef955";
            packageRequires = [
              emacs compat
            ];
            recipe = pkgs.writeText "recipe" ''
              (embark-consult
              :repo "oantolin/embark"
              :fetcher github
              :files
              ("embark-consult.el"))
            '';
            meta = {
              homepage = "https://elpa.gnu.org/packages/embark-consult.html";
              license = lib.licenses.free;
            };
          }) {};

        orderless = self.callPackage({melpaBuild, fetchFromGitHub, lib, emacs, compat, pkgs}:
          melpaBuild rec {
            pname = "orderless";
            version = "0.7";
            src = pkgs.fetchFromGitHub {
              owner = "oantolin";
              repo = "orderless";
              rev = "8b9af2796fa0eb87eea4140bc08d16880a493803";
              sha256 = "sha256-yHJYdyEUM6rQPVjCXnVFIHwHoMgRjEHBOcwbTmZ6fW0=";
            };
            commit = "8b9af2796fa0eb87eea4140bc08d16880a493803";
            packageRequires = [
              emacs
            ];
            recipe = pkgs.writeText "recipe" ''
              (orderless
              :repo "oantolin/orderless"
              :fetcher github
              :files
              ("*.el"))
            '';
            meta = {
              homepage = "https://elpa.gnu.org/packages/orderless.html";
              license = lib.licenses.free;
            };
          }) {};
      };
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
              # inherit emacsPackagesOverlay;
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

      # used for testing/development purposes, makes it easier to experiment
      emacsPackagesOverlay = nixpkgs.legacyPackages."${builtins.currentSystem}".emacsPackages.overrideScope' emacsPackagesOverlay;
    };
}

