
{ user, home }:
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
      pkgs.ripgrep
      pkgs.jq
      pkgs.jl
      pkgs.ispell
      pkgs.bitwarden-cli
      pkgs.direnv
      pkgs.mr  # myrepos https://myrepos.branchable.com/install/
      pkgs.graphviz
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
    };

    # workaround; see https://github.com/nix-community/home-manager/issues/3342#issuecomment-1283158398
    manual.manpages.enable = false;

    # home.file = {
    #   ".emacs.d/init.el".text = ''
    #       ;; loads doom from nix store
    #       (load "default.el")
    #       (let
    #         ((this-machine (expand-file-name "~/workstation/this-machine.el")))
    #         (when (file-exists-p this-machine)
    #           (load this-machine)))
    #       ;; (when (file-exists-p "./this-machine.el" ))
    #       (require 'nix-mode) ;; not sure why I suddenly need to do this
    #        '';
    # };
  }
