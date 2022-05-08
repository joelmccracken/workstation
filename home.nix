{ config, pkgs, lib, nix-doom-emacs, ... }:
  {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "joel";
    home.homeDirectory = "/Users/joel";

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "22.05";

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;
    home.packages = [
      # nix-doom-emacs.hmModule {
      #   doomPrivateDir = ./doom.d;
      # }
    ];
    home.file = {
      ".emacs.d/init.el".text = ''
          ;; loads doom from nix store
          (load "default.el")
          (let
            ((this-machine (expand-file-name "~/workstation/this-machine.el")))
            (when (file-exists-p this-machine)
              (load this-machine)))
          ;; (when (file-exists-p "./this-machine.el" ))
          (require 'nix-mode) ;; not sure why I suddenly need to do this
           '';
    };
  }
