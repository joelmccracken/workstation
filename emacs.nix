# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
{ config, lib, pkgs, ... }:

{
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/c440f4afe4ff2d38d2beb40d7e4bcfa2496f60c2.tar.gz; # version hash instead of master
  }) {
    doomPrivateDir = ./doom.d;  # Directory containing your config.el init.el
                                # and packages.el files
    extraPackages = epkgs: [
      epkgs.nix-mode
      epkgs.vterm
    ];
  };

  files = {
    ".emacs.d/init.el".text = ''
    ;; loads doom from nix store
    (load "default.el")
    '';
  };
}
