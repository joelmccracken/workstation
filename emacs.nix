# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
{ config, lib, pkgs, ... }:
let
  this-machine = import ./this-machine.nix;
in
{
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/4cd7e15082bad25ff6b097f9bb419e50d32f621b.tar.gz; # version hash instead of master
  }) {
    doomPrivateDir = ./doom.d;  # Directory containing your config.el init.el
                                # and packages.el files
    extraPackages = epkgs: [
      epkgs.nix-mode
      epkgs.vterm
    ];
  };

  packages = [
    pkgs.ripgrep # required SPC-* / +default/search-project-for-symbol-at-point
    pkgs.dhall
    pkgs.dhall-json
  ];

  files = {
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
