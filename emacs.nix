# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
{ config, lib, pkgs, ... }:

{
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/9fe0402668dfb9d53352aeaec87d5e2a629260f5.tar.gz; # version hash instead of master
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
  ];

  files = {
    ".emacs.d/init.el".text = ''
    ;; loads doom from nix store
    (load "default.el")
    '';
  };
}
