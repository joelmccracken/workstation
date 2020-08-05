{ config, lib, pkgs, ... }:

{
  packages = epkgs: [
    epkgs.nix-mode
    epkgs.magit
    epkgs.vterm
  ];

  files = {
    ".emacs.d/init.el".text = ''
    (load "~/.emacs.d/doom/init.el")
    '';
  };
}
