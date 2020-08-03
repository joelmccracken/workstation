{ config, lib, pkgs, ... }:

{
  packages = epkgs: [
    epkgs.nix-mode
    epkgs.magit
    epkgs.vterm
  ];

  files = {
    ".emacs.d".text = ''
    (load (load "~/.emacs.d/doom/init.el"))
    '';
  };
}
