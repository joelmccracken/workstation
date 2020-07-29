ctx@{ config, pkgs, ... }:
let
  this-machine = (import ./this-machine.nix) ctx;
  workstation-dir = "${this-machine.homeDirectory}/workstation";
  dotfiles = (import ./dotfiles.nix) ctx;
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = this-machine.username;
  home.homeDirectory = this-machine.homeDirectory;

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.nix-mode
      epkgs.magit
      epkgs.vterm
    ];
  };

  home.file = dotfiles;

  home.packages = [
    pkgs.cmake
    pkgs.libtool
    pkgs.ripgrep
    pkgs.fd
    pkgs.fontconfig
    pkgs.coreutils
    pkgs.ghcid
    pkgs.jq
    pkgs.jl
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";
}
