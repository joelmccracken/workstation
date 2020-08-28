# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
ctx:
let
  # Load specific nixpkgs reference via niv
  sources = import ./nix/sources.nix;
  nixpkgs = sources.nixpkgs;
  pkgs = import nixpkgs {};
  ctx2 = ctx // {pkgs = pkgs;};
  this-machine = import ./this-machine.nix;
  dotfiles = (import ./dotfiles.nix) ctx2;
  emacs = (import ./emacs.nix) ctx2;
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = this-machine.username;
  home.homeDirectory = this-machine.homeDirectory;

  home.file = dotfiles // emacs.files;

  home.packages = [
    pkgs.ghcid
    pkgs.jq
    pkgs.jl
    emacs.doom-emacs
    pkgs.stack
    pkgs.bitwarden-cli
    # make this work; how??
    # want to be able to access the niv binary
    sources.niv
  ] ++ emacs.packages;

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
