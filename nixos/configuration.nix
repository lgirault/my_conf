# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/efi";
  };

  networking = {
    hostName = "firebird"; # Define your hostname.
    extraHosts = "127.0.0.1 firebird"; 
  };
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Paris";

  nix.useSandbox = true;
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
   nixpkgs.config.allowUnfree = true;
   environment.systemPackages = with pkgs; [
     psmisc # contains killall
     wget plymouth 
     htop tree

     tmux unzip

     gnumake
     gmp
     gcc
     vim atom
     #medias
     gnome3.eog zathura vlc 
     transmission_gtk
     git  
     calibre
     zsh
     sbt

     firefox
     #jdk
     #jetbrains.jdk # openjdk fork that better supports jetbrains
     pavucontrol
     xscreensaver     
     gmrun
     dmenu
     numlockx 
     
     gnome3.gnome-screenshot 
  
     redshift geoclue2 
     ghc 
     stack
     haskellPackages.xmobar
     zlib
     udiskie
     
     xorg.xmodmap 
   ];
  programs.zsh.enable = true;
  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    videoDrivers = ["nvidia"];
    windowManager.xmonad = {
    	enable = true;
        enableContribAndExtras = true;
    };
    displayManager.lightdm = {
        enable = true;
        #defaultUser = "lorilan";
    };
    layout = "fr";
    xkbVariant = "bepo";
  };
  i18n.consoleUseXkbConfig = true;
  hardware.opengl.driSupport32Bit = true;
  sound = {
    enable = true;
    mediaKeys = {
      enable = true;
      volumeStep = "5";
    };
  };
   
  #hardware.pulseaudio.enable = true;

  virtualisation.docker.enable = true;
 
  # Define a user account. Don't forget to set a password with ‘passwd’.
  #users.mutableUsers = false;
  users = {
    users.lorilan = {
     createHome = true;
     home = "/home/lorilan/";
     isNormalUser = true;
     extraGroups = [ "wheel" "plugdev" "docker" ];
     uid = 1000;
     shell = pkgs.zsh;
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";

}
