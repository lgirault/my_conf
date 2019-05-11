# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/efi";
  };


  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Paris";

  nix.useSandbox = false;
  nix.sandboxPaths = [ "/var/run/docker.sock" ];

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
     vim emacs
     #medias
     gnome3.eog zathura vlc
     gitAndTools.gitFull
     calibre
     zsh
     sbt

     vagrant

     firefox
     #jdk
     jetbrains.jdk # openjdk fork that better supports jetbrains
     #alsaUtils
     #pavucontrol
     xscreensaver
     dmenu
     numlockx

     gnome3.gnome-screenshot

     keychain
     feh
     redshift geoclue2
     ghc
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

  services.logind.extraConfig = "HandleSuspendKey=ignore";
  
  services.bloop.install = true;

  i18n.consoleUseXkbConfig = true;
  hardware.opengl.driSupport32Bit = true;
  sound = {
    enable = true;
    mediaKeys = {
      enable = true;
      volumeStep = "5";
    };
  };
  
  networking = {
    hostName = "firebird"; # Define your hostname.
    extraHosts = "127.0.0.1 firebird";
  };
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  virtualisation = {
  	docker.enable = true;
  	virtualbox.host.enable = true;
        virtualbox.host.enableHardening = false;	
  };
  
  # Define a user account. Don't forget to set a password with ‘passwd’.
  #users.mutableUsers = false;
  users = {
    extraUsers = 
    let
    	nixblds = lib.foldl addNixbldToDockerGroup {} (lib.range 1 config.nix.nrBuildUsers);
    	addNixbldToDockerGroup = acc: idx: acc // { "nixbld${builtins.toString idx}".extraGroups = [ "nixbld" "docker" ]; };
    in nixblds;

    users.lorilan = {
     createHome = true;
     home = "/home/lorilan/";
     isNormalUser = true;
     extraGroups = [ "wheel" "plugdev" "docker" "vboxusers" ];
     uid = 1000;
     shell = pkgs.zsh;
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";

}
