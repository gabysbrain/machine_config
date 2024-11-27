{
  description = "My system config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    homeage.url = "github:jordanisaacs/homeage";
    homeage.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { 
    self, 
    nixpkgs, 
    nixpkgs-unstable, 
    nixos-wsl,
    home-manager, 
    agenix, 
    homeage 
  }@inputs:
    let
      extra-pkgs-overlay = final: prev: {
        unstable = import nixpkgs-unstable {
          system=prev.system;
          config.allowUnfree = true; # FIXME: doesn't work with nixos-install
        };
        inherit agenix;
      };
      my-overrides = import overlays/default.nix;
    in {

    # FIXME: tom user should be created and have zsh shell
    nixosConfigurations = {
      work = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ 
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ my-overrides extra-pkgs-overlay ]; })
          nixos-wsl.nixosModules.default
          ./work-configuration.nix 
          #agenix.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.soy9a = { pkgs, nixosConfig, ... }: {
              home.stateVersion = "24.05";

              imports = [
                ./home-config/common.nix
                ./home-config/desktop.nix
                ./profiles/dev.nix
                ./profiles/work.nix

                # FIXME: not sure why this breaks in home-config/common...
                homeage.homeManagerModules.homeage
              ];
            };
          }
        ];
      };
      philadelphia = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ 
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ my-overrides extra-pkgs-overlay ]; })
          ./portege-configuration.nix 
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.tom = { pkgs, nixosConfig, ... }: {
              home.stateVersion = "20.09";

              imports = [
                ./home-config/common.nix
                ./home-config/desktop.nix
                ./profiles/dev.nix
                ./profiles/writing.nix

                # FIXME: not sure why this breaks in home-config/common...
                homeage.homeManagerModules.homeage
              ];
            };
          }
        ];
      };
      katana = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ 
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ my-overrides extra-pkgs-overlay ]; })
          ./katana-configuration.nix 
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.tom = { pkgs, nixosConfig, ... }: {
              home.stateVersion = "20.09";
              imports = [
                ./home-config/common.nix
                ./home-config/desktop.nix
                ./profiles/dev.nix
                ./profiles/games.nix
                ./profiles/writing.nix

                # FIXME: not sure why this breaks in home-config/common...
                homeage.homeManagerModules.homeage
              ];

              # adjust terminal for high dpi screen
              programs.kitty.font.size = pkgs.lib.mkForce 14;
            };
          }
        ];
      };
    };

  };
}
