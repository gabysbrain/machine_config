{
  description = "My system config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-21.11";
    agenix.url = "github:ryantm/agenix";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, agenix }:
    let
      extra-pkgs-overlay = final: prev: {
        unstable = import nixpkgs-unstable {
          system=prev.system;
          config.allowUnfree = true;
        };
        inherit agenix;
      };
    in {

    nixosConfigurations = {
      philadelphia = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ 
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ extra-pkgs-overlay ]; })
          ./portege-configuration.nix 
          agenix.nixosModules.age
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
              ];
            };
          }
        ];
      };
      katana = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ 
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ extra-pkgs-overlay ]; })
          ./katana-configuration.nix 
          agenix.nixosModules.age
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
              ];
            };
          }
        ];
      };
    };

  };
}
