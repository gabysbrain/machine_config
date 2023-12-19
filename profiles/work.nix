{ pkgs, config, ... }:

{
  homeage.file.raicoon-envvars.source = ../secrets/raicoon-envvars.age;
  programs = {
    git = {
      includes = [
        { contents = {
            user = {
              name = "Tom Torsney-Weir";
              email = "t.torsney@raicoon.com";
            };
          };
          condition = "gitdir:~/raicoon/";
        }
      ];
    };
    k9s = {
      enable = true;
      plugin = {
        # Defines a plugin to provide a `ctrl-l` shortcut to
        # tail the logs while in pod view.
        fred = {
          shortCut = "Ctrl-L";
          description = "Pod logs";
          scopes = [ "po" ];
          command = "kubectl";
          background = false;
          args = [
            "logs"
            "-f"
            "$NAME"
            "-n"
            "$NAMESPACE"
            "--context"
            "$CLUSTER"
          ];
        };
      };
      skin = 
        let
          foreground="#DADEE8";
          background="#30343F";
          current_line="#383D4A";
          selection="#D9DEE8";
          comment="#8891A7";
          cyan="#88C0D0";
          green="#A3BE8C";
          orange="#D08770";
          blue="#81A1C1";
          magenta="#B48EAD";
          red="#BF616A";
          yellow="#EBCB8B";
        in {
          k9s = {
            body = {
              fgColor = foreground;
              bgColor = "default";
              logoColor = magenta;
            };
            # Command prompt styles
            prompt = {
              fgColor = foreground;
              bgColor = background;
              suggestColor = orange;
            };
            # ClusterInfoView styles.
            info = {
              fgColor = blue;
              sectionColor = foreground;
            };
            # Dialog styles.
            dialog = {
              fgColor = foreground;
              bgColor = "default";
              buttonFgColor = foreground;
              buttonBgColor = magenta;
              buttonFocusFgColor = yellow;
              buttonFocusBgColor = blue;
              labelFgColor = orange;
              fieldFgColor = foreground;
            };
            frame = {
              # Borders styles.
              border = {
                fgColor = selection;
                focusColor = current_line;
              };
              menu = {
                fgColor = foreground;
                keyColor = blue;
                # Used for favorite namespaces
                numKeyColor = blue;
              };
              # CrumbView attributes for history navigation.
              crumbs = {
                fgColor = foreground;
                bgColor = current_line;
                activeColor = current_line;
              };
              # Resource status and update styles
              status = {
                newColor = cyan;
                modifyColor = magenta;
                addColor = green;
                errorColor = red;
                highlightColor = orange;
                killColor = comment;
                completedColor = comment;
              };
              # Border title styles.
              title = {
                fgColor = foreground;
                bgColor = current_line;
                highlightColor = orange;
                counterColor = magenta;
                filterColor = blue;
              };
            };
            views = {
              # Charts skins...
              charts = {
                bgColor = "default";
                defaultDialColors = [ magenta red ];
                defaultChartColors = [ magenta red ];
              };
              # TableView attributes.
              table = {
                fgColor = foreground;
                bgColor = "default";
                # Header row styles.
                header = {
                  fgColor = foreground;
                  bgColor = "default";
                  sorterColor = cyan;
                };
              };
              # Xray view attributes.
              xray = {
                fgColor = foreground;
                bgColor = "default";
                cursorColor = current_line;
                graphicColor = magenta;
                showIcons = false;
              };
              # YAML info styles.
              yaml = {
                keyColor = blue;
                colonColor = magenta;
                valueColor = foreground;
              };
              # Logs styles.
              logs = {
                fgColor = foreground;
                bgColor = "default";
                indicator = {
                  fgColor = foreground;
                  bgColor = magenta;
                  toggleOnColor = magenta;
                  toggleOffColor = blue;
                };
              };
              help = {
                fgColor = foreground;
                bgColor = background;
                indicator = {
                  fgColor = red;
                };
              };
            };
          };
        };
    };
    zsh.shellAliases = {
      k9s_staging = "k9s --kubeconfig ~/.kube/tom-config-staging.yaml";
      k9s_prod = "k9s --kubeconfig ~/.kube/tom-config-production.yaml";
    };
    zsh.initExtra = ''
      # work envvars for services
      source "${config.homeage.file.raicoon-envvars.path}"
    '';
  };

  home.packages = with pkgs; [
    slack
    bump2version
    dbeaver
    magic-wormhole
    iredis
    rclone
    teams-for-linux
    thunderbird

    # kubernetes
    openlens
    kubectl
  ];
}
