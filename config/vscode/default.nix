{pkgs,...}:

{
  programs.vscode = {
    enable = true;

    extensions = (with pkgs.vscode-extensions; [
      vscodevim.vim

      bbenoist.nix
      golang.go
    ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
      publisher = "arcticicestudio";
      name = "nord-visual-studio-code";
      version = "0.18.0";
      sha256 = "1caism5qa62pgyggxyary2nv9xyqyym62x02kzxdar5n3xwsk3jj";
    } {
      name = "hg";
      publisher = "mrcrowl";
      version = "1.7.1";
      sha256 = "1qlnc77qhr992yk5zzxvagh157lfg47ax6k1zl9smd07c1jg7l80";
    } {
      name = "language-julia";
      publisher = "julialang";
      version = "1.3.33";
      sha256 = "11nag14jycj1dzxa4w97c0y160hlwaxghrjvcm1aq1q5k5k1gd4d";
    }];
  };
}
