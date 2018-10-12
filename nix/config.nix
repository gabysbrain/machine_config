#let 
  #zshrc = import ./zsh-config.nix;
#in
{
  allowUnfree = true;
  #packageOverrides = pkgs: rec {
    #zsh = pkgs.stdenv.lib.overrideDerivation pkgs.zsh (oldAttrs: {
    #zsh = nixos.programs.zsh.override {
  #programs.zsh = {
      #interactiveShellInit = ''
        #cat << EOF > $HOME/.zshrc
          #. ${import ./zsh-config.nix}
        #EOF
      #'';
    #});
    #};
  #};
}
