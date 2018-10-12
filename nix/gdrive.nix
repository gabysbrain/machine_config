with import <nixpkgs> {};

buildGoPackage rec {
  rev = "8f299578a2f0222e4b11e1a03692f7aba69c0977";
  name = "gdrive-${rev}";
  goPackagePath = "github.com/prasmussen/gdrive";
  src = fetchFromGitHub {
    inherit rev;
    owner = "prasmussen";
    repo = "gdrive";
    sha256 = "1v2gnvhxp94waam3c13v2hk94g03an5837g8pfzmyxjb4c2rkj3m";
  };
}

