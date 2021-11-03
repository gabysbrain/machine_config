{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.buildGoModule rec {
  pname = "notmuch-imap-tag-mover";
  version = "2021-11-03";

  src = pkgs.fetchFromGitHub {
    owner = "gabysbrain";
    repo = "${pname}";
    rev = "91137c2fe839d16bab02e95b18224daca2f4414a";
    sha256 = "0vq8blih7kpgdn9jx3n7gv81cgkz5jrpc10fgd8sb1f3lyxif5a0";
  };

  # will need a bump when deps change
  vendorSha256 = "11n1wsym39x89bf0pgyq69nbm71pf23jy3wsx21vvyppaygsp1b6";
  runVend = true;

  buildInputs = [ pkgs.notmuch ];
}
