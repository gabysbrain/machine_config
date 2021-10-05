{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.buildGoModule rec {
  pname = "notmuch-imap-tag-mover";
  version = "2021-10-05";

  src = pkgs.fetchFromGitHub {
    owner = "gabysbrain";
    repo = "${pname}";
    rev = "e4906b6940711db7241d61ca44627e3696881ccd";
    sha256 = "1ny25mjv2i26wnjlhcadg7a98d8l5m0wy9jim8abmk5qmsziv3j2";
  };

  # will need a bump when deps change
  vendorSha256 = "11n1wsym39x89bf0pgyq69nbm71pf23jy3wsx21vvyppaygsp1b6";
  runVend = true;

  buildInputs = [ pkgs.notmuch ];
}
