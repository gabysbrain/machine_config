{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.buildGoModule rec {
  pname = "notmuch-imap-tag-mover";
  version = "2021-12-20";

  src = pkgs.fetchFromGitHub {
    owner = "gabysbrain";
    repo = "${pname}";
    rev = "a1c1b53f8b9c147243f928b0cddf3b83ba37ae88";
    sha256 = "08xs8r392wdk4wwd9rr9h4z7xvfdni78h5lnm619qx4b34zand92";
  };

  # will need a bump when deps change
  vendorSha256 = "11n1wsym39x89bf0pgyq69nbm71pf23jy3wsx21vvyppaygsp1b6";
  runVend = true;

  buildInputs = [ pkgs.notmuch ];
}
