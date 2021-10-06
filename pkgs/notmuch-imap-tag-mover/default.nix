{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.buildGoModule rec {
  pname = "notmuch-imap-tag-mover";
  version = "2021-10-06";

  src = pkgs.fetchFromGitHub {
    owner = "gabysbrain";
    repo = "${pname}";
    rev = "b5618166eac84b193cabedea5d55aa07064ee401";
    sha256 = "0h98hq50x0zanlg1j5ywfdpsw93gxig0g38z58ny9dswkhdv9kn0";
  };

  # will need a bump when deps change
  vendorSha256 = "11n1wsym39x89bf0pgyq69nbm71pf23jy3wsx21vvyppaygsp1b6";
  runVend = true;

  buildInputs = [ pkgs.notmuch ];
}
