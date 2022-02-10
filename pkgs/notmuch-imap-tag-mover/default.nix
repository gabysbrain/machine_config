{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.buildGoModule rec {
  pname = "notmuch-imap-tag-mover";
  version = "2022-02-10";

  src = pkgs.fetchFromGitHub {
    owner = "gabysbrain";
    repo = "${pname}";
    rev = "c897fa731fd5cb8409c79818e2c9dd5d7bbba1e5";
    sha256 = "HnNs3uXtpiu2BE7udtT97NLdnx/wbEXNYA+uB4aEA7s=";
  };

  # will need a bump when deps change
  vendorSha256 = "11n1wsym39x89bf0pgyq69nbm71pf23jy3wsx21vvyppaygsp1b6";
  runVend = true;

  buildInputs = [ pkgs.notmuch ];
}
