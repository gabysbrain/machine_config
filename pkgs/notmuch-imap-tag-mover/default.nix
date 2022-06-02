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
  vendorSha256 = "1i/NctOsgB324C7GijpJE46JwLEJMUHLN1XghsyI1Wo=";
  proxyVendor = true;

  buildInputs = [ pkgs.notmuch ];
}
