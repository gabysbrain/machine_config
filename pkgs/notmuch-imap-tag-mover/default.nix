{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.buildGoModule rec {
  pname = "notmuch-imap-tag-mover";
  version = "2023-03-09";

  src = pkgs.fetchFromGitHub {
    owner = "gabysbrain";
    repo = "${pname}";
    rev = "bf7d1b20b0e6e5f7e95ee5b0ee3d039a70df83eb";
    sha256 = "sha256-m/V8Fb43/yEYqXVJ0Pgk5o72eFgqbMDgstihA8wrmkw=";
  };

  # will need a bump when deps change
  vendorSha256 = "1i/NctOsgB324C7GijpJE46JwLEJMUHLN1XghsyI1Wo=";
  proxyVendor = true;

  buildInputs = [ pkgs.notmuch ];
}
