/*{ stdenv, fetchFromGitHub, pythonPackages,*/
  /*libxml2, libxslt, zlib }:*/
with import <nixpkgs> {};

pkgs.stdenv.mkDerivation rec {
  pname = "bawag2ynab";
  version = "0.1.0";
  name = "${pname}-${version}";

  unpackPhase = "true";

  #propagatedBuildInputs = [ 
  buildInputs = [
    (pkgs.python27.withPackages (pythonPackages: with pythonPackages; [
      selenium
    ]))
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp ${./bawag2ynab.py} $out/bin/bawag2ynab
    chmod +x $out/bin/bawag2ynab
  '';

  meta = {
    description = "Converter from BAWAG bank csv to YNAB's csv format";
  };
}
