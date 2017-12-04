/*{ stdenv, fetchFromGitHub, pythonPackages,*/
  /*libxml2, libxslt, zlib }:*/
with import <nixpkgs> {};

#python35Packages.buildPythonPackage rec {
let pypkg = python35Packages;
    clicklog = pypkg.buildPythonPackage rec {
      version = "0.2.1";
      name = "click-log-${version}";

      src = pkgs.fetchurl {
        url = "mirror://pypi/c/click-log/${name}.tar.gz";
        sha256 = "1r1x85023cslb2pwldd089jjk573mk3w78cnashs77wrx7yz8fj9";
      };

      propagatedBuildInputs = [ pypkg.click ];

      meta = {
        homepage = https://github.com/click-contrib/click-log/;
        description = "Logging integration for Click";
        license = stdenv.lib.licenses.mit;
        maintainers = with maintainers; [ ];
      };
    };
in
pypkg.buildPythonApplication rec {
  version = "0.16.3";
  name = "vdirsyncer-${version}";
  namePrefix = "";

  src = pkgs.fetchurl {
    url = "https://pypi.python.org/packages/89/61/c1e7d46487f6ed33fbd7c1ae8c55887b07469e47c700e3f4dc9a6627dcbd/vdirsyncer-0.16.3.tar.gz";
    sha256 = "0dpwbfi97ksijqng191659m8k0v215y8ld95w8gb126m4m96qpzw";
  };

  doCheck = false;

  nativeBuildInputs = [ libxml2 libxslt zlib ];
  propagatedBuildInputs = [ 
    pypkg.atomicwrites 
    pypkg.click
    pypkg.click-threading 
    clicklog 
    pypkg.requests_toolbelt
  ];

  meta = {
    description = "Synchronize calendars and contacts";
    homepage = https://vdirsyncer.pimutils.org;
    license = stdenv.lib.licenses.bsd3;
    #maintainers = [ stdenv.lib.maintainers.garbas ];
  };
}
