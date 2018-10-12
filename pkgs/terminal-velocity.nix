/*{ stdenv, fetchFromGitHub, pythonPackages,*/
  /*libxml2, libxslt, zlib }:*/
with import <nixpkgs> {};

let pypkg = python27Packages;
    chardet = pypkg.buildPythonPackage rec {
      version = "2.1.1";
      name = "chardet-${version}";

      src = pkgs.fetchurl {
        url = "mirror://pypi/c/chardet/${name}.tar.gz";
        sha256 = "1l3fmdgcb409qnbf6q3c6qxy0j4n1lfhr5j3ix27nxzcg2819m5h";
      };

      doCheck = false;
    };
    urwid = pypkg.buildPythonPackage rec {
      version = "1.1.1";
      name = "urwid-${version}";

      src = pkgs.fetchurl {
        url = https://pypi.python.org/packages/5c/ea/bc8bea146961e3b7d0deb3b9de04b5d023e0380815caceef3d1965f92e10/urwid-1.1.1.tar.gz;
        sha256 = "0lyqpbkdv45b8gzbvpx99c5a9smmisnw8c9558gqsxp3i1d6jpri";
      };

      doCheck = false;
    };
in
pypkg.buildPythonApplication rec {
  pname = "terminal-velocity";
  version = "0.1.10";
  name = "${pname}-${version}";

  src = pkgs.fetchurl {
    url = https://pypi.python.org/packages/85/16/dc318f00221a7fbe85bc766c575f14a9191516cdfb7dccc9bfe0c9fbd8f9/terminal_velocity-0.1.10.tar.gz;
    sha256 = "004pmblirx4qhl5dz7hfwrassq0pynxrkn6y2yg6ylnn61jhqsxz";
  };

  doCheck = false;

  nativeBuildInputs = [ ];
  propagatedBuildInputs = [ 
    chardet
    urwid
  ];

  meta = {
    description = "Fast note taking app for the linux terminal";
    homepage = https://vhp.github.io/terminal_velocity;
    license = stdenv.lib.licenses.gpl3;
    #maintainers = [ stdenv.lib.maintainers.garbas ];
  };
}
