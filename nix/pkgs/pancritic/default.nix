{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

let 
  pypkgs = pkgs.python3Packages;
in 
pypkgs.buildPythonApplication rec {
  pname = "pancritic";
  version = "0.3.1";

  src = pypkgs.fetchPypi {
    inherit pname version;
    sha256 = "1h74x42h8xqgxrrpmgalk5b3wis5njjmyrv3av8ah82wg10p3bhj";
  };

  propagatedBuildInputs = [
    pypkgs.markdown
    pypkgs.markdown2
    pypkgs.pypandoc
  ];

  doCheck = false;
}
