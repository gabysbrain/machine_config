{lib, pkgs, autoPatchelfHook, ...}:
#with import <nixpkgs> {};
with pkgs.python3Packages;

buildPythonPackage rec {
  pname = "problog";
  version = "2.2.4";

  src = fetchPypi {
    inherit pname version;
    extension = "tar.gz";
    sha256 = "sha256-N96os6CYgNmAURfVR2J9B8n6DrssURYytwdUvIlLa80=";
  };

  # Required at running time
  buildInputs = with pkgs; [
    #glibc
    gcc-unwrapped
    #tox
  ];

  nativeBuildInputs = [
    autoPatchelfHook
  ];

  propagatedBuildInputs = [ ];

  doCheck = false;

  meta = {
    description = "ProbLog is a Probabilistic Logic Programming Language for logic programs with probabilities";
    homepage = "https://dtai.cs.kuleuven.be/problog/";
    license = lib.licenses.asl20;
  };
}
