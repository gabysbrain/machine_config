{lib, pkgs, autoPatchelfHook, ...}:
#with import <nixpkgs> {};
with pkgs.python3Packages;

buildPythonPackage rec {
  pname = "problog";
  version = "2.1.0.42";

  src = fetchPypi {
    inherit pname version;
    extension = "tar.gz";
    sha256 = "bc1fad5d51304f3f071589ee280e56ef70d13c97100c006d84ee14450fe3ef51";
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
