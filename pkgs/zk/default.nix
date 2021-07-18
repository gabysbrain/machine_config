{ mkDerivation, aeson, base, bytestring, data-default, directory
, filepath, hpack, hspec, lib, mtl, optparse-applicative
, raw-strings-qq, system-filepath, temporary, text, time, turtle
, yaml
}:
mkDerivation {
  pname = "zk";
  version = "0.1.0.0";
  src = /home/tom/Projects/zk;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring data-default directory filepath mtl
    optparse-applicative raw-strings-qq system-filepath temporary text
    time turtle yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring data-default directory filepath mtl
    optparse-applicative raw-strings-qq system-filepath temporary text
    time turtle yaml
  ];
  testHaskellDepends = [
    aeson base bytestring data-default directory filepath hspec mtl
    optparse-applicative raw-strings-qq system-filepath temporary text
    time turtle yaml
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/zk#readme";
  license = lib.licenses.bsd3;
}
