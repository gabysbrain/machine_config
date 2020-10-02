{ stdenv, python38Packages }:

let 
  pypkgs = python38Packages;
  xdg = pypkgs.buildPythonPackage rec {
    pname = "xdg";
    version = "4.0.1";

    src = pypkgs.fetchPypi {
      inherit pname version;
      sha256 = "c939c99def394cbaf765a3ee55efd6ea7e4c5eaed8d9ebc2d03af84ba35dec57";
    };
  };
in
pypkgs.buildPythonPackage rec {
  pname = "goobook";
  version = "3.5";

  src = pypkgs.fetchPypi {
    inherit pname version;
    sha256 = "4992e1e1a86c82c96651e483b6da83c83b83bc317250c2732daef13754f5aee6";
  };

  propagatedBuildInputs = with pypkgs; [ 
    google_api_python_client 
    simplejson 
    oauth2client 
    xdg
  ];

  meta = with stdenv.lib; {
    description = "Search your google contacts from the command-line or mutt";
    homepage    = https://pypi.python.org/pypi/goobook;
    license     = licenses.gpl3;
    maintainers = with maintainers; [ primeos ];
    platforms   = platforms.unix;
  };
}
