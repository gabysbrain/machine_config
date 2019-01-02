with import <nixpkgs> {};
#{ stdenv, substituteAll, buildEnv, fetchUrl, pythonPackages }:

stdenv.mkDerivation rec {
  name = "weechat-whatsapp-${version}";
  version = "0.1";

  src = fetchurl {
    url = "https://weechat.org/files/scripts/whatsapp.py";
    sha256 = "1zflw63rh66w42giz6gkzn99fcbg82i0kdlfwxycx3ja3hvi2yrs";
  };
  unpackPhase = "true"; # nothing to unpack

  # can't get nix patching a single file so use this
  patchfile = (substituteAll {
    src = ./libpath.patch;
    env = "${buildEnv {
      name = "weechat-whatsapp-env";
      paths = with pythonPackages; [ websocket_client six ];
    }}/${pythonPackages.python.sitePackages}";
  });

  passthru.scripts = [ "whatsapp.py" ];

  installPhase = ''
    mkdir -p $out/share
    patch $src $patchfile -o $out/share/whatsapp.py
  '';

  meta = with stdenv.lib; {
    homepage = https://weechat.org/scripts/source/whatsapp.py.html/;
    #license = licenses.mit;
    #maintainers = with maintainers; [ ma27 ];
    description = ''
      A WeeChat plugin for whatsapp.
    '';
  };
}
