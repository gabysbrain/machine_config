
final: prev: {
  python38 = prev.python38.override {
    packageOverrides = python-final: python-prev: {
      taskw = python-prev.taskw.overrideAttrs (oldAttrs: {
        version = "develop";
        src = prev.fetchFromGitHub {
          owner = "ralphbean";
          repo = "taskw";
          rev = "develop";
          #sha256 = "0000000000000000000000000000000000000000000000000000";
          sha256 = "0xjv8ivi7037j6my76dk8l4xg6nazw41hp9cc0k74dhk42cx0r3h";
        };
      });
    };
  };
}

