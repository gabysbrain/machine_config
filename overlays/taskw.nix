
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
          sha256 = "19h2h6jdcyxjvbbn1lwqjb3lkabfqvljcjyahak0xwch3rzxlqcp";
        };
      });
    };
  };
}

