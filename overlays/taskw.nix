
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
          sha256 = "12ryycz7hzz0v84p6fskgx7l9gf036j3af4xfa55hlz62c9wjq4g";
        };
      });
    };
  };
}

