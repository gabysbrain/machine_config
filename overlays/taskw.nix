
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
          sha256 = "0vgsx3ppmpbqb4zkfj5a4rq07639ifvjbqaiy6jcjqv9599v6q3j";
        };
      });
    };
  };
}

