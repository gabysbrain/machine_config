{ ... }:
{
  age.secrets = {
    google-vdirsyncer = {
      file = ./google-vdirsyncer.age;
      owner = "tom";
    };
    vrvis = {
      file = ./vrvis.age;
      owner = "tom";
    };
    github-bugwarrior = {
      file = ./github-bugwarrior.age;
      owner = "tom";
    };
  };
}

