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
    github = {
      file = ./github.age;
      owner = "tom";
    };
  };
}

