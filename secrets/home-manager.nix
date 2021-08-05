{ ... }:
{
  age.secrets = {
    vdirsyncer = {
      file = ./vdirsyncer.age;
      owner = "tom";
    };
    vrvis = {
      file = ./vrvis.age;
      owner = "tom";
    };
  };
}

