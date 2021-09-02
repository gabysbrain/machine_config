{ ... }:
{
  age.sshKeyPaths = [ 
    "/home/torsney-weir/.ssh/id_ed25519" 
    "/etc/ssh/ssh_host_ed25519_key"
  ];
  age.secrets = {
    google-vdirsyncer = {
      file = ./google-vdirsyncer.age;
      owner = "torsney-weir";
    };
    vrvis = {
      file = ./vrvis.age;
      owner = "torsney-weir";
    };
  };
}

