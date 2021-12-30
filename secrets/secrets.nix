let
  # NOTE: secrets need to be assigned to both users (for agenix command) and systems (for agenix serivce)
  tom = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGJQREmqaoPzlEQZfnOVZqH7rWkYaUuWmoQ2T5daJ/uU";
  torsney-weir = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGJQREmqaoPzlEQZfnOVZqH7rWkYaUuWmoQ2T5daJ/uU";
  me = [ tom torsney-weir ];

  philadelphia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGAYMK3yvisKXVemHBGQ80/rxxOgdAhLMxVmBo3ILD6o";
  katana = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBX/H11Ur29rHfI4X3zTz5KSYoW0XyIXtOxFUYwxn/r+";
  brokkoli = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBA1HIzwzLMlZINSD1p36schK1NmxoiRr3jKoZKtsO6j";
  interactiveSystems = [ philadelphia katana brokkoli ];

  monitor = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG6NDnkgBhim0cyNvsjt94RMqdPDnzmE12zW8FV3OMq7";
  util = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILmEI64SnaAwNsyPFw0hPTZvM8SI3XdncrEpa1DF95OS";
  servers = [ util monitor ];

  backup = [ philadelphia katana util ];

in
{
  "google-vdirsyncer.age".publicKeys = me ++ interactiveSystems;
  "github.age".publicKeys = me ++ interactiveSystems;
  "github_token.age".publicKeys = me ++ interactiveSystems;
  "vrvis.age".publicKeys = me ++ interactiveSystems;
  "vrvis-smb.age".publicKeys = me ++ interactiveSystems;

  "wasabi.age".publicKeys = me ++ backup;
  "restic.age".publicKeys = me ++ backup;

  "diskstation-key.age".publicKeys = me ++ [ philadelphia ];
  "diskstation-smb.age".publicKeys = me ++ [ philadelphia ];

  "mikrotik-prometheus-config.yml.age".publicKeys = me ++ [ monitor ];
  "fritzbox-pw.age".publicKeys = me ++ [ monitor ];
}
