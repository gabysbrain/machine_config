let
  # NOTE: secrets need to be assigned to both users (for agenix command) and systems (for agenix serivce)
  tom = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGJQREmqaoPzlEQZfnOVZqH7rWkYaUuWmoQ2T5daJ/uU";
  tom-work = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN4dwEF9ZciW2+oxTmQi2cUUd+Jh5Mlng7876gA5U3il";
  me = [ tom tom-work ];

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
  "google-vdirsyncer.age".publicKeys = me;
  "github-bugwarrior.age".publicKeys = me;

  "wasabi.age".publicKeys = [tom] ++ backup;
  "restic.age".publicKeys = [tom] ++ backup;

  "diskstation-key.age".publicKeys = [ tom katana philadelphia ];
  "diskstation-smb.age".publicKeys = [ tom katana philadelphia ];

  "mikrotik-prometheus-config.yml.age".publicKeys = [ tom monitor ];
  "fritzbox-pw.age".publicKeys = [ tom monitor ];

  "raicoon.age".publicKeys = [ tom-work ];
  "raicoon-davmail.age".publicKeys = [ tom-work ];
}
