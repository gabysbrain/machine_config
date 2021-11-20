let
  # NOTE: secrets need to be assigned to both users (for agenix command) and systems (for agenix serivce)
  tom = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGJQREmqaoPzlEQZfnOVZqH7rWkYaUuWmoQ2T5daJ/uU";
  torsney-weir = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGJQREmqaoPzlEQZfnOVZqH7rWkYaUuWmoQ2T5daJ/uU";
  me = [ tom torsney-weir ];

  philadelphia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGAYMK3yvisKXVemHBGQ80/rxxOgdAhLMxVmBo3ILD6o";
  brokkoli = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBA1HIzwzLMlZINSD1p36schK1NmxoiRr3jKoZKtsO6j";
  interactiveSystems = [ philadelphia brokkoli ];

  monitor = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC4aiKG9pWpXlFaTFGN3y9JaN53x5dzb+TOjKcay1WbT";
  util = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA20Dwsc3NRv2eq3dCK3bcOBiQZ2FkFwsggmI8Q9L10U";
  servers = [ util monitor ];

in
{
  "google-vdirsyncer.age".publicKeys = me ++ interactiveSystems;
  "github.age".publicKeys = me ++ interactiveSystems;
  "github_token.age".publicKeys = me ++ interactiveSystems;
  "vrvis.age".publicKeys = me ++ interactiveSystems;
  "vrvis-smb.age".publicKeys = me ++ interactiveSystems;

  "wasabi.age".publicKeys = me ++ [ philadelphia ];
  "restic.age".publicKeys = me ++ [ philadelphia ];
  "diskstation-key.age".publicKeys = me ++ [ philadelphia ];
  "diskstation-smb.age".publicKeys = me ++ [ philadelphia ];

  "router-pw.age".publicKeys = me ++ servers;
  "fritzbox-pw.age".publicKeys = me ++ servers;
}
