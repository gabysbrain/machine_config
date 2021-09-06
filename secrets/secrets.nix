let
  # NOTE: secrets need to be assigned to both users (for agenix command) and systems (for agenix serivce)
  tom = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGJQREmqaoPzlEQZfnOVZqH7rWkYaUuWmoQ2T5daJ/uU";
  torsney-weir = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGJQREmqaoPzlEQZfnOVZqH7rWkYaUuWmoQ2T5daJ/uU";
  me = [ tom torsney-weir ];

  philadelphia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGAYMK3yvisKXVemHBGQ80/rxxOgdAhLMxVmBo3ILD6o";
  brokkoli = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBA1HIzwzLMlZINSD1p36schK1NmxoiRr3jKoZKtsO6j";
  systems = [ philadelphia brokkoli ];

in
{
  "google-vdirsyncer.age".publicKeys = me ++ systems;
  "vrvis.age".publicKeys = me ++ systems;

  "wasabi.age".publicKeys = me ++ [ philadelphia ];
  "restic.age".publicKeys = me ++ [ philadelphia ];
  "diskstation-key.age".publicKeys = me ++ [ philadelphia ];
}
