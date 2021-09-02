let
  tom = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGJQREmqaoPzlEQZfnOVZqH7rWkYaUuWmoQ2T5daJ/uU";
  users = [ tom ];

  philadelphia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGAYMK3yvisKXVemHBGQ80/rxxOgdAhLMxVmBo3ILD6o";
  brokkoli = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBA1HIzwzLMlZINSD1p36schK1NmxoiRr3jKoZKtsO6j";
  systems = [ philadelphia brokkoli ];

in
{
  "google-vdirsyncer.age".publicKeys = [tom];
  "vrvis.age".publicKeys = [tom];

  "wasabi.age".publicKeys = systems;
  "restic.age".publicKeys = systems;
  "diskstation-smb.age".publicKeys = systems;
  "diskstation-key.age".publicKeys = systems;
}
