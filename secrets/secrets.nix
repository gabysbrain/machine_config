let
  tom = "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAIEAwDPUjo80GFY2FO9bDH9cAXo7n7SiUKjXIHzfRMfsAqD9Rk/puV+W4QRvT0XOZSEZQf3gifcPM/raA35BVmAzAa2jYISWeUWIqYf+AcipFrMKKqS639Q9/GgJL2STr6Gh0EVHsGcFJpuJ8GO5eqnKR0ZYl3j9bpMO/WpgkAw7hUU=";
  users = [ tom ];

  philadelphia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGAYMK3yvisKXVemHBGQ80/rxxOgdAhLMxVmBo3ILD6o";
  systems = [ philadelphia ];

in
{
  "vdirsyncer.age".publicKeys = systems;
}
