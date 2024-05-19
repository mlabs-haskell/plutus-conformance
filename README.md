# plutus-conformance

## Nix Binary Cache

You can optionally use binary cache to skip building software and download it instead.
Edit `/etc/nix/nix.conf` (or related settings in NixOS config) and merge the new
values separated by spaces into the options:

<!-- markdownlint-disable MD013 -->
```conf
substituters = ...https://plutonomicon.cachix.org https://cache.iog.io
trusted-public-keys = ... public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=  hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```
<!-- markdownlint-enable MD013 -->

Warning: do not add users to trusted-users because that
[gives them root accesswithout password](https://nixos.org/manual/nix/stable/command-ref/conf-file.html?highlight=giving%25root#conf-trusted-users).
