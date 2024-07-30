{
  description = "plutus-conformance";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    hci-effects.url = "github:mlabs-haskell/hercules-ci-effects/push-cache-effect";
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ flake-parts, haskell-nix, iohk-nix, CHaP, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      debug = true;
      imports = [
        inputs.hci-effects.flakeModule
        inputs.pre-commit-hooks-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { lib, config, system, ... }:
        let
          pkgs =
            import haskell-nix.inputs.nixpkgs {
              inherit system;
              overlays = [
                haskell-nix.overlay
                iohk-nix.overlays.crypto
                iohk-nix.overlays.haskell-nix-crypto
              ];
              inherit (haskell-nix) config;
            };
          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc966";
            index-state = "2024-07-23T08:58:13Z";
            inputMap = {
              "https://chap.intersectmbo.org/" = CHaP;
            };
            shell = {
              withHoogle = true;
              withHaddock = true;
              exactDeps = false;
              tools = {
                cabal = { };
                haskell-language-server = { };
                hlint = { };
                cabal-fmt = { };
                ormolu = { };
                hspec-discover = { };
                markdown-unlit = { };
              };
              shellHook = ''
                export LC_CTYPE=C.UTF-8;
                export LC_ALL=C.UTF-8;
                export LANG=C.UTF-8;
                ${config.pre-commit.installationScript}
              '';
            };
          };
          flake = project.flake { };
        in
        {
          inherit (flake) devShells packages checks;

          pre-commit.settings.hooks = {
            nixpkgs-fmt.enable = true;
            deadnix.enable = true;
            statix.enable = true;
            cabal-fmt.enable = true;
            ormolu.enable = true;
            shellcheck.enable = true;
            typos = {
              enable = true;
              settings.configPath = "./.typos.toml";
            };
            markdownlint = {
              enable = true;
              settings.configuration = lib.importJSON ./.markdownlint.json;
            };
          };
        };

      herculesCI.ciSystems = [ "x86_64-linux" "x86_64-darwin" ];
    };
}
