{
  description = "plutus-conformance";
  nixConfig = {
    extra-substituters = [ "https://plutonomicon.cachix.org" ];
    allow-import-from-derivation = "true";
    auto-optimise-store = "true";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs";
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    hci-effects.url = "github:mlabs-haskell/hercules-ci-effects/push-cache-effect";
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ flake-parts, nixpkgs, haskell-nix, iohk-nix, CHaP, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      debug = true;
      imports = [
        inputs.hci-effects.flakeModule
        inputs.pre-commit-hooks-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, system, ... }:
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
            compiler-nix-name = "ghc964";
            index-state = "2024-01-16T11:00:00Z";
            inputMap = {
              "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
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
                fourmolu = { };
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
          inherit (flake) devShells packages;

          pre-commit.settings.hooks = {
            nixpkgs-fmt.enable = true;
            deadnix.enable = true;
            statix.enable = true;
            cabal-fmt.enable = true;
            # TODO(chfanghr): Configuration for ormolu
            ormolu.enable = true;
            shellcheck.enable = true;
            typos = {
              enable = true;
              settings.configPath = "./.typos.toml";
            };
            markdownlint.enable = true;
          };
        };

      herculesCI.ciSystems = [ "x86_64-linux" "x86_64-darwin" ];
    };
}
