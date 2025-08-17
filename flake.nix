{
  description = "Group Meowing website";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    website-engine = {
      url = "github:eviefp/website-engine";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pico-css = {
      url = "github:picocss/pico?ref=main";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, treefmt-nix, website-engine, pico-css }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          # use this for tooling, higher chances of nixos cache hits
          pkgs = import nixpkgs {
            inherit system;
          };

          # use this for haskellPackages
          pkgs-pandoc-upgrade = import nixpkgs {
            inherit system;
            overlays = [ website-engine.overlays.default ];
          };

          # add our current package to the list;
          haskellPackages = pkgs-pandoc-upgrade.haskell.packages.ghc9102.override (prevArgs: {
            # warning: we need to `composeExtensions` because we also have the overlay that overrides this bit
            overrides = pkgs-pandoc-upgrade.lib.composeExtensions (prevArgs.overrides or (_: _: { })) (final: prev: {
              group-meowing = final.callCabal2nix "group-meowing" ./. { };
            });
          });

          # helper to create nix apps
          mkCommand = runtimeInputs: text: {
            type = "app";
            program = pkgs.lib.getExe (pkgs.writeShellApplication {
              name = "command";
              inherit runtimeInputs text;
            });
          };

          group-meowing = haskellPackages.callCabal2nix "group-meowing" ./. { };

          # formatter config
          treefmt-config = {
            projectRootFile = "flake.nix";
            programs = {
              nixpkgs-fmt.enable = true;
              cabal-fmt.enable = true;
              fourmolu.enable = true;
              fourmolu.package = pkgs.haskell.packages.ghc9102.fourmolu;
            };
          };
          treefmt = (treefmt-nix.lib.evalModule pkgs treefmt-config).config.build;
        in
        {
          # run via `nix run .#build` / `nix run .#generate`
          apps = {
            build = (mkCommand [ ] ''
              cabal build group-meowing
            '');
            generate = (mkCommand [ ] ''
              ${pkgs.lib.getExe (group-meowing)}
            '');
            clean = (mkCommand [ ] ''
              ${pkgs.lib.getExe (group-meowing)} clean
            '');
            repl = (mkCommand [ ] ''
              cabal v2-repl group-meowing
            '');
            http-server = (mkCommand [ ] ''
              http-server docs
            '');
            browse-site = (mkCommand [ ] ''
              xdg-open http://localhost:8080
            '');
            hoogle = (mkCommand [ ] ''
              hoogle server --local --port 8999
            '');
            browse-hoogle = (mkCommand [ ] ''
              xdg-open http://localhost:8999
            '');
            help = (mkCommand [ ] ''
              ${pkgs.lib.getExe (group-meowing)} --help
            '');
          };

          # run with `nix fmt`
          formatter = treefmt.wrapper;

          # run with `nix flake check`
          checks = {
            # run the formatter in check mode (errors instead of fixing)
            fmt = treefmt.check self;

            # haskell lints
            hlint = pkgs.runCommand "hlint" { buildInputs = [ pkgs.hlint ]; } ''
              cd ${./.}
              hlint app
              touch $out
            '';

            # by building the project, nix also runs the tests automatically
            hstests = pkgs.runCommand "hstests" { buildInputs = [ group-meowing ]; } ''
              touch $out
            '';
          };

          # run with `nix run .#default`
          packages.default = group-meowing;

          # run with `nix develop`
          devShells.default = haskellPackages.shellFor {
            packages = p: [ p.group-meowing ];
            # This is a bit slow.
            withHoogle = false;
            buildInputs = [
              pkgs.haskell.compiler.ghc9102

              pkgs.haskell.packages.ghc9102.cabal-install
              pkgs.haskell.packages.ghc9102.haskell-language-server

              pkgs.http-server
              pkgs.zlib.dev
            ];
            shellHook = ''
              ln - sf ${pico-css}/css/pico.min.css site/css/pico.min.css
            '';
          };
        }
      );
}

