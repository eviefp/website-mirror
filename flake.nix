{
  description = "Group Meowing website";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05-small";
    flake-utils.url = "github:numtide/flake-utils";
    website-engine-source = {
      url = "github:eviefp/website-engine?ref=main";
      flake = false; # TODO: figure out how to export a cabal package from the flake
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

  outputs = { self, nixpkgs, flake-utils, treefmt-nix, website-engine-source, pico-css }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };

          mkCommand = runtimeInputs: text: {
            type = "app";
            program = pkgs.lib.getExe (pkgs.writeShellApplication {
              name = "command";
              inherit runtimeInputs text;
            });
          };
          haskellPackages = pkgs.haskell.packages.ghc984.override {
            overrides = final: prev: {
              website-engine = prev.callCabal2nix "website-engine" website-engine-source { };
              group-meowing = prev.callCabal2nix "group-meowing" ./. { };
            };
          };
          treefmt-config = {
            projectRootFile = "flake.nix";
            programs = {
              nixpkgs-fmt.enable = true;
              cabal-fmt.enable = true;
              fourmolu.enable = true;
              fourmolu.package = pkgs.haskell.packages.ghc984.fourmolu;
            };
          };
          treefmt = (treefmt-nix.lib.evalModule pkgs treefmt-config).config.build;
        in
        {
          apps = {
            build = (mkCommand [ ] ''
              cabal build group-meowing
            '');
            generate = (mkCommand [ ] ''
              cabal run group-meowing --
            '');
            clean = (mkCommand [ ] ''
              cabal run group-meowing -- clean
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
              cabal run group-meowing -- --help
            '');
          };

          formatter = treefmt.wrapper;

          checks = {
            fmt = treefmt.check self;
            hlint = pkgs.runCommand "hlint" { buildInputs = [ pkgs.hlint ]; } ''
              cd ${./.}
              hlint src spec app
              touch $out
            '';
          };

          packages.default = haskellPackages.callCabal2nix "group-meowing" ./. { };

          devShells.default = haskellPackages.shellFor {
            packages = p: [ p.group-meowing ];
            withHoogle = true;
            buildInputs = [
              pkgs.http-server

              pkgs.zlib.dev
              pkgs.haskell.compiler.ghc984
              haskellPackages.cabal-install
              haskellPackages.cabal2nix
              haskellPackages.haskell-language-server
            ];
            shellHook = ''
              ln -sf ${pico-css}/css/pico.min.css site/css/pico.min.css
            '';
          };
        }
      );
}
