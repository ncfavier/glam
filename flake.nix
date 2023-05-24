{
  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }: let
    examples = [
      { name = "fibonacci"; description = "Fibonacci sequence"; }
      { name = "primes"; description = "Prime numbers"; }
      { name = "y"; description = "Y combinator"; }
    ];

    inherit (nixpkgs) lib;
    system = "x86_64-linux";
    haskellOverlay = self: super: {
      haskell = super.haskell // {
        packageOverrides = hself: hsuper: {
          glam = self.haskell.lib.overrideCabal (hself.callCabal2nix "glam" ./glam {}) {
            doHaddock = !(hself.ghc.isGhcjs or false);
            isLibrary = true; # otherwise doHaddock does nothing
            haddockFlags = [ "--all" "--html-location='https://hackage.haskell.org/package/$pkg-$version/docs'" ];
          };
        };
      };
    };
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ haskellOverlay ];
    };
    hpkgs = pkgs.haskell.packages.ghc94;
  in {
    packages.${system} = rec {
      default = glam;
      glam = hpkgs.glam;
      glam-js = pkgs.haskell.packages.ghcjs.glam;

      glam-min-js = pkgs.runCommand "glam.min.js" {
        nativeBuildInputs = with pkgs; [ closurecompiler ];
        glam = "${glam-js}/bin/glam.jsexe";
      } ''
        closure-compiler -O advanced -W quiet --jscomp_off undefinedVars \
          --externs "$glam/all.js.externs" --js "$glam/all.js" --js_output_file "$out"
      '';

      web = pkgs.runCommandLocal "glam-web" {
        examples = lib.concatMapStrings ({ name, description }: ''
          <button class=example id="${name}" data-example="${
            lib.escapeXML (lib.fileContents examples/${name}.glam)
          }">${lib.escapeXML description}</button>
        '') examples;
        scripts = ''
          <script src="glam.min.js?v=${builtins.hashFile "sha1" glam-min-js}" defer></script>
          <script src="glam_syntax.js?v=${builtins.hashFile "sha1" ./web/glam_syntax.js}"></script>
        '';
      } ''
        mkdir -p "$out"
        cp -rT ${./web} "$out"
        ln -s ${glam.haddockDir glam}/glam "$out/doc"
        ln -s ${glam-min-js} "$out/glam.min.js"
        substituteAllInPlace "$out/index.html"
      '';
    };

    devShells.${system}.default = hpkgs.shellFor {
      packages = ps: with ps; [ glam self.packages.${system}.glam-js ];
      nativeBuildInputs = with pkgs; [
        haskell.compiler.ghcjs
        cabal-install
        haskell-language-server
      ];
    };
  };
}
