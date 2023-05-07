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
    haskellOverlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = final.haskell.lib.packageSourceOverrides {
          glam = ./glam;
        };
      };
    };
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ haskellOverlay ];
    };
  in {
    packages.${system} = {
      default = self.packages.${system}.glam;
      glam = pkgs.haskellPackages.glam;

      glamjs = pkgs.runCommand "glam.min.js" {
        nativeBuildInputs = with pkgs; [ closurecompiler ];
        glam = "${pkgs.haskell.packages.ghcjs.glam}/bin/glam.jsexe";
      } ''
        closure-compiler -O advanced -W quiet --jscomp_off undefinedVars \
          --externs "$glam/all.js.externs" --js "$glam/all.js" --js_output_file "$out"
      '';

      web = pkgs.runCommand "glam-web" {
        examples = lib.concatMapStrings ({ name, description }: ''
          <button class=example id="${name}" data-example="${
            lib.escapeXML (builtins.readFile examples/${name}.glam)
          }">${lib.escapeXML description}</button>
        '') examples;
      } ''
        mkdir -p "$out"
        cp -rT ${./web} "$out"
        ln -s ${self.packages.${system}.glamjs} "$out/glam.min.js"
        substituteAllInPlace "$out/index.html"
      '';
    };

    devShells.${system}.default = pkgs.haskellPackages.shellFor {
      packages = ps: with ps; [ glam pkgs.haskell.packages.ghcjs.glam ];
      nativeBuildInputs = with pkgs; [
        haskell.compiler.ghcjs
        cabal-install
        haskell-language-server
      ];
    };
  };
}
