{
  description = "A Haskell implementation of BOLT #7.";

  inputs = {
    ppad-bolt1 = {
      type = "path";
      path = "/Users/jtobin/src/ppad/bolt1";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    ppad-nixpkgs = {
      type = "git";
      url  = "git://git.ppad.tech/nixpkgs.git";
      ref  = "master";
    };
    flake-utils.follows = "ppad-nixpkgs/flake-utils";
    nixpkgs.follows = "ppad-nixpkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ppad-nixpkgs, ppad-bolt1 }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = "ppad-bolt7";

        pkgs  = import nixpkgs { inherit system; };
        hlib  = pkgs.haskell.lib;
        llvm  = pkgs.llvmPackages_19.llvm;
        clang = pkgs.llvmPackages_19.clang;

        bolt1 = ppad-bolt1.packages.${system}.default;
        bolt1-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag bolt1 "llvm")
            [ llvm clang ];

        hpkgs = pkgs.haskell.packages.ghc910.extend (new: old: {
          ppad-bolt1 = bolt1-llvm;
          ${lib} = new.callCabal2nix lib ./. {
            ppad-bolt1 = new.ppad-bolt1;
          };
        });

        cc    = pkgs.stdenv.cc;
        ghc   = hpkgs.ghc;
        cabal = hpkgs.cabal-install;
      in
        {
          packages.default = hpkgs.${lib};

          packages.haddock = hpkgs.${lib}.doc;

          devShells.default = hpkgs.shellFor {
            packages = p: [
              (hlib.doBenchmark p.${lib})
            ];

            buildInputs = [
              cabal
              cc
              llvm
            ];

            shellHook = ''
              PS1="[${lib}] \w$ "
              echo "entering ${system} shell, using"
              echo "cc:    $(${cc}/bin/cc --version)"
              echo "ghc:   $(${ghc}/bin/ghc --version)"
              echo "cabal: $(${cabal}/bin/cabal --version)"
            '';
          };
        }
      );
}
