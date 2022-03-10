let
  nixpkgsPath_21_11 =
    "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz";
  nixpkgsDefault = import (builtins.fetchTarball nixpkgsPath_21_11) { };
in
{
  nixpkgs ? nixpkgsDefault
, compiler ? "default"
}:
let haskellPackages =
        if compiler == "default"
        then nixpkgs.haskellPackages
        else nixpkgs.haskell.packages.${compiler};

    mkPackage = super: pkg: path: opts: inShell:
                let orig = super.callCabal2nixWithOptions pkg path opts {};
                 in if inShell
                    # Avoid copying the source directory to nix store by using
                    # src = null.
                    then orig.overrideAttrs (oldAttrs: { src = null; })
                    else orig;

  mkPkgGit = super: gitSrc: ref: name:
    let
      src = builtins.fetchGit {
        url = gitSrc;
        ref = ref;
      };
    in super.callCabal2nix name src { };

    mkHaskellPackages = inShell:
        haskellPackages.override {
            overrides = self: super:
                with nixpkgs.haskell.lib;
                {
          streamly-coreutils = let
            src = "git@github.com:composewell/streamly-coreutils.git";
            ref = "a12756efe24bbf303f2f6d66a95426c7657d297b";
          in mkPkgGit super src ref "streamly-coreutils";
          streamly-shell = let
            src = "https://github.com/composewell/streamly-shell.git";
            ref = "7d77ddc0c06aec21fbbfd1b920ec4f58da654a1b";
          in mkPkgGit super src ref "streamly-shell";
          streamly-process = nixpkgs.haskell.lib.dontCheck
            (super.callHackageDirect {
              pkg = "streamly-process";
              ver = "0.1.0";
              sha256 = "01nxisqfmn29fbirdsx71sfjp2rdqwrf469qyjcql2d11i1bxn94";
            } { });
          streamly = super.callHackageDirect {
            pkg = "streamly";
            ver = "0.8.0";
            sha256 = "0vy2lkljizlhpbpbybmg9jcmj2g4s1aaqd2dzy5c0y0n4rgwxask";
          } { };
                    bench-show = super.callHackageDirect {
                      pkg = "bench-show";
                      ver = "0.3.2";
                      sha256 = "16b8vyzdp9b5bh34kqmbfwjsyv8wgnxxwl8kjcpgxjsh52xzyaa0";
                    } { };

                    bench-report = mkPackage super "bench-report" ./. "" inShell;
                };
        };

    drv = mkHaskellPackages true;

    shell = drv.shellFor {
        packages = p:
          [ p.bench-report
          ];
        # Use a better prompt
        shellHook = ''
          export CABAL_DIR="$(pwd)/.cabal.nix"
          if test -n "$PS_SHELL"
          then
            export PS1="$PS_SHELL\[$bldred\](nix)\[$txtrst\] "
          fi
        '';
    };
in if nixpkgs.lib.inNixShell
   then shell
   else (mkHaskellPackages false).bench-report
