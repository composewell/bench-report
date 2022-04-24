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

  mkPkgGit = super: gitSrc: rev: name:
    let
      src = builtins.fetchGit {
        url = gitSrc;
        rev = rev;
      };
    in super.callCabal2nix name src { };

  mkPkgGitRef = super: gitSrc: rev: ref: name:
    let
      src = builtins.fetchGit {
        url = gitSrc;
        rev = rev;
        ref = ref;
      };
    in super.callCabal2nix name src { };

    mkHaskellPackages = inShell:
        haskellPackages.override {
            overrides = self: super:
                with nixpkgs.haskell.lib;
                {
          #streamly-coreutils = let
          #  src = "git@github.com:composewell/streamly-coreutils.git";
          #  rev = "a12756efe24bbf303f2f6d66a95426c7657d297b";
          #in mkPkgGit super src rev "streamly-coreutils";

          streamly-coreutils =
            nixpkgs.haskell.lib.overrideCabal
              (let src = fetchGit {
                  url = "git@github.com:composewell/streamly-coreutils.git";
                  rev = "8fa4a3ebedb3f970f64b7d78d8ab340b0f771abe";
              }; in super.callCabal2nix "streamly-coreutils" src {})
              (old:
                { librarySystemDepends =
                    if builtins.currentSystem == "x86_64-darwin"
                    then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
                    else [];
                  #enableLibraryProfiling = false;
                  doHaddock = false;
                  doCheck = false;
                });

          streamly-shell = let
            src = "https://github.com/composewell/streamly-shell.git";
            rev = "7d77ddc0c06aec21fbbfd1b920ec4f58da654a1b";
          in mkPkgGit super src rev "streamly-shell";

          streamly-process = let
            src = "https://github.com/composewell/streamly-process.git";
            rev = "a56ef61c1208c2d564a526eba315d903c0b4af2c";
          in nixpkgs.haskell.lib.dontCheck
            (mkPkgGitRef super src rev "streamly-latest" "streamly-process");

          #streamly-process = nixpkgs.haskell.lib.dontCheck
          #  (super.callHackageDirect {
          #    pkg = "streamly-process";
          #    ver = "0.2.0.1";
          #    sha256 = "sha256-no/U5aWkZJXaA6HN6H78iZiYu3kaR7i2Ouu8oewAN2o=";
          #  } { });

          streamly =
            nixpkgs.haskell.lib.overrideCabal
              #(super.callHackageDirect
              #  { pkg = "streamly";
              #    ver = "0.8.2";
              #    sha256 = "sha256-CjFq9SCdbgLZa7NqOE4OtC8OaFg4vK8VmIDjGU5rGko=";
              #  } {})
              (let src = fetchGit {
                  url = "git@github.com:composewell/streamly.git";
                  rev = "a849812d9960d5b7868e770156ab774ee8f70157";
              }; in super.callCabal2nix "streamly" src {})
              (old:
                { librarySystemDepends =
                    if builtins.currentSystem == "x86_64-darwin"
                    then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
                    else [];
                  #enableLibraryProfiling = false;
                  doHaddock = false;
                });

          streamly-core =
            nixpkgs.haskell.lib.overrideCabal
              (let src = fetchGit {
                  url = "git@github.com:composewell/streamly.git";
                  rev = "a849812d9960d5b7868e770156ab774ee8f70157";
              }; in super.callCabal2nix "streamly-core" "${src}/core" {})
              (old:
                { librarySystemDepends =
                    if builtins.currentSystem == "x86_64-darwin"
                    then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
                    else [];
                  #enableLibraryProfiling = false;
                  doHaddock = false;
                });

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
