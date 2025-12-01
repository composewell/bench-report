{
  description = "bench-report";

  inputs = {
    basepkgs.url = "git+ssh://git@github.com/composewell/streamly-packages?rev=5c3c31fa35f9f75a52e2fd68f7d1d47f7622ce33";
    nixpkgs.follows = "basepkgs/nixpkgs";
    nixpkgs-darwin.follows = "basepkgs/nixpkgs-darwin";
  };

  outputs = { self, nixpkgs, nixpkgs-darwin, basepkgs }:
    basepkgs.nixpack.mkOutputs {
      inherit nixpkgs nixpkgs-darwin basepkgs;
      name = "bench-report";
      sources = import ./sources.nix;
      # sources = basepkgs.nixpack.lib.localSource "bench-report" ./.;
      packages = basepkgs.nixpack.lib.devPackage "bench-report";
    };
}
