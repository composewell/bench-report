{nixpack}:
with nixpack.mkSources;
let
  cwgh = repo: rev: gh "composewell" repo rev;
in
{
layers = [
{
  bench-report = local ./.
    // { c2nix = ["--flag no-charts"];
         flags = ["--flags no-charts"];
       };

  streamly-coreutils =
    cwgh "streamly-coreutils" "9daf64da9868c8e6bed11d51bcff45bf6e0b0a93";
}
];
}
