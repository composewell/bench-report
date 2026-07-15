{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  bench-report = local ./.
    // { c2nix = ["--flag no-charts"];
         flags = ["--flags no-charts"];
       };
}
];
}
