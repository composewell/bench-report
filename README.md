# Benchmark running tool

Provides the following functionality:

* Multiple benchmark executables can be grouped into a named group and the
  whole group can be run using a single command.
* Individual benchmarks can be invoked with different RTS options to limit
  memory.
* We can run a certain category of benchmarks e.g. o-1-space benchmarks to test
  streaming nature of operations.
* We can compare benchmark results from a baseline.
* Allows using git-cabal for branch specific builds

# Test running tool

Provides the following functionality:

* Multiple test executables can be grouped into a named group and the
  whole group can be run using a single command.
* Individual tests can be invoked with different RTS options to limit
  memory.
* Coverage data can be combined for different tests.
* Allows using git-cabal for branch specific builds
