# multiverse 0.6.1

### Bug Fixes
- Fix for #107. Parallel execution of the multiverse (i.e. when cores >= 2 as an argument to `execute_multiverse`) resulted in creation of new environments and execution in these new environments, which are different from the ones that are stored in the multiverse object and returned by `expand`. Fixed implementation so that all the bindings from the environments were copied to the stored environments, and the resultant variable bindings can be accessed through `expand` (thanks, @markromanmiller, #108).
- Throw errors when empty options are declared (eg. trailing commas) inside `branch` statements

### Minor Changes
- Improved support for parallel processing using `future` as backend. Users can set up parallel execution workflows on both unix and non-unix systems using `future`. Multiverses can be executed in parallel on a local machine or concurrently on a cluster.
- Support for displaying progress of execution using `txtProgressBar`. Use argument `progress = TRUE` for the function `execute_multiverse()`.
- `.errors` column added to the output of `expand` which lists the errors encountered during execution of the multiverse.
- Updates to the documentation and examples, to improve walkthrough of the steps involved in creating a multiverse analysis.

# multiverse 0.5.0

### Major changes:
- First CRAN release.