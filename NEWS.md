# multiverse 0.6.2

### Bug Fixes
- Fix for #119. Fixes for compatibility with evaluate 1.0.0

### Minor Changes
- Added functions to allow export of the multiverse results as JSON to be compatible with [Milliways](https://abhsarma.github.io/milliways/). EMARs are not yet supported however, as that requires changes to knitr
- Added support for naming variables when extracting multiple variables using `extract_variables`
- Added support for extraction of objects using `extract_variables`
- Changes to printing behavior: (1) we do not allow a user to print variables declared in a multiverse code block from an R code block (it will throw an error); (2) we do not show the output of multiverse code blocks when knitting (as it can create hundreds or thousands of outputs)


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