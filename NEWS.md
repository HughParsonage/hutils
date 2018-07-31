# hutils 1.2.0
* New functions:
  - `RQ(p, yes, no)` short for `if (!requireNamespace("p", quietly = TRUE)) yes else no`.
  - `isAttached` for conveniently determining whether a namespace is attached
  - `ahull` for locating rectangles in a plot, as for automatically locating a text box.
  - `Switch` vectorized version of `switch` to avoid nested `if_else`'s. 
* `drop_grep` is an alias for `drop_colr`.

* Minor changes:
  - `if_else` reports a clearer error message when `length(condition) == 1`.

# hutils 1.1.1
* Change of stable test:
    - The only changes were so that the test proceeds when a required package is present. The tests would never have succeeded in cases where they now skip. Consequently the major version number has not been incremented.
    
* Bug fix:
    - `find_pattern_in` respects `include.comments`
* Enhancement:
    - `find_pattern_in` accepts argument `which_lines` to allow multiple lines per file, not just the first (the default).

# hutils 1.1.0
* New functions:
    - `auc`: area under the curve given predicted and actual values.
    - `select_grep`: select columns matching a pattern.
    - `dev_copy2a4`: convenience function for copying to an A4 PDF.

* Other minor changes:
  - `if_else` should be slightly faster when the condition contains `NAs`.
     before: 3.6 ms now 1.5 ms (for 100,000 entries -- see vignette)
  - `drop_constant_cols` first checks whether the first and second entries are identical before working out the number of unique values.
   

# hutils 1.0.0
* Stable release:
  - All tests are now marked as major release.
* New function `%notchin%` for a 'safer' alternative to `%notin%`.

# hutils 0.10.0
* New function `implies`, logical implies.
* New function `drop_constant_cols`
* New function `print_transpose_data_table` for glimpsing data tables by rows
* New alias `pow` for exponentiation.
* Bug fixes:
    - Ensure `drop_empty_cols` retains non-empty columns when duplicate names are used.
    - `coalesce` errors if there is `...` wrongly contains factors.

# hutils 0.9.0
* Changed `if_else` to reflect dplyr's formals so it can be a drop-in replacement.
* Fix error for multi-length `missing` value in `if_else` when length-one `condition`.
* Add vignette
* Minor performance improvements

# hutils 0.8.0

* Added a `NEWS.md` file to track changes to the package.
* New functions
    - `%ein%` `%enotin%` avoid misspellings in filters
    - `AND`, `NEITHER`, `NOR`, `OR`, `nor`, `neither` logical aliases
    - `drop_colr` drop columns matching pattern
    - `ngrep` negate regular expression
    - `select_which` similar to `dplyr::select_if` 
    - `set_colsuborder` change the order of some columns without affecting the order of others
    - `weight2rows` convert a weighted `data.table` to an unweighted one by repeating rows by the weight
    - `coalesce` and `if_else`: lightweight versions of `dplyr::` equivalents
* Bug fixes:
    - `set_cols_first|last` now respects the order of the supplied columns
* Enhancements:
    - `mutate_other` now accepts a `mass` argument as another way to generate an 'Other' column.



