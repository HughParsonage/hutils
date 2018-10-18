[![Travis-CI Build Status](https://travis-ci.org/HughParsonage/hutils.svg?branch=master)](https://travis-ci.org/HughParsonage/hutils)
[![codecov.io](https://codecov.io/github/HughParsonage/hutils/coverage.svg?branch=master)](https://codecov.io/github/HughParsonage/hutils?branch=master)


# hutils
Miscellaneous R functions and aliases

My name is Hugh and this is a package of functions I often put in `R/utils.s`.
Hence, `hutils`.

### Scope

The package attempts to provide lightweight, fast, and stable functions for 
common operations.

By **lightweight**, I mean in terms of dependencies: we import 
`package:data.table` and `package:fastmatch` which do require compilation, but 
in C.  Since so many operations handle data frames, `data.table` seemed 
worthwhile -- and besides its compile time is not too onerous.
Otherwise, all dependencies do not require compilation. (I also try to minimize
the cardinality of package imports, but it's mostly the compile time I'm 
focused on.)

By **fast**, I mean essentially as fast as possible without using compilation.

By **stable**, I mean that unit tests should not change unless the major version also
changes. To make this completely transparent, tests include the version of their
introduction and are guaranteed to not be modified (not even in the sense of 
adding extra, independent tests) while the major version is 1. Tests that do not
include the version in their filename may be modified from version to version 
(though this will be avoided).

