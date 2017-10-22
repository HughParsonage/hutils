## Test environments
* local MRAN install, R 3.4.1
* ubuntu 12.04 (on travis-ci), release and dev <https://travis-ci.org/HughParsonage/hutils/jobs/291102693>
* local R 3.5.0 (devel) r73560

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a package update, fixing inter alia a problem with upcoming package `testthat`.
* Package errors with `win-builder` which appears to be older than the environment on which I ran `R CMD check --as-cran hutils_0.9.0.tar.gz`


