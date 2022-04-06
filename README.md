
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datapipe

<!-- badges: start -->
<!-- [![Check package](https://github.com/inSileco/graphicsutils/actions/workflows/check-moreorless-standard.yaml/badge.svg)](https://github.com/inSileco/graphicsutils/actions/workflows/check-moreorless-standard.yaml) -->
<!-- [![codecov](https://codecov.io/gh/inSileco/graphicsutils/branch/master/graph/badge.svg)](https://codecov.io/gh/inSileco/graphicsutils) -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- [![r-universe](https://insileco.r-universe.dev/badges/graphicsutils)](https://insileco.r-universe.dev/ui#builds) -->
<!-- badges: end -->

*datapipe* is a package that provides pipelines to access, load, and
format a variety of data from multiple sources programatically. The
intent of this package is to support the varying projects undertaken
under the umbrella organization *Ecosystem Assessments*. Certain
datasets are directly accessed through APIs and open data portals, while
others require a local copy of the raw data for varying reasons such as
data whose distribution is limited or protected by data sharing
agreements. The sole purpose of this package is to facilate the
reproducible use and reuse of specific datasets accross different
projects.

## Installation

The easiest way to install `datapipe` is to use
[`remotes`](https://cran.r-project.org/package=remotes):

``` r
install.packages("remotes")
remotes::install_github("inSileco/graphicsutils")
```

Then, load it:

``` r
library(datapipe)
```

## Main features

The datapipe function is built aroung a main function called
`datapipe()` that is used to access, load and format a wide variety of
data.
