
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipedat

<!-- badges: start -->
<!-- [![Check package](https://github.com/inSileco/graphicsutils/actions/workflows/check-moreorless-standard.yaml/badge.svg)](https://github.com/inSileco/graphicsutils/actions/workflows/check-moreorless-standard.yaml) -->
<!-- [![codecov](https://codecov.io/gh/inSileco/graphicsutils/branch/master/graph/badge.svg)](https://codecov.io/gh/inSileco/graphicsutils) -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#preliminary)
![](https://img.shields.io/badge/status-preliminary-blue.svg)
[![R-CMD-check](https://github.com/Ecosystem-Assessments/pipedat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Ecosystem-Assessments/pipedat/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

*pipedat* is a package that provides pipelines to access, load, and
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

The easiest way to install `pipedat` is to use
[`remotes`](https://cran.r-project.org/package=remotes):

``` r
install.packages("remotes")
remotes::install_github("Ecosystem-Assessments/pipedat")
```

Then, load it:

``` r
library(pipedat)
```

## Main features

The `pipedat` package is built around function called `pipedat()` that
is used to access, load and format a wide variety of data; this function
calls on a series of individual scripts built to access data
programmatically and reproducibly, which we refer to as *data
pipelines*. Individual data pipelines are executed by using their
*unique identifier*, which are specific to the `pipedat` package. The
full list of data pipelines available can be viewed with the
`pipelist()` function:

``` r
# View list of pipelines 
# pipelist()

# Download and format a single dataset 
# pipedat("a3jsd4jh")

# Download and format multiple datasets
# pipedat(c("a3jsd4jh","a8732975y","soif8yiao"))
```

By default, the `pipedat()` function will export the raw and formatted
data in folders ‘data/data-raw/’ and `data/data-format`, respectively.

## List of pipelines

| Pipeline ID | Name                                                    | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | Source                                                                                                                                   |
|:------------|:--------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------|
| f635934a    | Federal Marine Bioregions                               | The spatial planning framework for Canada’s national network of Marine Protected Areas (MPAs) is comprised of 13 ecologically defined bioregions that cover Canada’s oceans and the Great Lakes.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | Fisheries and Oceans Canada (2009); Fisheries and Oceans Canada (2010); Government of Canada (2011); Fisheries and Oceans Canada (2021a) |
| 750b39f9    | Maritimes cumulative effects assessment study area grid | Gridded study area used jointly with N. Kelly and G. Murphy for the Maritimes region cumulative effects assessment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Kelly and Murphy (2021)                                                                                                                  |
| f2109e69    | Commercial fisheries logbooks                           | A compilation of landing data from Zonal Interchange File Format (ZIFF) data between 2000 and 2020                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Fisheries and Oceans Canada (2021b)                                                                                                      |
| 35608fef    | CanCoast - Coastal Materials Version 2.0                | CanCoast 2.0: data and indices to describe the sensitivity of Canada’s marine coasts to changing climate                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | Manson, Couture, and James (2019)                                                                                                        |
| 06230ea3    | Canadian National Marine Seismic Data Repository        | The Geological Survey of Canada (Atlantic and Pacific) has collected marine survey field records on marine expeditions for over 50 years. This release makes available the results of an ongoing effort to scan and convert our inventory of analog marine survey field records (seismic, sidescan and sounder) to digital format.                                                                                                                                                                                                                                                                                                                                                                                                 | Geological Survey of Canada (2021)                                                                                                       |
| 804db12e    | Federal Contaminated Sites Inventory                    | The Federal Contaminated Sites Inventory includes information on all known federal contaminated sites under the custodianship of departments, agencies and consolidated Crown corporations as well as those that are being or have been investigated to determine whether they have contamination arising from past use that could pose a risk to human health or the environment. The inventory also includes non-federal contaminated sites for which the Government of Canada has accepted some or all financial responsibility. It does not include sites where contamination has been caused by, and which are under the control of, enterprise Crown corporations, private individuals, firms or other levels of government. | Treasury Board of Canada Secretariat (2021)                                                                                              |
| 4b72884d    | Active and Inactive Disposal at Sea Sites               | The Active and Inactive Disposal at Sea Sites in Canadian Waters dataset provides spatial and related information of at-sea disposal sites approved for use in Canada in the last ten years and that remain open for consideration for additional use.                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | Environment and Climate Change Canada (2021)                                                                                             |

## How to contribute

External contributors are welcome to contribute data pipelines to this
package. Simply fork the [public repo]() and create your own data
pipeline. The `pipenew()` function creates a `dp_#####.R` template for
you to use to create a new data pipeline with a unique id. Create a pull
request for us to review the data pipeline for inclusion in the package.

A single pull request per pipeline should be created, and merged pull
requests should be squashed into a single commit.
