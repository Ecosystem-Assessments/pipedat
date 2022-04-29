
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

    #> Registered S3 methods overwritten by 'stars':
    #>   method             from
    #>   st_bbox.SpatRaster sf  
    #>   st_crs.SpatRaster  sf

| Pipeline ID | Name                                                    | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | Source                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|:------------|:--------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| f635934a    | Federal Marine Bioregions                               | The spatial planning framework for Canada’s national network of Marine Protected Areas (MPAs) is comprised of 13 ecologically defined bioregions that cover Canada’s oceans and the Great Lakes.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | Fisheries and Oceans Canada (2009); Fisheries and Oceans Canada (2010); Government of Canada (2011); Fisheries and Oceans Canada (2021a)                                                                                                                                                                                                                                                                                                                                   |
| 750b39f9    | Maritimes cumulative effects assessment study area grid | Gridded study area used jointly with N. Kelly and G. Murphy for the Maritimes region cumulative effects assessment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Kelly and Murphy (2021)                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| f2109e69    | Commercial fisheries logbooks                           | A compilation of landing data from Zonal Interchange File Format (ZIFF) data between 2000 and 2020                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Fisheries and Oceans Canada (2021b)                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| 35608fef    | CanCoast - Coastal Materials Version 2.0                | CanCoast 2.0: data and indices to describe the sensitivity of Canada’s marine coasts to changing climate                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | Manson, Couture, and James (2019)                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| 06230ea3    | Canadian National Marine Seismic Data Repository        | The Geological Survey of Canada (Atlantic and Pacific) has collected marine survey field records on marine expeditions for over 50 years. This release makes available the results of an ongoing effort to scan and convert our inventory of analog marine survey field records (seismic, sidescan and sounder) to digital format.                                                                                                                                                                                                                                                                                                                                                                                                 | Geological Survey of Canada (2021)                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| 804db12e    | Federal Contaminated Sites Inventory                    | The Federal Contaminated Sites Inventory includes information on all known federal contaminated sites under the custodianship of departments, agencies and consolidated Crown corporations as well as those that are being or have been investigated to determine whether they have contamination arising from past use that could pose a risk to human health or the environment. The inventory also includes non-federal contaminated sites for which the Government of Canada has accepted some or all financial responsibility. It does not include sites where contamination has been caused by, and which are under the control of, enterprise Crown corporations, private individuals, firms or other levels of government. | Treasury Board of Canada Secretariat (2021)                                                                                                                                                                                                                                                                                                                                                                                                                                |
| 4b72884d    | Active and Inactive Disposal at Sea Sites               | The Active and Inactive Disposal at Sea Sites in Canadian Waters dataset provides spatial and related information of at-sea disposal sites approved for use in Canada in the last ten years and that remain open for consideration for additional use.                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | Environment and Climate Change Canada (2021)                                                                                                                                                                                                                                                                                                                                                                                                                               |
| 68609420    | Atlantic Shoreline Classification                       | The Atlantic dataset is part of Environment and Climate Change Canada�s Shoreline Classification and Pre-Spill database. Shoreline classification data has been developed for use by the Environmental Emergencies Program of Environment and Climate Change Canada for environmental protection purposes. Marine and estuarine shorelines are classified according to the character (substrate and form) of the upper intertidal (foreshore) or upper swash zone (Sergy, 2008).                                                                                                                                                                                                                                                   | Sergy (2008)                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| 084860fd    | WoRMS North-West Atlantic species checklist             | CaRMS and WoRMS species list for the North-West Atlantic area of interest                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | Nozères and Kennedy (2021); Horton, Kroh, Ahyong, Bailly, Bieler, Boyko, Brandão, Gofas, Hooper, Hernandez, Mees, Molodtsova, Paulay, Bouirig, Decock, Dekeyzer, Vandepitte, Vanhoorne, Adlard, Agatha, Ahn, Akkari, Alvarez, Amorim, Anderberg, Anderson, Andrés, Ang, Antic, Antonietto, Arango, Artois, Atkinson, Auffenberg, Baldwin, Bank, Barber, Barbosa, Bartsch, Bellan-Santini, Bergh, Bernot, Berta, Bezerra, Blanco, Blasco-Costa, Blazewicz, Bock, Bonifacino |

de León, Böttger-Schnack, Bouchet, Boury-Esnault, Bouzan, Boxshall,
Bray, Bruce, Bruneau, Bueno, Bueno-Villegas, Cairns, Calvo Casas,
Carballo, Cárdenas, Carstens, Chan, Chan, Cheng, Christenhusz,
Churchill, Coleman, Collins, Collins, Corbari, Cordeiro, Cornils, Costa
Corgosinho, Coste, Costello, Crandall, Cremonte, Cribb, Cutmore,
Dahdouh-Guebas, Daly, Daneliya, Dauvin, Davie, De Broyer, De Grave, de
Lima Ferreira, de Mazancourt, de Voogd, Decker, Defaye, Dekker, d’Hondt,
Dippenaar, Dohrmann, Dolan, Domning, Downey, Dreyer, Ector, Eisendle,
Eitel, Encarnaç{ã}o, Enghoff, Epler, Evenhuis, Ewers-Saucedo, Faber,
Figueroa, Fišer, Fordyce, Foster, Frank, Fransen, Freire, Furuya,
Galbany, Gale, Galea, Gao, Garcia-Alvarez, Garcia-Jacas, Garic, Garnett,
Gasca, Gaviria-Melo, Gerken, Gibson, Gibson, Gil, Gittenberger, Glasby,
Glenner, Glover, Gómez-Noguera, Gondim, González-Sol'is, Goodwin,
Gostel, Grabowski, Gravili, Grossi, Guerra-Garc'ia, Guerrero, Guidetti,
Guimarães, Guiry, Gutierrez, Hadfield, Hajdu, Hallermann, Hayward,
Hegna, Heiden, Hendrycks, Herbert, Herrera Bachiller, Ho, Hodda, Høeg,
Hoeksema, Holovachov, Houart, Hughes, Hyžn'y, Iniesta, Iseto, Ivanenko,
Iwataki, Janssen, Jaume, Jazdzewski, Jersabek, Jó{ź}wiak, Kabat, Kantor,
Karanovic, Karthick, Kathirithamby, Katinas, Kim, King, Kirk, Klautau,
Kociolek, Köhler, Kolb, Konowalik, Kotov, Kovács, Kremenetskaia,
Kristensen, Kulikovskiy, Kullander, Kupriyanova, Lamaro, Lambert,
Lazarus, Le Coze, Le Roux, LeCroy, Leduc, Lefkowitz, Lemaitre,
Lichter-Marck, Lim, Lindsay, Liu, Loeuille, Lörz, Ludwig, Lundholm,
Macpherson, Madin, Mah, Mamo, Mamos, Manconi, Mapstone, Marek, Marshall,
Marshall, Martin, Mast, McFadden, McInnes, McKenzie, Means, Meland,
Merrin, Miller, Mills, Moestrup, Mokievsky, Monniot, Mooi, Morandini,
Moreira da Rocha, Morrow, Mortelmans, Mortimer, Muñoz Gallego, Musco,
Nery, Nesom, Neubauer, Neubert, Neuhaus, Ng, Nguyen, Nielsen, Nishikawa,
Norenburg, O’Hara, Opresko, Osawa, Osigus, Ota, Páll-Gergely, Panero,
Pasini, Patterson, Paxton, Pelser, Peña Santiago, Perez-Losada,
Petrescu, Pfingstl, Pica, Picton, Pilger, Pisera, Polhemus, Poore,
Potapova, Pů{ž}a, Read, Reich, Reimer, Reip, Reuscher, Reynolds,
Richling, Rimet, Ríos, Rius, Rodríguez, Rogers, Roque, Rosenberg,
Rützler, Saavedra, Sabbe, Saiz-Salinas, Sala, Santagata, Santos, Sar,
Satoh, Saucède, Schierwater, Schilling, Schmidt-Lebuhn, Schmidt-Rhaesa,
Schneider, Schönberg, Schuchert, Senna, Sennikov, Serejo, Shaik, Shamsi,
Sharma, Shear, Shenkar, Short, Sicinski, Sierwald, Simmons, Sinniger,
Sinou, Sivell, Sket, Smit, Smit, Smol, Souza-Filho, Spelda, Sterrer,
Stienen, Stoev, Stöhr, Strand, Suárez-Morales, Susanna, Suttle, Swalla,
Taiti, Tanaka, Tandberg, Tang, Tasker, Taylor, Taylor, Tchesunov,
Temereva, ten Hove, ter Poorten, Thomas, Thuesen, Thurston, Thuy, Timi,
Timm, Todaro, Turon, Uetz, Urbatsch, Uribe-Palomino, Urtubey, Utevsky,
Vacelet, Vachard, Vader, Väinölä, Van de Vijver, van der Meij, van
Haaren, van Soest, Vanreusel, Venekey, Vinarski, Vonk, Vos, Vouilloud,
Walker-Smith, Walter, Watling, Wayland, Wesener, Wetzel, Whipps, White,
Wieneke, Williams, Williams, Wilson, Witkowski, Wyatt, Wylezich, Xu,
Zanol, Zeidler, Zhao, and Zullini (2021) \| \|8509eeb1 \|Nighttime
Ligths \|A new consistently processed time series of annual global VIIRS
nighttime lights has been produced from monthly cloud-free average
radiance grids spanning 2012\* to 2020. The new methodology is a
modification of the original method based on nightly data (Annual VNL
V1). Visite <https://eogdata.mines.edu/products/vnl/#annual_v2> for more
information. \|Elvidge, Zhizhin, Ghosh, Hsu, and Taneja (2021) \|
\|8449dee0 \|AIS global shipping data \|Monthly shipping rasters at 0.1
degree resolution including the number of vessels and total hours of
vessel presence for all vessels classified by Global Fishing Watch as
one of the following: ‘cargo’, ‘specialized\_reefer’, ‘tanker’,
‘bunker’, ‘cargo\_or\_tanker’, ‘cargo\_or\_reefer’,
‘bunker\_or\_tanker’, ‘container\_reefer’, ‘passenger’. There are two
versions of the data available, one based only on actual AIS positions
and one where vessel location is interpolated to a regular interval of
five minutes. \|Watch (2022) \|

## How to contribute

External contributors are welcome to contribute data pipelines to this
package. Simply fork the [public repo]() and create your own data
pipeline. The `pipenew()` function creates a `dp_#####.R` template for
you to use to create a new data pipeline with a unique id. Create a pull
request for us to review the data pipeline for inclusion in the package.

A single pull request per pipeline should be created, and merged pull
requests should be squashed into a single commit.
