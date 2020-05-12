# PrAna

`PrAna` aims to aggregate and normalise England’s national level prescription data, for all groups of drugs. The name is an acronym for _**Pr**escription **Ana**lysis_

During the last decade, wide range of **active pharmaceutical ingredients (APIs)** have been identified and quantified in aquatic environment across several studies and indicated their impacts on exposed environmental species and humans. For the prediction of total amount of the APIs released to the environment, information about APIs consumption data is vital. Globally, several methods were reported to estimate the APIs consumption data based on the national prescription data, manufacturers, importers and dispenser’s data.

In the UK, national prescription data provided by [National Health Service][NHS digital] was used to calculate the consumption data. This data is freely accessible and consist of individual files for each month. With the large file with over 10 million records every month, the data from the NHS cannot be used for the direct calculation of the prescription levels of different APIs. Re-organisation and processing of the files is required before to do any exploration or analysis and to speed up the data reading. 

The aim of `PrAna` is to aggregate and normalize prescription data to calculate total prescribed quantity of different APIs, using open source statistical software [R language][R]. 

Apart, from the calculation of the total prescribed quantity of an API or a group of APIs, specified to a postcode or region, We have also developed, _an open interactive web-based tool_, `PrAnaViz` with the processed dataset for the period `2015` to `2018`.

`PrAnaViz` facilitates users **to visualise, explore and report** different spatiotemporal and long-term prescription trends for wider use. 

## Installation

`PrAna` can be installed as any other R package, as follows,

Install the released version of PrAna from CRAN with:
```
install.packages("PrAna")
```
To install the development version of PrAna from GitHub with:
```
install.packages("devtools")
library(devtools)
install_github("jkkishore85/PrAna")
```
However, since it is dependent on some other software tools some extra steps are required for the installation. Please see the [installation section in the handbook][handbook-inst] for more information.


For a very quick start to `PrAnaViz`:
```
library(PrAna)
runShiny("PrAnaViz")
```
The `runShiny("PrAnaViz")` function will pop-up the `PrAnaViz` tool which will allow you to explore different spatiotemporal and long-term prescription trends with the sample dataset.

However, for a better guide to get started it is recommended to read the [tutorial].


[R]: https://www.r-project.org/
[tutorial]: https://github.bath.ac.uk/pages/kjj28/PrAna/articles/PrAnaViz_Tutoral.html
[NHS digital]: https://digital.nhs.uk/organisation-data-service/data-downloads/gp-data
[handbook-inst]: https://github.bath.ac.uk/pages/kjj28/PrAna/articles/handbook.html#installation




