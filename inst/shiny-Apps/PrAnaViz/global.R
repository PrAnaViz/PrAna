library(reshape)
library(reshape2)
library(stringi)
library(utils)
library(shiny)
library(plyr)
library(dplyr, warn.conflicts = FALSE)
library(DT)
library(data.table)
library(magrittr)
library(stringr)
library(tidyr)
library(plotly)
library(readxl)
library(lubridate)
library(ggplot2)
library(chron)
library(RMariaDB)
library(dbplyr)
library(tidyverse)
library(DBI)
library(leaflet)
library(rgdal)
library(sf)
library(shinyBS)
options(shiny.sanitize.errors = TRUE)


source(file.path("ui", "tab-targeted.R"),  local = TRUE)$value
source(file.path("ui", "tab-non-targeted.R"),  local = TRUE)$value

map_2018 <-  sf::st_read("https://opendata.arcgis.com/datasets/5252644ec26e4bffadf9d3661eef4826_2.geojson" ) %>%
  rename(region_name = 3)

map_2017 <- sf::st_read("https://opendata.arcgis.com/datasets/de108fe659f2430d9558c20fe172638f_0.geojson" ) %>%
  rename(region_name = 3)

map_2016 <-  sf::st_read("https://opendata.arcgis.com/datasets/1bc1e6a77cdd4b3a9a0458b64af1ade4_4.geojson" ) %>%
  rename(region_name = 3)

map_2015 <-  sf::st_read("https://opendata.arcgis.com/datasets/67993b98f52743899751f188c960f7df_2.geojson" ) %>%
  rename(region_name = 3)