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

map_2018 <-  readr::read_csv ("dataset/map_2018.csv")

map_2017 <- readr::read_csv ("dataset/map_2017.csv")

map_2016 <-  readr::read_csv ("dataset/map_2016.csv")

map_2015 <-  readr::read_csv ("dataset/map_2015.csv")