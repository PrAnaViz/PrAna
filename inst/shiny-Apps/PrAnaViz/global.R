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
library(ggsci)
library(viridis)
library(paletteer)
library(RColorBrewer)
options(shiny.sanitize.errors = TRUE)
library(shinyWidgets)


source(file.path("ui", "tab-targeted.R"),  local = TRUE)$value
source(file.path("ui", "tab-non-targeted.R"),  local = TRUE)$value

map_2018 <-  readr::read_csv ("dataset/map_2018.csv")

map_2017 <- readr::read_csv ("dataset/map_2017.csv")

map_2016 <-  readr::read_csv ("dataset/map_2016.csv")

map_2015 <-  readr::read_csv ("dataset/map_2015.csv")


ggplot_dark_theme <-
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, colour = "snow"),
        axis.text.y = element_text(size = 12, colour = "snow"),
        axis.title.x = element_text(size = 15, face = "bold", colour = "snow" ),
        axis.title.y = element_text(size = 15, face = "bold", colour = "snow" ),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_line(color = "gray93", size = 0.4), # get rid of major grid
        panel.grid.minor = element_line(color = "gray93", size = 0.3), # get rid of minor grid
        strip.background = element_rect(color="snow",
                                        fill="transparent"),
        strip.text.x = element_text(size = 15, color = "snow"),
        strip.text.y = element_text(size = 15, color = "snow"),
        legend.title= element_text(size = 12, colour = "snow"),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent") ,# get rid of legend panel bg
        legend.text = element_text(size = 12, colour = "snow") ,
        title = element_text(size = 12, color = "snow")
  )

ggplot_light_theme <-
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 15, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_line(color = "gray93", size = 0.4), # get rid of major grid
        panel.grid.minor = element_line(color = "gray93", size = 0.3), # get rid of minor grid
        strip.background = element_rect(colour="black",
                                        fill="transparent"),
        strip.text.x = element_text(size = 15, color = "black"),
        strip.text.y = element_text(size = 15, color = "black"),
        legend.title= element_text(size = 12, colour = "black"),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent") ,# get rid of legend panel bg
        legend.text = element_text(size = 12, colour = "black"),
        title = element_text(size = 12, color = "black")
  )

Blue2DarkOrange18Steps = c("#006666", "#009999", "#00CCCC", "#00FFFF", 
                           "#33FFFF", "#65FFFF", "#99FFFF", "#B2FFFF", 
                           "#CBFFFF", "#E5FFFF", "#FFE5CB", "#FFCA99", 
                           "#FFAD65", "#FF8E33", "#FF6E00", "#CC5500", 
                           "#993D00", "#662700")


PairedColor12Steps = c("#FFBF7F", "#FF7F00", "#FFFF99", "#FFFF32", 
                       "#B2FF8C", "#32FF00", "#A5EDFF", "#19B2FF", 
                       "#CCBFFF", "#654CFF", "#FF99BF", "#E51932")

#SteppedSequential5Steps
SteppedSequential5Steps = c("#990F0F", "#B22C2C", "#CC5151", "#E57E7E", 
                            "#FFB2B2", "#99540F", "#B26F2C", "#CC8E51", 
                            "#E5B17E", "#FFD8B2", "#6B990F", "#85B22C",
                            "#A3CC51", "#C3E57E", "#E5FFB2", "#0F6B99", 
                            "#2C85B2", "#51A3CC", "#7EC3E5", "#B2E5FF",
                            "#260F99", "#422CB2", "#6551CC", "#8F7EE5", 
                            "#BFB2FF")


