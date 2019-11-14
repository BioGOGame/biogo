# Copyright © 2019, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2019 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.



# LOAD ALL REQUIRED LIBRARIES
library(shiny)
library(shinyWidgets)
library(shinyBS)
library(shinymaterial)
library(miniUI)
library(leaflet)
library(OpenImageR)
library(stringi)
library(exifr)
library(RSQLite)
library(DBI)
library(DT)
library(data.table)
library(formattable)
library(tidyverse)
library(plyr)
library(shinydashboard)
library(digest)
library(lubridate)
library(stringi)
library(sp)
library(cowplot)
source('src/lightbox.R')
source('src/photoswipe.R')


# LOAD NEEDED TABLE FROM SQLITE DATABASE AND PROCESS THEM
# INTO USABLE INFORMATION

# Get the tables from the database
all_tables  <- dbListTables(con)
con <- dbConnect(RSQLite::SQLite(), "www/database.sql")
all_quests  <- dbReadTable(con, "quests")
all_zones  <- dbReadTable(con, "zones")
all_zones_delim  <- dbReadTable(con, "zones_delim")
all_groups  <- dbReadTable(con, "groups")
all_users  <- dbReadTable(con, "users")
all_logs  <- dbReadTable(con, "log")
all_params  <- dbReadTable(con, "params")
all_bounties  <- dbReadTable(con, "bounties") %>% 
  filter(date != 'string')
planning <- dbReadTable(con, "planning") %>% 
  filter(date != "hello")

# Create a list with all the zones 
zone_list <- split(all_zones$id, all_zones$name)

# Create a list with all the tables structures 
# This is used when uplodaing data to a table, to check
# consistency in format
tables_str <- list(all_bounties = colnames(all_bounties),
                   all_groups = colnames(all_groups),
                   log = colnames(all_logs),
                   planning = colnames(planning),
                   all_quests = colnames(all_quests),
                   all_users = colnames(all_users),
                   all_zones = colnames(all_zones),
                   all_zones_delim = colnames(all_zones_delim)
                 )



# GAME PARAMETERS. CAN BE CHANGE IN THE INTERFACE
req_bounties <- all_params$value[all_params$param == "req_bounties"] # min number of bounties
req_types <- all_params$value[all_params$param == "req_types"] # min number of each bounties type
req_zones <- all_params$value[all_params$param == "req_zones"] # min number of zones to visit
req_out_zones <- all_params$value[all_params$param == "req_out_zones"] # min number of zones to visit
max_found_quest <- all_params$value[all_params$param == "max_found_quest"] # min number of zones to visit





# GET IMAGE FROM PREVIOUS YEAR
# This is used i the correction terface, to compare the current images
# With validated onces. 
all_corr <- read_csv("www/data_corr/all_corrections.csv") %>% 
  select(img, check) %>% 
  mutate(quest_id = str_split_fixed(img, "_", 3)[,1])


# OTHERS
# Color scale for dashboard plots
mycols <- c( "#CD534CFF", "#EFC000FF", "#0073C2FF", "#65ac54")


# Help texts for the interface
help_store <- "When you store a picture, it become available for your whole group. It can then be submitted as a bounty"

help_submit <- "When you submit a picture, it means you are formaly sending this picture for evaluation, adding it to your active bounty. This action cannot be undone"

help_bounty <- "Here you can see all your bounties, submitted or not, as well as their current evaluation status. You can also see here the ranking of all the groups. "