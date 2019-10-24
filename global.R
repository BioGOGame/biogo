# Copyright © 2019, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2019 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
#   
#   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# 
# Disclaimer
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# 
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.




library(shiny)
library(shinyWidgets)
# library(shinyalert)
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

source('src/lightbox.R')
source('src/photoswipe.R')


# REQUIREMENTS
req_animal <- 5
req_vegetal <- 5
req_zones <- 2
req_bounties <- 20

# library(semantic.dashboard)
theme_col <- "black"

help_store <- "When you store a picture, it become available for your whole group. It can then be submitted as a bounty for the current week"

help_submit <- "When you submit a picture, it means you are formaly sending this picture for evaluation, adding it to your active bounty, for the week in progress. This action cannot be undone"


# Get the tables from the database
con <- dbConnect(RSQLite::SQLite(), "www/database.sql")
all_quests  <- dbReadTable(con, "quests")
all_zones  <- dbReadTable(con, "zones")
all_zones_delim  <- dbReadTable(con, "zones_delim")
all_groups  <- dbReadTable(con, "groups")
all_users  <- dbReadTable(con, "users")
all_bounties  <- dbReadTable(con, "bounties") %>% 
  filter(date != 'string')
planning <- dbReadTable(con, "planning") %>% 
  filter(date != "hello")   #%>% 
  # mutate(date = as.Date(date))

zone_list <- split(all_zones$id, all_zones$name)

tables_str <- list(all_bounties = colnames(all_bounties),
                   all_groups = colnames(all_groups),
                   planning = colnames(planning),
                   all_quests = colnames(all_quests),
                   all_users = colnames(all_users),
                   all_zones = colnames(all_zones),
                   all_zones_delim = colnames(all_zones_delim)
                 )

all_tables  <- dbListTables(con)



current_week <- planning$week[planning$date == Sys.Date()]


# GET IMAGE FROM PREVIOUS YEAR

all_corr <- read_csv("www/data_corr/all_corrections.csv") %>% 
  select(img, check) %>% 
  mutate(quest_id = str_split_fixed(img, "_", 3)[,1])




