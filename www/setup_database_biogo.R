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




# REQURIED LIBRARIES 
library(RSQLite)
library(DBI)
library(googlesheets)
library(tidyverse)
library(rjson)



#--------------------------------------------------
# SETUP DATABASE
con <- dbConnect(RSQLite::SQLite(), "www/database.sql")
dbListTables(con)

ti <- gs_title("biogo_setup")

#--------------------------------------------------
# QUEST TABLE
# Get the list of quests from google sheet

  quests <- gs_read(ti, ws = 1) 
  dbWriteTable(con, "quests", quests, overwrite = TRUE)

  
#--------------------------------------------------  
# ZONES TABLE
# Get the list of zones form the JSON delim files
  
  zones_raw <- fromJSON(file = "www/maps/map_small.json")[[2]]
  zones_delim <- NULL
  zones <-  gs_read(ti, ws = 2)
  
  for( i in c(1:length(zones_raw))){
    temp <- data.frame(matrix(unlist(zones_raw[[i]][[3]][2]), ncol=2, byrow=T))
    max_id <- 0
    if(!is.null(zones_delim)) max_id <- max(zones_delim$id)
    zones_delim <- rbind(zones_delim, 
                         data.frame(id = c((max_id + 1) : (max_id + nrow(temp))),
                                    group=i, 
                                    latitude=temp$X2, 
                                    longitude = temp$X1, 
                                    zone_id = zones$id[i]))
  }
  dbWriteTable(con, "zones", zones, overwrite = TRUE)
  dbWriteTable(con, "zones_delim", zones_delim, overwrite = TRUE)

  
  
#--------------------------------------------------
# GROUPS TABLE
# Get the list of groups from google sheet
  
  groups <- gs_read(ti, ws = 3) 
  dbWriteTable(con, "groups", groups, overwrite = TRUE)  
  
#--------------------------------------------------
# USER TABLE
# Get the list of users from google sheet
  
  users <- gs_read(ti, ws = 4) 
  dbWriteTable(con, "users", users, overwrite = TRUE)
  
#--------------------------------------------------
# BOUNTIES TABLE
# Get the list of bounties from google sheet
  
  bounties <- gs_read(ti, ws = 5) 
  dbWriteTable(con, "bounties", bounties, overwrite = TRUE)    
  
  
#--------------------------------------------------
# PLANNING TABLE
# Get the planning of the game
  
  planning <- gs_read(ti, ws = 6) 
  dbWriteTable(con, "planning", planning, overwrite = TRUE)    
  
  
  
  

#--------------------------------------------------
# LOG TABLE
# get user logging history
  
  log <- gs_read(ti, ws = 7) 
  dbWriteTable(con, "log", log, overwrite = TRUE)    
  
  
  
  #--------------------------------------------------
  # PARAM TABLE
  # get user logging history
  
  params <- gs_read(ti, ws = 8) 
  dbWriteTable(con, "params", params, overwrite = TRUE)    
  
  


dbListTables(con)
